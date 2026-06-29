(in-package #:cl-llama-chat)

(defparameter *prompt* "you> ")

(defun print-help (out)
  (format out "~&Commands:~%")
  (format out "  /branch [A] [B]  compare two replies to your next message~%")
  (format out "  /regen           regenerate the last reply~%")
  (format out "  /sampler NAME    set the default sampler preset~%")
  (format out "  /presets         list sampler presets~%")
  (format out "  /bench           benchmark speculative decoding~%")
  (format out "  /stats           show speculative decoding stats~%")
  (format out "  /reset           clear the conversation~%")
  (format out "  /help            this help~%")
  (format out "  /quit            exit~%"))

(defun print-presets (engine out)
  (let ((cfg (engine-config engine)))
    (format out "~&Presets (default: ~a):~%" (engine-default-sampler engine))
    (dolist (p (config-presets cfg))
      (format out "  ~a~a~%"
              (%pad (car p) 10) (colorize (format nil "~s" (cdr p)) +dim+)))))

(defun %stream-assistant (out label-color label)
  "Return an on-token closure that prints LABEL once then streams tokens."
  (let ((started nil))
    (lambda (tok)
      (unless started
        (format out "~&~a " (colorize label label-color))
        (setf started t))
      (write-string tok out)
      (force-output out))))

(defparameter *branch-width* 100)

(defun %do-branch (engine text a b out)
  "Generate A/B candidates, showing each column as it completes.
Column A is displayed immediately after generation; column B fills in beside it
using ANSI cursor movement."
  (let ((left-lines 0))
    (let* ((cands
             (engine-branch
              engine text a b
              :on-token
              (let ((a-toks (make-array 0 :element-type 'character
                                          :adjustable t :fill-pointer 0))
                    (left-printed nil))
                (lambda (which tok)
                  (case which
                    (:a (loop for c across tok
                              do (vector-push-extend c a-toks)))
                    (:b (unless left-printed
                          (setf left-printed t)
                          (multiple-value-bind (text lines)
                              (format-left-column
                               (copy-seq a-toks)
                               :width *branch-width*
                               :header-left (%branch-label
                                             :a (or a
                                                    (engine-default-sampler engine)))
                               :header-right (%branch-label
                                              :b (or b
                                                     (engine-default-sampler engine))))
                            (setf left-lines lines)
                            (terpri out)
                            (write-string text out)
                            (force-output out)))))))))
           (ca (first cands)) (cb (second cands)))
      ;; Fill in right column using cursor movement
      (if (plusp left-lines)
          (progn
            (write-string (fill-right-column
                           (getf cb :text) left-lines
                           :width *branch-width*)
                          out)
            (force-output out))
          ;; Fallback if left column was never printed (shouldn't happen)
          (progn
            (terpri out)
            (write-string (format-two-columns
                           (getf ca :text) (getf cb :text)
                           :width *branch-width*
                           :header-left (getf ca :label)
                           :header-right (getf cb :label))
                          out)))
      (format out "~&Pick [a]/[b], [r]egenerate, [d]iscard: ")
      (force-output out)
      (let ((choice (string-downcase
                     (%trim (or (read-line *standard-input* nil "d") "d")))))
        (cond
          ((string= choice "a") (engine-commit engine text (getf ca :text))
                                (format out "~&~a~%" (colorize "committed A" +dim+)))
          ((string= choice "b") (engine-commit engine text (getf cb :text))
                                (format out "~&~a~%" (colorize "committed B" +dim+)))
          ((string= choice "r") (%do-branch engine text a b out))
          (t (format out "~&~a~%" (colorize "discarded" +dim+))))))))

(defun %do-stats (engine out)
  (let ((n-draft (engine-spec-n-draft engine))
        (n-accepted (engine-spec-n-accepted engine)))
    (if (and (zerop n-draft) (null (engine-speculative-fns engine)))
        (format out "~&~a~%"
                (colorize "Speculative decoding not configured." +yellow+))
        (let ((accept-pct (if (plusp n-draft)
                              (* 100.0d0 (/ n-accepted n-draft))
                              0.0d0)))
          (format out "~&Speculative decoding (session):~%")
          (format out "  drafts: ~d  accepted: ~d  accept%%: ~,1f%%~%"
                  n-draft n-accepted accept-pct)))))

(defstruct spec-stats
  (n-draft 0 :type fixnum)
  (n-accepted 0 :type fixnum))

(defun %wrap-spec-fns (fns stats)
  "Wrap draft-fn and accept-fn to accumulate stats."
  (let ((orig-draft  (getf fns :draft-fn))
        (orig-accept (getf fns :accept-fn)))
    (list :begin-fn (getf fns :begin-fn)
          :draft-fn (lambda (&rest args)
                      (let ((drafts (apply orig-draft args)))
                        (incf (spec-stats-n-draft stats) (length drafts))
                        drafts))
          :accept-fn (lambda (seq-id n-accepted)
                       (incf (spec-stats-n-accepted stats) n-accepted)
                       (funcall orig-accept seq-id n-accepted)))))

(defun %bench-generate (engine prompt-tokens max-tokens spec-fns)
  "Run one benchmark pass. Returns (values n-tokens elapsed-seconds).
N-TOKENS is the count of generated tokens (from the third return value of generate)."
  (let* ((cfg (engine-config engine))
         (model (engine-model engine))
         (preset (engine-sampler-plist engine nil))
         (scratch-n-ctx (min (config-n-ctx cfg)
                             (+ (length prompt-tokens) max-tokens 64)))
         (start (get-internal-real-time))
         (n-tokens 0))
    (llama:with-context (scratch model :n-ctx scratch-n-ctx :n-batch scratch-n-ctx)
      (multiple-value-bind (text stop result-tokens)
          (llama:generate scratch nil
                          :prompt-tokens prompt-tokens
                          :sampler-config (%sampler-config preset)
                          :max-tokens max-tokens
                          :seed 42
                          :speculative-fns spec-fns)
        (declare (ignore text stop))
        (setf n-tokens (length result-tokens))))
    (let ((elapsed (/ (- (get-internal-real-time) start)
                      (float internal-time-units-per-second 1.0d0))))
      (values n-tokens elapsed))))

(defun %do-bench (engine out)
  "Run speculative decoding benchmark and print results."
  (let* ((session (engine-session engine))
         (model (engine-model engine))
         (messages (append (llama:chat-session-messages session)
                           (list (list :role "user"
                                       :content "Write a short paragraph about the history of computing."))))
         (prompt (llama:tokenize-chat model messages :add-assistant-prefix t))
         (max-tokens (config-max-tokens (engine-config engine)))
         (spec-fns (engine-speculative-fns engine)))
    (format out "~&~a~%" (colorize "Running benchmark..." +dim+))
    (force-output out)
    ;; Baseline (no speculation)
    (multiple-value-bind (base-toks base-time)
        (%bench-generate engine prompt max-tokens nil)
      ;; Speculative
      (if (null spec-fns)
          (format out "~&~a~%"
                  (colorize "Speculative decoding not configured. Set :speculative :ngram in config." +yellow+))
          (let ((stats (make-spec-stats)))
            (multiple-value-bind (spec-toks spec-time)
                (%bench-generate engine prompt max-tokens
                                 (%wrap-spec-fns spec-fns stats))
              (let ((base-tps (if (plusp base-time)
                                  (/ base-toks base-time) 0.0d0))
                    (spec-tps (if (plusp spec-time)
                                  (/ spec-toks spec-time) 0.0d0))
                    (accept-pct (if (plusp (spec-stats-n-draft stats))
                                    (* 100.0d0 (/ (spec-stats-n-accepted stats)
                                                  (spec-stats-n-draft stats)))
                                    0.0d0)))
                (format out "~&~14a ~6a ~8a ~7a ~6a ~8a~%"
                        "" "Tokens" "Time(s)" "Tok/s" "Drafts" "Accept%")
                (format out "~14a ~6d ~8,2f ~7,1f ~6a ~8a~%"
                        "  baseline:" base-toks base-time base-tps "—" "—")
                (format out "~14a ~6d ~8,2f ~7,1f ~6d ~7,1f%~%"
                        "  speculative:" spec-toks spec-time spec-tps
                        (spec-stats-n-draft stats) accept-pct)
                (if (plusp base-tps)
                    (format out "  speedup: ~,2fx~%" (/ spec-tps base-tps))
                    (format out "  speedup: N/A~%")))))))))

(defun run-ui (engine &key (in *standard-input*) (out *standard-output*))
  (let ((*standard-input* in))
    (format out "~&cl-llama-chat — /help for commands.~%")
    (loop
      (format out "~&~a" (colorize *prompt* +cyan+)) (force-output out)
      (let ((line (read-line in nil :eof)))
        (when (eq line :eof) (return))
        (destructuring-bind (kind &rest args) (parse-command line)
          (case kind
            (:empty)
            (:quit (return))
            (:help (print-help out))
            (:presets (print-presets engine out))
            (:bench
             (handler-case (%do-bench engine out)
               (error (e) (format out "~&~a~%" (colorize (princ-to-string e) +yellow+)))))
            (:stats (%do-stats engine out))
            (:reset (engine-reset engine)
                    (format out "~&~a~%" (colorize "conversation cleared" +dim+)))
            (:set-sampler
             (handler-case
                 (progn (engine-set-default-sampler engine (first args))
                        (format out "~&default sampler: ~a~%" (first args)))
               (error (e) (format out "~&~a~%" (colorize (princ-to-string e) +yellow+)))))
            (:error (format out "~&~a~%" (colorize (first args) +yellow+)))
            (:unknown (format out "~&unknown command: /~a (try /help)~%" (first args)))
            (:regen
             (handler-case
                 (let ((r (engine-regenerate
                           engine :on-token (%stream-assistant out +green+ "ai>"))))
                   (if r (terpri out)
                       (format out "~&~a~%"
                               (colorize "nothing to regenerate yet" +dim+))))
               (error (e) (format out "~&~a~%" (colorize (princ-to-string e) +yellow+)))))
            (:branch
             (format out "~&message to compare> ") (force-output out)
             (let ((msg (read-line in nil "")))
               (if (zerop (length (%trim msg)))
                   (format out "~&~a~%" (colorize "branch cancelled" +dim+))
                   (handler-case (%do-branch engine (%trim msg) (first args) (second args) out)
                     (error (e) (format out "~&~a~%" (colorize (princ-to-string e) +yellow+)))))))
            (:say
             (handler-case
                 (progn
                   (engine-send engine (first args)
                                :on-token (%stream-assistant out +green+ "ai>"))
                   (terpri out))
               (error (e) (format out "~&~a~%" (colorize (princ-to-string e) +yellow+))))))))))
  nil)
