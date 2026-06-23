(in-package #:tui-chat)

(defparameter *prompt* "you> ")

(defun print-help (out)
  (format out "~&Commands:~%")
  (format out "  /branch [A] [B]  compare two replies to your next message~%")
  (format out "  /regen           regenerate the last reply~%")
  (format out "  /sampler NAME    set the default sampler preset~%")
  (format out "  /presets         list sampler presets~%")
  (format out "  /reset           clear the conversation~%")
  (format out "  /help            this help    /quit  exit~%"))

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

(defun %do-branch (engine text a b out)
  "Generate, stream, then show A/B columns and commit the user's pick."
  (let* ((cands
           (engine-branch
            engine text a b
            :on-token
            (let ((cb-a nil) (cb-b nil))
              (lambda (which tok)
                (case which
                  (:a (unless cb-a
                        (format out "~&~a~%"
                                (colorize "── candidate A ──" +yellow+))
                        (setf cb-a t))
                      (write-string tok out) (force-output out))
                  (:b (unless cb-b
                        (format out "~&~%~a~%"
                                (colorize "── candidate B ──" +yellow+))
                        (setf cb-b t))
                      (write-string tok out) (force-output out)))))))
         (ca (first cands)) (cb (second cands)))
    (terpri out)
    (write-string (format-two-columns
                   (getf ca :text) (getf cb :text)
                   :width 100
                   :header-left (getf ca :label)
                   :header-right (getf cb :label))
                  out)
    (format out "~&Pick [a]/[b], [r]egenerate, [d]iscard: ")
    (force-output out)
    (let ((choice (string-downcase (%trim (or (read-line *standard-input* nil "d") "d")))))
      (cond
        ((string= choice "a") (engine-commit engine text (getf ca :text))
                              (format out "~&~a~%" (colorize "committed A" +dim+)))
        ((string= choice "b") (engine-commit engine text (getf cb :text))
                              (format out "~&~a~%" (colorize "committed B" +dim+)))
        ((string= choice "r") (%do-branch engine text a b out))
        (t (format out "~&~a~%" (colorize "discarded" +dim+)))))))

(defun run-ui (engine &key (in *standard-input*) (out *standard-output*))
  (let ((*standard-input* in))
    (format out "~&tui-chat — /help for commands.~%")
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
