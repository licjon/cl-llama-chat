(defpackage #:tui-chat/tests/engine-smoke
  (:use #:cl #:rove)
  (:local-nicknames (#:llama #:cl-llama-cpp)))
(in-package #:tui-chat/tests/engine-smoke)

(defun model-available-p ()
  (probe-file (tui-chat::config-model-path (tui-chat::default-config))))

(deftest sampler-resolution-no-model
  ;; pure helper: does not need a model
  (let* ((cfg (tui-chat::default-config))
         (eng (tui-chat::make-engine* :config cfg :default-sampler "balanced")))
    (ok (equal (getf (tui-chat::engine-sampler-plist eng nil) :temp) 0.7))
    (ok (equal (getf (tui-chat::engine-sampler-plist eng "precise") :temp) 0.2))))

(deftest branch-smoke
  (if (not (model-available-p))
      (skip "model file not present; skipping llama smoke test")
      (let ((cfg (tui-chat::parse-config '(:n-ctx 1024 :max-tokens 24))))
        (llama:with-model (model (namestring (tui-chat::config-model-path cfg))
                                 :n-gpu-layers 99)
          (llama:with-context (ctx model :n-ctx 1024)
            (let ((eng (tui-chat::make-engine cfg model ctx)))
              ;; normal turn
              (let ((r (tui-chat::engine-send eng "Say hi in one word.")))
                (ok (stringp r)) (ok (plusp (length r))))
              ;; branch: two candidates, main session unchanged in length
              (let* ((before (length (llama:chat-session-messages
                                      (tui-chat::engine-session eng))))
                     (cands (tui-chat::engine-branch eng "Name a color."
                                                     "precise" "creative"))
                     (after (length (llama:chat-session-messages
                                     (tui-chat::engine-session eng)))))
                (ok (= (length cands) 2))
                (ok (= before after))            ; branch did not mutate session
                (ok (stringp (getf (first cands) :text)))
                ;; commit one, session grows by 2 (user + assistant)
                (tui-chat::engine-commit eng "Name a color."
                                         (getf (first cands) :text))
                (ok (= (length (llama:chat-session-messages
                                (tui-chat::engine-session eng)))
                       (+ after 2))))))))))
