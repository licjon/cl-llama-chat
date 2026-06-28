(defpackage #:cl-llama-chat/tests/commands
  (:use #:cl #:rove))
(in-package #:cl-llama-chat/tests/commands)

(deftest plain-text
  (ok (equal (cl-llama-chat::parse-command "hello there") '(:say "hello there")))
  (ok (equal (cl-llama-chat::parse-command "  spaced  ") '(:say "spaced"))))

(deftest blank
  (ok (equal (cl-llama-chat::parse-command "   ") '(:empty))))

(deftest simple-commands
  (ok (equal (cl-llama-chat::parse-command "/help") '(:help)))
  (ok (equal (cl-llama-chat::parse-command "/quit") '(:quit)))
  (ok (equal (cl-llama-chat::parse-command "/exit") '(:quit)))
  (ok (equal (cl-llama-chat::parse-command "/reset") '(:reset)))
  (ok (equal (cl-llama-chat::parse-command "/regen") '(:regen)))
  (ok (equal (cl-llama-chat::parse-command "/presets") '(:presets))))

(deftest sampler
  (ok (equal (cl-llama-chat::parse-command "/sampler creative") '(:set-sampler "creative")))
  (ok (eq (car (cl-llama-chat::parse-command "/sampler")) :error)))

(deftest branch
  (ok (equal (cl-llama-chat::parse-command "/branch") '(:branch nil nil)))
  (ok (equal (cl-llama-chat::parse-command "/branch balanced") '(:branch "balanced" nil)))
  (ok (equal (cl-llama-chat::parse-command "/branch balanced creative")
             '(:branch "balanced" "creative"))))

(deftest unknown
  (ok (equal (cl-llama-chat::parse-command "/wat now") '(:unknown "wat"))))
