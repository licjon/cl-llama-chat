(defpackage #:tui-chat/tests/commands
  (:use #:cl #:rove))
(in-package #:tui-chat/tests/commands)

(deftest plain-text
  (ok (equal (tui-chat::parse-command "hello there") '(:say "hello there")))
  (ok (equal (tui-chat::parse-command "  spaced  ") '(:say "spaced"))))

(deftest blank
  (ok (equal (tui-chat::parse-command "   ") '(:empty))))

(deftest simple-commands
  (ok (equal (tui-chat::parse-command "/help") '(:help)))
  (ok (equal (tui-chat::parse-command "/quit") '(:quit)))
  (ok (equal (tui-chat::parse-command "/exit") '(:quit)))
  (ok (equal (tui-chat::parse-command "/reset") '(:reset)))
  (ok (equal (tui-chat::parse-command "/regen") '(:regen)))
  (ok (equal (tui-chat::parse-command "/presets") '(:presets))))

(deftest sampler
  (ok (equal (tui-chat::parse-command "/sampler creative") '(:set-sampler "creative")))
  (ok (eq (car (tui-chat::parse-command "/sampler")) :error)))

(deftest branch
  (ok (equal (tui-chat::parse-command "/branch") '(:branch nil nil)))
  (ok (equal (tui-chat::parse-command "/branch balanced") '(:branch "balanced" nil)))
  (ok (equal (tui-chat::parse-command "/branch balanced creative")
             '(:branch "balanced" "creative"))))

(deftest unknown
  (ok (equal (tui-chat::parse-command "/wat now") '(:unknown "wat"))))
