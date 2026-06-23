(defpackage #:tui-chat/tests/ui-format
  (:use #:cl #:rove))
(in-package #:tui-chat/tests/ui-format)

(deftest colorize-wraps
  (let ((s (tui-chat::colorize "hi" tui-chat::+cyan+)))
    (ok (search "hi" s))
    (ok (search tui-chat::+reset+ s))))

(deftest wrap-basic
  (let ((lines (tui-chat::wrap-text "the quick brown fox" 9)))
    (ok (every (lambda (l) (<= (length l) 9)) lines))
    (ok (string= (format nil "~{~a~^ ~}" lines) "the quick brown fox"))))

(deftest wrap-hard-split-long-word
  (let ((lines (tui-chat::wrap-text "abcdefghij" 4)))
    (ok (every (lambda (l) (<= (length l) 4)) lines))
    (ok (string= (apply #'concatenate 'string lines) "abcdefghij"))))

(deftest two-columns-has-both
  (let ((s (tui-chat::format-two-columns "left side" "right side"
            :width 40 :header-left "A: x" :header-right "B: y")))
    (ok (search "A: x" s))
    (ok (search "B: y" s))
    (ok (search "│" s))))
