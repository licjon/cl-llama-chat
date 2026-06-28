(defpackage #:cl-llama-chat/tests/ui-format
  (:use #:cl #:rove))
(in-package #:cl-llama-chat/tests/ui-format)

(deftest colorize-wraps
  (let ((s (cl-llama-chat::colorize "hi" cl-llama-chat::+cyan+)))
    (ok (search "hi" s))
    (ok (search cl-llama-chat::+reset+ s))))

(deftest wrap-basic
  (let ((lines (cl-llama-chat::wrap-text "the quick brown fox" 9)))
    (ok (every (lambda (l) (<= (length l) 9)) lines))
    (ok (string= (format nil "~{~a~^ ~}" lines) "the quick brown fox"))))

(deftest wrap-hard-split-long-word
  (let ((lines (cl-llama-chat::wrap-text "abcdefghij" 4)))
    (ok (every (lambda (l) (<= (length l) 4)) lines))
    (ok (string= (apply #'concatenate 'string lines) "abcdefghij"))))

(deftest two-columns-has-both
  (let ((s (cl-llama-chat::format-two-columns "left side" "right side"
            :width 40 :header-left "A: x" :header-right "B: y")))
    (ok (search "A: x" s))
    (ok (search "B: y" s))
    (ok (search "│" s))))
