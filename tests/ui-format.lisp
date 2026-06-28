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

(deftest wrap-preserves-paragraphs
  (let ((lines (cl-llama-chat::wrap-text (format nil "hello world~%~%second paragraph") 40)))
    ;; Should produce: "hello world", "", "second paragraph"
    (ok (= (length lines) 3))
    (ok (string= (first lines) "hello world"))
    (ok (string= (second lines) ""))
    (ok (string= (third lines) "second paragraph"))))

(deftest wrap-preserves-single-newline
  (let ((lines (cl-llama-chat::wrap-text (format nil "line one~%line two") 40)))
    (ok (= (length lines) 2))
    (ok (string= (first lines) "line one"))
    (ok (string= (second lines) "line two"))))

(deftest two-columns-has-both
  (let ((s (cl-llama-chat::format-two-columns "left side" "right side"
            :width 40 :header-left "A: x" :header-right "B: y")))
    (ok (search "A: x" s))
    (ok (search "B: y" s))
    (ok (search "│" s))))

(deftest left-column-returns-line-count
  (multiple-value-bind (text lines col)
      (cl-llama-chat::format-left-column "hello world"
       :width 40 :header-left "A" :header-right "B")
    (ok (search "A" text))
    (ok (search "B" text))
    (ok (search "│" text))
    (ok (search "hello world" text))
    ;; 2 header rows + 1 data row
    (ok (= lines 3))
    (ok (plusp col))))

(deftest left-column-wraps-count
  (multiple-value-bind (text lines)
      (cl-llama-chat::format-left-column "the quick brown fox jumps"
       :width 20 :header-left "A" :header-right "B")
    (declare (ignore text))
    ;; col width = floor((20-3)/2) = 8, wrapping "the quick brown fox jumps" at 8
    ;; => "the", "quick", "brown", "fox", "jumps" — each word fits on its own line
    ;; 2 header + 5 data = 7
    (ok (= lines 7))))

(deftest fill-right-column-contains-text
  (let ((s (cl-llama-chat::fill-right-column "hello" 5 :width 40)))
    (ok (search "hello" s))))

(deftest fill-right-column-has-cursor-up
  (let ((s (cl-llama-chat::fill-right-column "hi" 4 :width 40)))
    ;; Should contain ESC[4A (cursor up 4)
    (ok (search (format nil "~c[4A" #\Escape) s))))

(deftest fill-right-column-extends-when-longer
  (let* ((col (cl-llama-chat::%col-width 40))
         (long-text (format nil "~{~a~^ ~}"
                            (loop for i below 10 collect (format nil "word~d" i))))
         (s (cl-llama-chat::fill-right-column long-text 4 :width 40)))
    (declare (ignore col))
    ;; When right text wraps to more lines than left (4 - 2 = 2 data rows),
    ;; the overflow lines should contain the separator
    (ok (search "│" s))))
