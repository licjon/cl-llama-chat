(in-package #:cl-llama-chat)

(defparameter +reset+  (format nil "~c[0m" #\Escape))
(defparameter +cyan+   (format nil "~c[36m" #\Escape))
(defparameter +green+  (format nil "~c[32m" #\Escape))
(defparameter +dim+    (format nil "~c[2m" #\Escape))
(defparameter +yellow+ (format nil "~c[33m" #\Escape))

(defun colorize (text code) (concatenate 'string code text +reset+))

(defun %split-lines (s)
  "Split S on newline characters, preserving blank lines."
  (let ((lines '()) (start 0) (n (length s)))
    (dotimes (i n)
      (when (char= (char s i) #\Newline)
        (push (subseq s start i) lines)
        (setf start (1+ i))))
    (push (subseq s start) lines)
    (nreverse lines)))

(defun %wrap-line (text width)
  "Word-wrap a single line (no embedded newlines) to WIDTH columns."
  (let ((lines '()) (line ""))
    (flet ((flush () (push line lines) (setf line "")))
      (dolist (word (%split-words text))
        (loop while (> (length word) width) do
          (when (plusp (length line)) (flush))
          (push (subseq word 0 width) lines)
          (setf word (subseq word width)))
        (cond ((string= line "") (setf line word))
              ((<= (+ (length line) 1 (length word)) width)
               (setf line (concatenate 'string line " " word)))
              (t (flush) (setf line word))))
      (when (plusp (length line)) (flush)))
    (or (nreverse lines) (list ""))))

(defun wrap-text (text width)
  "Word-wrap TEXT to WIDTH columns, preserving paragraph breaks."
  (when (<= width 0) (setf width 1))
  (let ((result '()))
    (dolist (paragraph (%split-lines text))
      (dolist (line (%wrap-line paragraph width))
        (push line result)))
    (or (nreverse result) (list ""))))

(defun %pad (s width)
  (if (>= (length s) width) (subseq s 0 width)
      (concatenate 'string s (make-string (- width (length s))
                                          :initial-element #\Space))))

(defun %col-width (total-width)
  (max 1 (floor (- total-width 3) 2)))

(defun format-two-columns (left-text right-text &key (width 80)
                                                      (header-left "A")
                                                      (header-right "B"))
  (let* ((col (%col-width width))
         (sep " │ ")
         (l (wrap-text left-text col))
         (r (wrap-text right-text col))
         (n (max (length l) (length r)))
         (out (make-string-output-stream)))
    (format out "~a~a~a~%" (%pad header-left col) sep (%pad header-right col))
    (format out "~a~a~a~%" (make-string col :initial-element #\─) sep
            (make-string col :initial-element #\─))
    (dotimes (i n)
      (format out "~a~a~a~%"
              (%pad (or (nth i l) "") col) sep (%pad (or (nth i r) "") col)))
    (get-output-stream-string out)))

(defun format-left-column (left-text &key (width 80) (header-left "A")
                                          (header-right "B"))
  "Print the left column with separator and blank right side.
Returns (values output-string line-count column-width)."
  (let* ((col (%col-width width))
         (sep (colorize " │ " +dim+))
         (lines (wrap-text left-text col))
         (out (make-string-output-stream))
         (blank (make-string col :initial-element #\Space))
         (line-count 0))
    (format out "~a~a~a~%"
            (colorize (%pad header-left col) +yellow+)
            sep
            (colorize (%pad header-right col) +yellow+))
    (incf line-count)
    (format out "~a~a~a~%"
            (colorize (make-string col :initial-element #\─) +dim+)
            sep
            (colorize (make-string col :initial-element #\─) +dim+))
    (incf line-count)
    (dolist (l lines)
      (format out "~a~a~a~%" (%pad l col) sep blank)
      (incf line-count))
    (values (get-output-stream-string out) line-count col)))

(defun fill-right-column (right-text line-count &key (width 80))
  "Generate ANSI escape sequences to cursor-up LINE-COUNT rows and fill in the
right column text beside an already-printed left column.
The first two rows (header + separator) are skipped — they were already complete.
If the right text wraps to more lines than the left, extra full-width rows are
appended at the bottom."
  (let* ((col (%col-width width))
         (sep (colorize " │ " +dim+))
         (right-col-start (+ col 4))
         (lines (wrap-text right-text col))
         (data-rows (- line-count 2))
         (blank-left (make-string col :initial-element #\Space))
         (out (make-string-output-stream)))
    ;; Move cursor up to the first data row (skip header + separator)
    (format out "~c[~dA" #\Escape line-count)
    (format out "~%~%")
    ;; Overwrite existing left-column rows with right-column text
    (dotimes (i data-rows)
      (format out "~c[~dG~a~c[0K~%"
              #\Escape right-col-start
              (%pad (or (nth i lines) "") col)
              #\Escape))
    ;; If right column is longer, append new rows with blank left side
    (loop for i from data-rows below (length lines) do
      (format out "~a~a~a~%"
              blank-left sep (%pad (nth i lines) col)))
    (get-output-stream-string out)))
