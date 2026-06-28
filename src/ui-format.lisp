(in-package #:cl-llama-chat)

(defparameter +reset+  (format nil "~c[0m" #\Escape))
(defparameter +cyan+   (format nil "~c[36m" #\Escape))
(defparameter +green+  (format nil "~c[32m" #\Escape))
(defparameter +dim+    (format nil "~c[2m" #\Escape))
(defparameter +yellow+ (format nil "~c[33m" #\Escape))

(defun colorize (text code) (concatenate 'string code text +reset+))

(defun wrap-text (text width)
  "Greedy word-wrap TEXT to WIDTH columns. Long words are hard-split."
  (when (<= width 0) (setf width 1))
  (let ((lines '()) (line ""))
    (flet ((flush () (push line lines) (setf line "")))
      (dolist (word (%split-words text))
        ;; hard-split words longer than width
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

(defun %pad (s width)
  (if (>= (length s) width) (subseq s 0 width)
      (concatenate 'string s (make-string (- width (length s))
                                          :initial-element #\Space))))

(defun format-two-columns (left-text right-text &key (width 80)
                                                      (header-left "A")
                                                      (header-right "B"))
  (let* ((col (max 1 (floor (- width 3) 2)))
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
