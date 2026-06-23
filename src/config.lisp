(in-package #:tui-chat)

(defstruct config
  (model-path (merge-pathnames "models/qwen2.5-14b-instruct-q4_k_m.gguf"
                               (user-homedir-pathname)))
  ;; When AUTO-RESOURCES is true, N-CTX and N-GPU-LAYERS are treated as ceilings:
  ;; cl-llama-cpp's resource planner picks values that fit free VRAM. When false,
  ;; they are used verbatim.
  (auto-resources t)
  (n-ctx 8192)
  (n-gpu-layers 99)
  (system-prompt "You are a helpful assistant.")
  (max-tokens 256)
  (presets '(("balanced" :temp 0.7)
             ("creative" :temp 1.4 :top-p 0.95 :min-p 0.05)
             ("wild"     :temp 1.9 :top-p 0.98 :min-p 0.02)
             ("precise"  :temp 0.2)))
  (default-sampler "balanced"))

(defun default-config () (make-config))

(defun config-file-path ()
  (let ((base (or (uiop:getenv "XDG_CONFIG_HOME")
                  (merge-pathnames ".config/" (user-homedir-pathname)))))
    (merge-pathnames "tui-chat/config.lisp" (uiop:ensure-directory-pathname base))))

(defun parse-config (plist)
  "Build a CONFIG by overlaying PLIST onto the defaults."
  (let ((c (make-config)))
    (loop for (k v) on plist by #'cddr do
      (ecase k
        (:model-path      (setf (config-model-path c) (pathname v)))
        (:auto-resources  (setf (config-auto-resources c) v))
        (:n-ctx           (setf (config-n-ctx c) v))
        (:n-gpu-layers    (setf (config-n-gpu-layers c) v))
        (:system-prompt   (setf (config-system-prompt c) v))
        (:max-tokens      (setf (config-max-tokens c) v))
        (:presets         (setf (config-presets c) v))
        (:default-sampler (setf (config-default-sampler c) v))))
    c))

(defun load-config (&optional (path (config-file-path)))
  (if (probe-file path)
      (parse-config
       (let ((*read-eval* nil))
         (with-open-file (s path :direction :input) (read s))))
      (default-config)))

(defun ensure-config-file (&optional (path (config-file-path)))
  "Write a default config file if none exists. Returns (values path created-p)."
  (if (probe-file path)
      (values path nil)
      (progn
        (ensure-directories-exist path)
        (with-open-file (s path :direction :output :if-does-not-exist :create)
          (format s ";;; tui-chat configuration. Edited as a Lisp plist.~%")
          (format s ";;; With :auto-resources t, :n-ctx and :n-gpu-layers are ceilings —~%")
          (format s ";;; cl-llama-cpp sizes them to fit free VRAM. Set it to nil to use~%")
          (format s ";;; the values below verbatim.~%")
          (format s "(:model-path ~s~%" (namestring (config-model-path (default-config))))
          (format s " :auto-resources t~%")
          (format s " :n-ctx 8192 :n-gpu-layers 99 :max-tokens 256~%")
          (format s " :system-prompt ~s~%" "You are a helpful assistant.")
          (format s " :presets ((\"balanced\" :temp 0.7)~%")
          (format s "           (\"creative\" :temp 1.4 :top-p 0.95 :min-p 0.05)~%")
          (format s "           (\"wild\"     :temp 1.9 :top-p 0.98 :min-p 0.02)~%")
          (format s "           (\"precise\"  :temp 0.2))~%")
          (format s " :default-sampler \"balanced\")~%"))
        (values path t))))

(defun config-preset (config name)
  (let ((entry (assoc name (config-presets config) :test #'string=)))
    (unless entry (error "Unknown sampler preset: ~a" name))
    (cdr entry)))

(defun config-default-preset (config)
  (config-preset config (config-default-sampler config)))
