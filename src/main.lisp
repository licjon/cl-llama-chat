(in-package #:cl-llama-chat)

(defun silence-llama-logging ()
  "Suppress llama.cpp / ggml log spam (model-load chatter, 'CUDA Graph id N
reused', etc.). Only error-level messages are surfaced, on *error-output*.
llama_log_set also routes ggml/CUDA messages, so one callback covers both."
  (llama:set-log-callback
   (lambda (level text)
     (when (>= level 4)                 ; 1=debug 2=info 3=warn 4=error
       (write-string text *error-output*)))))

(defun load-with-planning (cfg)
  "Load the model named by CFG, using cl-llama-cpp's resource planner to size
N-CTX / N-GPU-LAYERS to free VRAM when :auto-resources is true. Returns
 (values model n-ctx n-gpu-layers); the caller owns MODEL and must free it."
  (let* ((path (namestring (config-model-path cfg)))
         (gpu  (config-n-gpu-layers cfg))
         (model (llama:make-model path :n-gpu-layers gpu)))
    (if (config-auto-resources cfg)
        (let ((plan (llama:suggest-configuration
                     model :n-ctx (config-n-ctx cfg) :n-gpu-layers gpu)))
          (cond
            ((null plan)                                  ; no GPU detected
             (format t "~&Resource planning: no GPU budget; using config values.~%")
             (values model (config-n-ctx cfg) gpu))
            ((>= (getf plan :n-gpu-layers) gpu)           ; full offload fits
             (values model (getf plan :n-ctx) gpu))
            (t                                            ; tight VRAM: reload smaller
             (let ((gl (getf plan :n-gpu-layers)))
               (llama:free-model model)
               (format t "~&Resource planning: VRAM is tight; reloading with ~d GPU layers.~%" gl)
               (values (llama:make-model path :n-gpu-layers gl)
                       (getf plan :n-ctx) gl)))))
        (values model (config-n-ctx cfg) gpu))))

(defun run (&key config-path)
  (silence-llama-logging)
  (multiple-value-bind (path created)
      (ensure-config-file (or config-path (config-file-path)))
    (when created
      (format t "~&Wrote default config to ~a — edit it to set your model path.~%"
              path)))
  (let ((cfg (load-config (or config-path (config-file-path)))))
    (unless (probe-file (config-model-path cfg))
      (format t "~&Model not found: ~a~%Edit ~a and set :model-path.~%"
              (config-model-path cfg) (config-file-path))
      (return-from run nil))
    (format t "~&Loading model ~a …~%" (config-model-path cfg))
    (multiple-value-bind (model n-ctx gpu) (load-with-planning cfg)
      (setf (config-n-ctx cfg) n-ctx
            (config-n-gpu-layers cfg) gpu)
      (unwind-protect
           (llama:with-context (ctx model :n-ctx n-ctx)
             (format t "~&Ready — n-ctx ~d, ~d GPU layers. /help for commands.~%" n-ctx gpu)
             (let ((engine (make-engine cfg model ctx)))
               (unwind-protect (run-ui engine)
                 (free-engine-speculative engine))))
        (llama:free-model model))))
  nil)

(defun main () (run))
