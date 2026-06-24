(in-package #:tui-chat)

(defstruct (engine (:constructor make-engine*))
  config model ctx session default-sampler)

(defun make-engine (config model ctx)
  "Public factory: build an ENGINE with a fresh chat session seeded from CONFIG."
  (let ((session (llama:make-chat-session
                  ctx :system-prompt (config-system-prompt config))))
    (make-engine* :config config :model model :ctx ctx :session session
                  :default-sampler (config-default-sampler config))))

(defun engine-sampler-plist (engine name)
  "Resolve NAME (string) to a sampler plist, or the current default when NIL."
  (let ((cfg (engine-config engine)))
    (if name (config-preset cfg name)
        (config-preset cfg (engine-default-sampler engine)))))

(defun %sampler-config (plist)
  "Turn a preset plist into a cl-llama-cpp sampler-config object."
  (apply #'llama:make-sampler-config plist))

(defun engine-send (engine text &key on-token)
  "Normal turn: send TEXT through the main chat session with the default sampler.
ON-TOKEN, when supplied, is called with each token string as it streams."
  (let* ((preset (engine-sampler-plist engine nil))
         (cb (when on-token
               (lambda (tok) (funcall on-token tok) t))))
    (values
     (llama:chat-session-send
      (engine-session engine) text
      :sampler-config (%sampler-config preset)
      :max-tokens (config-max-tokens (engine-config engine))
      :token-callback cb))))

(defun %branch-label (which preset-name)
  (format nil "~a: ~a" (if (eq which :a) "A" "B") preset-name))

(defun engine-branch (engine text preset-a preset-b &key on-token)
  "Generate two alternate replies to TEXT from the current conversation point,
without disturbing the main session. Uses a scratch context: prefill the prompt
once, snapshot it, then fork each candidate via load-state.
Returns a list of two plists, each (:label string :preset string :text string).
ON-TOKEN, when supplied, is called as (funcall on-token which tok) with WHICH in
{:a :b}."
  (let* ((cfg (engine-config engine))
         (model (engine-model engine))
         (session (engine-session engine))
         (name-a (or preset-a (engine-default-sampler engine)))
         (name-b (or preset-b (engine-default-sampler engine)))
         (plist-a (config-preset cfg name-a))
         (plist-b (config-preset cfg name-b))
         (messages (append (llama:chat-session-messages session)
                           (list (list :role "user" :content text))))
         (prompt (llama:tokenize-chat model messages :add-assistant-prefix t))
         ;; The scratch context only needs to hold the prompt plus one candidate,
         ;; so keep it as small as possible (capped at the main n-ctx) to avoid
         ;; doubling KV-cache VRAM while a branch is in flight.
         (scratch-n-ctx (min (config-n-ctx cfg)
                             (+ (length prompt) (config-max-tokens cfg) 64))))
    (llama:with-context (scratch model :n-ctx scratch-n-ctx)
      ;; Prefill the shared prompt exactly once.
      (llama:prefill scratch prompt)
      (let ((snapshot (llama:save-state scratch)))
        (flet ((candidate (which name plist)
                 (llama:load-state scratch snapshot)
                 ;; Diverge unless the preset pins a seed: :random tells the C
                 ;; sampler to draw a fresh nondeterministic seed, so the two
                 ;; branches genuinely differ and [r]egenerate yields fresh
                 ;; options instead of repeating the same pair.
                 (let ((cb (when on-token
                             (lambda (tok) (funcall on-token which tok) t))))
                   (list :label (%branch-label which name)
                         :preset name
                         :text (llama:generate
                                scratch nil
                                :prompt-tokens prompt
                                :sampler-config (%sampler-config plist)
                                :max-tokens (config-max-tokens cfg)
                                :seed (or (getf plist :seed) :random)
                                :token-callback cb)))))
          (list (candidate :a name-a plist-a)
                (candidate :b name-b plist-b)))))))

(defun engine-commit (engine text chosen-text)
  "Adopt CHOSEN-TEXT as the assistant reply to TEXT. Append both messages to the
session; the next send re-renders and decodes the delta."
  (let ((session (engine-session engine)))
    (setf (llama:chat-session-messages session)
          (append (llama:chat-session-messages session)
                  (list (list :role "user" :content text)
                        (list :role "assistant" :content chosen-text)))))
  nil)

(defun engine-regenerate (engine &key on-token)
  "Replace the last assistant reply with a freshly sampled one for the same user
message, using the default sampler and a new seed. Returns the new reply string,
or NIL if the conversation does not end in a user/assistant exchange to redo.
The new reply is generated in a scratch context (the main session is untouched
until the reply is committed), mirroring how branching works."
  (let* ((session (engine-session engine))
         (rev (reverse (llama:chat-session-messages session))))
    (unless (and rev
                 (string= (getf (first rev) :role) "assistant")
                 (second rev)
                 (string= (getf (second rev) :role) "user"))
      (return-from engine-regenerate nil))
    (let* ((cfg (engine-config engine))
           (model (engine-model engine))
           ;; Conversation without the trailing assistant reply; it now ends on
           ;; the user turn, so the model generates a fresh reply to it.
           (base-msgs (reverse (cdr rev)))
           (prompt (llama:tokenize-chat model base-msgs :add-assistant-prefix t))
           (scratch-n-ctx (min (config-n-ctx cfg)
                               (+ (length prompt) (config-max-tokens cfg) 64)))
           (preset (engine-sampler-plist engine nil))
           (cb (when on-token (lambda (tok) (funcall on-token tok) t)))
           ;; :random forces a fresh nondeterministic seed so the redo differs
           ;; from the reply being replaced (overriding any seed the preset pins).
           (reply (llama:with-context (scratch model :n-ctx scratch-n-ctx)
                    (llama:generate scratch nil
                                    :prompt-tokens prompt
                                    :sampler-config (%sampler-config preset)
                                    :max-tokens (config-max-tokens cfg)
                                    :seed :random
                                    :token-callback cb))))
      ;; Commit: swap in the new reply (user turn already present).
      (setf (llama:chat-session-messages session)
            (append base-msgs (list (list :role "assistant" :content reply))))
      reply)))

(defun engine-set-default-sampler (engine name)
  "Validate NAME against the presets and make it the persistent default."
  (config-preset (engine-config engine) name) ; validate; errors if unknown
  (setf (engine-default-sampler engine) name))

(defun engine-reset (engine)
  (llama:chat-session-reset (engine-session engine) :keep-system t)
  nil)
