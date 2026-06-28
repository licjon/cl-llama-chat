(defsystem "cl-llama-chat"
  :version "0.1.0"
  :author "licjon"
  :license ""
  :depends-on ("cl-llama-cpp")
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "config")
                             (:file "commands")
                             (:file "ui-format")
                             (:file "engine")
                             (:file "ui")
                             (:file "main"))))
  :description "Terminal LLM chat with side-by-side branch/sampler comparison."
  :in-order-to ((test-op (test-op "cl-llama-chat/tests"))))

(defsystem "cl-llama-chat/tests"
  :author "licjon"
  :license ""
  :depends-on ("cl-llama-chat"
               "rove")
  :components ((:module "tests"
                :serial t
                :components ((:file "config")
                             (:file "commands")
                             (:file "ui-format")
                             (:file "engine-smoke"))))
  :description "Test system for cl-llama-chat"
  :perform (test-op (op c) (symbol-call :rove :run c)))
