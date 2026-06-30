(defpackage #:cl-llama-chat
  (:use #:cl)
  (:local-nicknames (#:llama #:cl-llama-cpp)
                    (#:spec #:cl-llama-cpp-extras/speculative))
  (:export #:main #:run))
