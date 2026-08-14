(defpackage #:cl-llama-chat
  (:use #:cl)
  (:local-nicknames (#:llama #:cl-llama-cpp)
                    (#:spec #:cl-llama-cpp/common/speculative))
  (:export #:main #:run))
