(defpackage #:cl-llama-chat/tests/config
  (:use #:cl #:rove))
(in-package #:cl-llama-chat/tests/config)

(deftest defaults
  (let ((c (cl-llama-chat::default-config)))
    (ok (equal (cl-llama-chat::config-default-sampler c) "balanced"))
    (ok (= (cl-llama-chat::config-n-ctx c) 8192))
    (ok (cl-llama-chat::config-auto-resources c))
    (ok (search "qwen" (namestring (cl-llama-chat::config-model-path c))))))

(deftest parse-merges-over-defaults
  (let ((c (cl-llama-chat::parse-config '(:n-ctx 2048 :default-sampler "precise"))))
    (ok (= (cl-llama-chat::config-n-ctx c) 2048))
    (ok (equal (cl-llama-chat::config-default-sampler c) "precise"))
    ;; untouched keys keep their default
    (ok (= (cl-llama-chat::config-n-gpu-layers c) 99))))

(deftest preset-lookup
  (let ((c (cl-llama-chat::default-config)))
    (ok (equal (getf (cl-llama-chat::config-preset c "balanced") :temp) 0.7))
    (ok (signals (cl-llama-chat::config-preset c "nope") 'error))))

(deftest preset-from-custom-plist
  (let ((c (cl-llama-chat::parse-config
            '(:presets (("hot" :temp 1.5)) :default-sampler "hot"))))
    (ok (equal (getf (cl-llama-chat::config-default-preset c) :temp) 1.5))))

(deftest speculative-config-defaults
  (let ((c (cl-llama-chat::default-config)))
    (ok (null (cl-llama-chat::config-speculative c)))
    (ok (= (cl-llama-chat::config-speculative-n c) 4))
    (ok (= (cl-llama-chat::config-speculative-m c) 48))))

(deftest speculative-config-parse
  (let ((c (cl-llama-chat::parse-config
            '(:speculative :ngram :speculative-n 5 :speculative-m 64))))
    (ok (eq (cl-llama-chat::config-speculative c) :ngram))
    (ok (= (cl-llama-chat::config-speculative-n c) 5))
    (ok (= (cl-llama-chat::config-speculative-m c) 64))))
