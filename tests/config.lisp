(defpackage #:tui-chat/tests/config
  (:use #:cl #:rove))
(in-package #:tui-chat/tests/config)

(deftest defaults
  (let ((c (tui-chat::default-config)))
    (ok (equal (tui-chat::config-default-sampler c) "balanced"))
    (ok (= (tui-chat::config-n-ctx c) 8192))
    (ok (tui-chat::config-auto-resources c))
    (ok (search "qwen" (namestring (tui-chat::config-model-path c))))))

(deftest parse-merges-over-defaults
  (let ((c (tui-chat::parse-config '(:n-ctx 2048 :default-sampler "precise"))))
    (ok (= (tui-chat::config-n-ctx c) 2048))
    (ok (equal (tui-chat::config-default-sampler c) "precise"))
    ;; untouched keys keep their default
    (ok (= (tui-chat::config-n-gpu-layers c) 99))))

(deftest preset-lookup
  (let ((c (tui-chat::default-config)))
    (ok (equal (getf (tui-chat::config-preset c "balanced") :temp) 0.7))
    (ok (signals (tui-chat::config-preset c "nope") 'error))))

(deftest preset-from-custom-plist
  (let ((c (tui-chat::parse-config
            '(:presets (("hot" :temp 1.5)) :default-sampler "hot"))))
    (ok (equal (getf (tui-chat::config-default-preset c) :temp) 1.5))))
