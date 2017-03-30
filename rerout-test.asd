(asdf:defsystem :rerout-test
  :description "Tests for Rerout"
  :author "Joram Schrijver <i@joram.io>"
  :depends-on (:rerout :parachute)
  :components ((:file "test"))
  :perform (test-op :after (op component)
                    (uiop:symbol-call :parachute :test :rerout-test)))
