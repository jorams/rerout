(asdf:defsystem #:rerout
  :description "Simple, regex-based HTTP router."
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:cl-ppcre #:quri)
  :components ((:file "rerout")))
