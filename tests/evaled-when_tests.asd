(asdf:defsystem #:evaled-when_tests

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  :license "Unlicense"

  :description "evaled-when unit tests."

  :depends-on ("evaled-when"
               "parachute"
               "enhanced-boolean")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:evaled-when_tests)))
