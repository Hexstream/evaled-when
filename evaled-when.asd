(asdf:defsystem #:evaled-when

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  :license "Unlicense"

  :description "Provides a way of extracting and replicating the compile-time side-effects of forms."

  :depends-on ("trivial-cltl2")

  :version "1.0"
  :serial cl:t
  :components ((:file "package")
	       (:file "main"))

  :in-order-to ((asdf:test-op (asdf:test-op #:evaled-when_tests))))
