(defsystem #:skin.djha.jfon
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               #:com.inuoe.jzon
               #:fset
               #:trivial-features
               #:trivial-package-local-nicknames
               )
  :components ((:module "jfon"
                        :components
                        (
                         (:file "main")
                         )))
  :description "Port of com.inuoe.jzon to use FSet collections."
  :in-order-to ((test-op (test-op "skin.djha.jfon/tests"))))

(defsystem #:skin.djha.jfon/tests
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               #:skin.djha.jfon
               #:cl-ppcre
               #:rove
               )

  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for jfon"
  :perform (test-op (op c) (symbol-call :rove :run c)))
