(defsystem "cl-nanomsg"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:cffi)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-nanomsg/tests"))))

(defsystem "cl-nanomsg/tests"
  :author ""
  :license ""
  :depends-on ("cl-nanomsg"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-nanomsg"
  :perform (test-op (op c) (symbol-call :rove :run c)))
