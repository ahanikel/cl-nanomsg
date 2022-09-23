(defpackage cl-nanomsg/tests/main
  (:use :cl
        :cl-nanomsg
        :rove))
(in-package :cl-nanomsg/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-nanomsg)' in your Lisp.

(deftest test-with-socket
  (testing "with-socket should return an int >= 0"
    (ok (>= (with-socket (sock nn-rep) sock) 0))))
