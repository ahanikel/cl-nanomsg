(defpackage cl-nanomsg
  (:use :cl :cffi)
  (:export :with-socket
	   :nn-rep))
(in-package :cl-nanomsg)

(define-foreign-library libnanomsg
  (:darwin (:or "libnanomsg.5.dylib" "libnanomsg.dylib"))
  (:unix   (:or "libnanomsg.so.5" "libnanomsg.so"))
  (t       (:default "libnanomsg")))

(use-foreign-library libnanomsg)

(defconstant af-sp 1
  "Standard full-blown SP socket")

(defconstant af-sp-raw 2
  "Raw SP socket. Raw sockets omit the end-to-end functionality found in AF_SP sockets and thus can be used to implement intermediary devices in SP topologies.")

(defconstant nn-sockaddr-max 128
  "Maximum length of an address string.")

(defconstant nn-sol-socket 0
  "Generic socket option level.")

;; Generic socket options
(defconstant nn-linger 1)
(defconstant nn-sndbuf 2)
(defconstant nn-rcvbuf 3)
(defconstant nn-sndtimeo 4)
(defconstant nn-rcvtimeo 5)
(defconstant nn-reconnect-ivl 6)
(defconstant nn-reconnect-ivl-max 7)
(defconstant nn-sndprio 8)
(defconstant nn-rcvprio 9)
(defconstant nn-sndfd 10)
(defconstant nn-rcvfd 11)
(defconstant nn-domain 12)
(defconstant nn-protocol 13)
(defconstant nn-ipv4only 14)
(defconstant nn-socket-name 15)
(defconstant nn-rcvmaxsize 16)
(defconstant nn-maxttl 17)

;; Send/recv options.
(defconstant nn-dontwait 1)

;; pubsub
(defconstant nn-pub 32)
(defconstant nn-sub 33)
(defconstant nn-sub-subscribe 1)
(defconstant nn-sub-unsubscribe 2)

;; reqrep
(defconstant nn-req 48)
(defconstant nn-rep 49)

;; pipeline
(defconstant nn-push 80)
(defconstant nn-pull 81)

;; survey
(defconstant nn-surveyor 98)
(defconstant nn-respondent 99)
(defconstant nn-surveyor-deadline 1)

;; bus
(defconstant nn-bus 102)


(defcfun "nn_socket" :int
  (domain :int)
  (protocol :int))

(defcfun "nn_close" :int
  (socket :int))

(defcfun "nn_setsockopt" :int
  (socket :int)
  (level :int)
  (option :int)
  (optval :pointer)
  (optvallen :long))

(defcfun "nn_getsockopt" :int
  (socket :int)
  (level :int)
  (option :int)
  (optval :pointer)
  (optvallen :long))

(defcfun "nn_bind" :int
  (socket :int)
  (addr :string))

(defcfun "nn_connect" :int
  (socket :int)
  (addr :string))

(defcfun "nn_shutdown" :int
  (socket :int)
  (how :int))

(defcfun "nn_send" :int
  (socket :int)
  (buf :pointer)
  (len :long)
  (flags :int))

(defcfun "nn_recv" :int
  (socket :int)
  (buf :pointer)
  (len :long)
  (flags :int))

(defstruct nn-iovec
  (iov-base :pointer)
  (iov-len :long))

(defstruct nn-msghdr
  (msg-iov :pointer)
  (msg-iovlen :long)
  (msg-control :pointer)
  (msg-controllen :long))

(defcfun "nn_sendmsg" :int
  (socket :int)
  (msghdr :pointer)
  (flags :int))

(defcfun "nn_recvmsg" :int
  (socket :int)
  (msghdr :pointer)
  (flags :int))

(defcfun "nn_allocmsg" :pointer
  (size :long)
  (type :int))

(defcfun "nn_reallocmsg" :pointer
  (msg  :pointer)
  (size :long))

(defcfun "nn_freemsg" :int
  (msg  :pointer))

(defcfun "nn_poll" :int
  (fds :pointer)
  (nfds :int)
  (timeout :int))

(defcfun "nn_errno" :int)

(defcfun "nn_term" :void)

(defcfun "strerror" :pointer
  (errnum :int))

(defun errno-to-string (errnum)
  (foreign-string-to-lisp (strerror errnum)))

(define-condition os-error (error)
  ((errno :initarg :errno
	  :reader  errno
	  :type    :int)))

(defmethod print-object ((obj os-error) stream)
  (print-unreadable-object (obj stream)
    (format stream "OS error ~a: ~A."
	    (errno obj)
	    (errno-to-string (errno obj)))))

(defmacro check-error (&body body)
  (let ((retvar (gensym)))
    `(let ((,retvar ,@body))
       (if (>= ,retvar 0)
	   ,retvar
	   (error 'os-error :errno (nn-errno))))))

(defun make-socket (protocol &optional (domain af-sp))
  (check-error (nn-socket domain protocol)))

(defun bind-socket (socket addr)
  (check-error (nn-bind socket addr)))

(defun recv-socket (socket &optional (max-len 1024) (flags 0))
  (with-foreign-object (buf :char max-len) 
    (check-error (nn-recv socket buf max-len flags))
    (foreign-string-to-lisp buf)))

(defun close-socket (socket)
  (check-error (nn-close socket)))

(defun send-socket (socket msg &optional (flags 0))
  (check-error
    (nn-send socket
     (if (stringp msg)
	 (foreign-string-alloc msg)
	 (foreign-alloc :char :initial-contents msg))
     (length msg)
     flags)))

(defmacro with-socket ((socket-var socket-type) &body body)
  `(let ((,socket-var (make-socket ,socket-type)))
     (unwind-protect
	  (progn ,@body)
       (close-socket ,socket-var))))

(defun test-reqrep ()
  (with-socket (sock nn-rep)
    (bind-socket sock "tcp://*:5000")
    (let ((ret (recv-socket sock)))
      (print ret) (finish-output)
      (send-socket sock "hello"))))
