(in-package "CL-USER")

;;;; A simple UDP send/recieve for Lispworks

;; Evolved from Marc Battyani's original: http://comments.gmane.org/gmane.lisp.lispworks.general/6643

(fli:define-c-typedef (socket (:foreign-name "SOCKET"))
                      (:unsigned :int))
 
(fli:define-c-struct (in-addr (:foreign-name "in_addr"))
                     (s-addr (:unsigned :long)))
 
(fli:define-c-struct (sockaddr-in (:foreign-name "sockaddr_in"))
                     (sin-family :short)
                     (sin-port (:unsigned :short))
                     ;(sin-addr (:unsigned :long))
                     (sin-addr :uint32)
                     (sin-zero (:c-array :char 8)))
 
(fli:define-c-typedef udp-buffer (:c-array (:unsigned :char) 64))
 
(fli:define-foreign-function (socket "socket" :source)
                             ((af :int) (type :int) (protocol :int))
                             :result-type socket
                             :language :ansi-c
                             :calling-convention :cdecl)
 
(fli:Define-c-struct (sockaddr
                      (:foreign-name "sockaddr")
                      (:forward-reference t)))
 
(fli:define-foreign-function (bind "bind" :source)
                             ((s socket)
                              (name (:pointer (:struct sockaddr-in)))
                              (namelen :int))
                             :result-type :int
                             :language :ansi-c
                             :calling-convention :cdecl)
 
(fli:define-foreign-function (set-sock-opt "setsockopt" :source)
                             ((s socket)
                              (level :int)
                              (optname :int)
                              (optval (:pointer :char))
                              (optlen :int))
                             :result-type :int
                             :language :ansi-c
                             :calling-convention :cdecl)
 
(fli:define-foreign-function (send-to "sendto" :source)
                             ((s socket)
                              ; (buf (:pointer :char))
                              (buf (:pointer :unsigned-byte))
                              (len :int)
                              (flags :int)
                              (to (:pointer (:struct sockaddr)))
                              (tolen :int))
                             :result-type :int
                             :language :ansi-c
                             :calling-convention :cdecl)
 
(fli:define-foreign-function (recv-from "recvfrom" :source)
                             ((s socket)
                              (buf :pointer)
                              (len :int)
                              (flags :int)
                              (from (:pointer (:struct sockaddr-in)))
                              (fromlen (:pointer :int)))
                             :result-type :int
                             :language :ansi-c
                             :calling-convention :cdecl)
 
(fli:define-foreign-function (close-socket "closesocket" :source)
                             ((s socket))
                             :result-type :int
                             :language :ansi-c
                             :calling-convention :cdecl)
 
(defconstant AF_INET           2)
(defconstant SOCK_DGRAM        2)
(defconstant IPPROTO_UDP      17)
(defconstant INADDR_ANY  #x00000000)
(defconstant IPPROTO_IP        0)
(defconstant IP_MULTICAST_TTL 33)
 

;;;; A UDP listener.

;;; currently this uses a lot of globals, so it only works for a single
;;; sender & listener

(defparameter *udp-port-for-listener* #x8913) ; 5001

(defvar *udp-socket* nil)
(defvar *udp-buffer* nil)
 
(defun start-udp-listener ()
  (unless *udp-socket*
    (setf *udp-socket* (socket AF_INET SOCK_DGRAM IPPROTO_UDP))
    (setf *udp-buffer* (fli:allocate-foreign-object :type '(:c-array (:unsigned :char) 100)))
    (fli:with-dynamic-foreign-objects ()
      (let ((recv-addr (fli:allocate-dynamic-foreign-object :type 'sockaddr-in)))
        (setf (fli:foreign-slot-value recv-addr 'sin-family) AF_INET
              (fli:foreign-slot-value recv-addr 'sin-port) *udp-port-for-listener*
              (fli:foreign-slot-value recv-addr 'sin-addr) INADDR_ANY)
        (bind *udp-socket* recv-addr (fli:size-of 'sockaddr-in)))))
  *udp-socket*)
 
(defun read-udp-socket ()
  (when *udp-socket*
    (fli:with-dynamic-foreign-objects ()
      (let* ((sender-addr (fli:allocate-dynamic-foreign-object :type 'sockaddr-in))
             (addr-size (fli:allocate-dynamic-foreign-object
                         :type :int :initial-element (fli:size-of 'sockaddr-in)))
             (nb-read (recv-from *udp-socket* *udp-buffer* 100 0 sender-addr addr-size)))
        (values
         (loop for i from 0 below nb-read collect (fli:foreign-aref *udp-buffer* i))
         (fli:foreign-slot-value sender-addr 'sin-port)
         (fli:foreign-slot-value sender-addr 'sin-addr))))))
 
(defun stop-udp-listener ()
  (when *udp-socket*
    (close-socket *udp-socket*)
    (setf *udp-socket* nil)))


;;;; UDP message sender

(defun htonl (v)
  (labels ((by (n) (ldb (byte 8 (* n 8)) v))
           (bs (n s) (ash (by n) (* 8 s))))
    (logior (bs 0 3)
            (bs 1 2)
            (bs 2 1)
            (bs 3 0))))

(defun htons (v)
  (labels ((by (n) (ldb (byte 8 (* n 8)) v))
           (bs (n s) (ash (by n) (* 8 s))))
    (logior (bs 0 1)
            (bs 1 0))))


(defvar *udp-sending-socket*)
(defvar *udp-buffer-pad*)
(defvar *udp-send-sockaddr_in*)

(defun init-udp-sender (ip port)
    (setf
     *udp-buffer-pad* (sys:in-static-area
                        (make-array 256
                                    :element-type '(unsigned-byte 8)))
     *udp-send-sockaddr_in* (fli:allocate-foreign-object :type 'sockaddr-in)
     (fli:foreign-slot-value *udp-send-sockaddr_in* 'sin-family) AF_INET
          (fli:foreign-slot-value *udp-send-sockaddr_in* 'sin-addr) (htonl ip)
          (fli:foreign-slot-value *udp-send-sockaddr_in* 'sin-port) (htons port))
    (setf *udp-sending-socket*
          (setf *udp-socket* (socket AF_INET SOCK_DGRAM IPPROTO_UDP))))


(defun send-udp (buffer start end)
  (replace *udp-buffer-pad* buffer :start2 start :end2 end)
  (fli:with-dynamic-lisp-array-pointer (ptr *udp-buffer-pad* :type :unsigned-byte)
    (send-to *udp-sending-socket* ptr (- end start)
             0 ; flags
             (fli:copy-pointer *udp-send-sockaddr_in* :type '(:struct sockaddr))
             (fli:size-of 'sockaddr-in))))
