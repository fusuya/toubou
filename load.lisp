(ql:quickload '(:jonathan :cl-ppcre :usocket :verbose))

(defpackage :toubou
  (:use :common-lisp :cl-user :sb-bsd-sockets)
  (:export #:gui-main))
(in-package :toubou)

(load "toubou.lisp" :external-format :utf-8)

(server-main)
#|
(sb-ext:save-lisp-and-die "mogerpg"
			  :toplevel #'main
			  :save-runtime-options t
			  :executable t)
|#

