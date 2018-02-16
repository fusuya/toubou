(ql:quickload (list :jonathan :cl-ppcre :ltk))

(defpackage :toubou
  (:use :common-lisp :cl-user :ltk)
  (:export #:gui-main))
(in-package :toubou)

(load "toubou.lisp" :external-format :utf-8)


(gui-main)
#|
(sb-ext:save-lisp-and-die "mogerpg"
			  :toplevel #'main
			  :save-runtime-options t
			  :executable t)
|#

