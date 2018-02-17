(ql:quickload '(:jonathan :cl-ppcre :ltk :usocket))

(defpackage clilin
   (:use :common-lisp :cl-user :ltk :sb-bsd-sockets)
   (:export #:gui-cli))
(in-package :clilin)


(defun make-socket-stream ()
  (let ((sock (make-instance 'inet-socket :type :stream :protocol :tcp))
        (addr (make-inet-address "127.0.0.1")))
    (socket-connect sock addr 9999)
    (socket-make-stream sock :input t :output t :element-type 'character)))

;; 文字列をキーとする連想リストの各値をキーと同名の変数に束縛する。
;; (alist-bind (a b c) '(("a" . 1) ("b" . 2))
;;   (list a b c)) ;=> (1 2 NIL)
(defmacro alist-bind (spec alist &rest body)
  (let ((alist-name (gensym)))
    `(let ((,alist-name ,alist))
       (destructuring-bind ,spec
	   (list
	    ,@(mapcar
	       (lambda (key)
		 `(let ((pair (assoc (symbol-name (quote ,key))
				     ,alist-name
				     :test #'equalp)))
		    (if pair (cdr pair) nil)))
	       spec))
	 ,@body))))


(defun lookup (key alist)
  (cdr (assoc key alist :test #'equal)))

(defun pos->xy (pos)
  (alist-bind
   (x y) pos
   (list x y)))

;;ゲーム描画
(defun game-show (data out)
  (alist-bind
   (blocks players hunters su) data
   (let* ((hunters-pos-list (loop for p in hunters
                              collect
                              (pos->xy (lookup "pos" p))))
          (players-pos-list (loop for p in players
                              collect
                              (pos->xy (lookup "pos" p))))
	  (x-max (caar (sort blocks #'> :key #'car)))
	  (y-max (cadar (sort blocks #'> :key #'cadr))))
     (loop for y from 0 to y-max do
       (loop for x from 0 to x-max do
	 (let ((xy (list x y)))
	   (cond
	     ((find xy blocks :test #'equal)
	      (format out "□"))
	     ((find xy su :test #'equal)
	      (format out "巣"))
	     ((find xy hunters-pos-list :test #'equal)
	      (format out "ハ"))
	     ((find xy players-pos-list :test #'equal)
	      (format out "プ"))
	     (t (format out "　")))))
       (fresh-line out)))))
	   
 
;;GUI
(defun gui-cli ()
  (with-ltk ()
    (wm-title *tk* "逃亡クライアント")
    (bind *tk* "<Alt-q>"
      (lambda (event)
	(declare (ignore event))
	(process-events)
	(return-from gui-cli)))
    (set-geometry *tk* 600 600 200 200)
    (configure *tk* :padx 16 :pady 16)
    (let* (;; ウィジェット。
	   (f (make-instance 'frame))
	   (fr1 (make-instance 'labelframe :master f :text "画面"))
	   (gamen  (make-instance 'label :master fr1
					 :font "Takaoゴシック 14 normal"))
	   (stream (make-socket-stream))
	   (id 0) (state 'hoge)
	   (start-btn (make-instance 'button :master f :text "スタート")))
      (labels
	  ((update-screen (data)
	     (setf (text gamen)
		   (let ((out (make-string-output-stream)))
		     (game-show data out)
		     (get-output-stream-string out))))
	   (iter ()
	     (case state
	       (playing
		(let ((data (jonathan:parse (read-line stream) :as :alist)))
		  ;; ゲーム状態の描画。
		  (update-screen data))))
	     ;;(sleep 0.03)
	     ;;(after-idle #'iter)
	     ))
	
	(pack f)
	(pack fr1)
	(pack start-btn)
	(pack gamen)
	(setf (command start-btn)
	      (lambda ()
		(format stream "クリリン~%")
		(force-output stream)
		(setf id (parse-integer (read-line stream))
		      state 'playing)
		(iter)))

	;;キー入力イベント
	(mapc (lambda (key cmd)
		(bind *tk* key
		  (lambda (event)
		    (declare (ignore event))
		    (format stream "~a~%" cmd)
		    (force-output stream)
		    (iter))))
	      '("<Right>" "<Left>" "<Up>" "<Down>" "<End>")
	      '("RIGHT" "LEFT" "UP" "DOWN" "STAY"))
	;;ループ
	;;(after-idle #'iter)
	(mainloop)))))

(gui-cli)
