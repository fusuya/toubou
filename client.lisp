(ql:quickload '(:jonathan :cl-ppcre :ltk))

(defpackage clilin
   (:use :common-lisp :cl-user :ltk :sb-bsd-sockets)
   (:export #:gui-cli))
(in-package :clilin)


(defun make-socket-stream (address port)
  (let ((sock (make-instance 'inet-socket :type :stream :protocol :tcp))
        (addr (make-inet-address address)))
    (if (ignore-errors (socket-connect sock addr port)
		       t)
	(socket-make-stream sock :input t :output t :element-type 'character)
	nil)))

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
	   (f2 (make-instance 'frame))
	   (lf1 (make-instance 'labelframe :master f :text "アドレス"))
	   (lf2 (make-instance 'labelframe :master f :text "ポート"))
	   (lf3 (make-instance 'labelframe :master f :text "名前"))
	   (addr (make-instance 'entry :master lf1 :width 13 :text "10.0.2.15"))
	   (port (make-instance 'entry :master lf2 :width 7 :text "9999"))
	   (name (make-instance 'entry :master lf3 :width 12 :text "もげ"))
	   (fr1 (make-instance 'labelframe :master f2 :text "画面"))
	   (gamen  (make-instance 'label :master fr1 :text "hoge"
					 :font "Takaoゴシック 14 normal"))
	   (stream nil);;(make-socket-stream))
	   (id nil) (state 'playing)
	   (start-btn (make-instance 'button :master f :text "接続")))
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
	(pack (list f f2))
	(pack (list lf1 lf2 lf3 start-btn) :side :left)
	(pack (list addr port name))
	(pack fr1)
	(pack gamen)
	(setf (command start-btn)
	      (lambda ()
		(setf stream (make-socket-stream (format nil "~a" (text addr))
						 (parse-integer (format nil "~a" (text port)))))
		(if stream
		    (progn
		      (format stream "~a~%" (text name))
		      (force-output stream)
		      
		      
		      (setf id (parse-integer (read-line stream)))
		      (setf (text gamen)
			    "受付完了：ゲーム開始待ち中")
		      (process-events)
		      (iter))
		    (setf (text gamen)
			  "だめぽ"))))

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
	;;(mainloop)
	))))

(gui-cli)
