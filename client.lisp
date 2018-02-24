

(defun looks-like-ip (str)
  (every (lambda (c)
           (case c
             ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.) t)
             (t nil))) str))

(defun make-socket-stream (address port)
  (let ((sock (make-instance 'inet-socket :type :stream :protocol :tcp))
        (addr (if (looks-like-ip address)
                  (make-inet-address address)
                (handler-case
                 (car (host-ent-addresses (get-host-by-name address)))

                 (host-not-found-error
                  (c)
                  (declare (ignore c))

                  (do-msg (format nil "ホスト~aのIPアドレスがわかりませんでした。" address))
                  (return-from make-socket-stream nil))))))

    (if (ignore-errors (socket-connect sock addr port) t)
	;;element-typeをdefaultにすると文字とバイナリがつかえるらしい(sbcl)
        (socket-make-stream sock :input t :output t :element-type :default)
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
            ((find xy hunters-pos-list :test #'equal)
             (format out "ハ"))
            ((find xy players-pos-list :test #'equal)
             (let ((player (find-if (lambda (p) (equal xy (pos->xy (lookup "pos" p)))) players)))
               (cond
                ((lookup "dead" player) (format out "墓"))
                (t (format out (lookup "atama" player))))))
            ((find xy su :test #'equal)
             (format out "巣"))
            ((find xy blocks :test #'equal)
             (format out "□"))
            (t (format out "　")))))
       (fresh-line out)))))

(defun read-message (stream)
  (jonathan:parse (read-line stream) :as :alist))

;; (defun message-type (message)
;;   (intern (map 'string #'char-upcase (lookup "type" message))))
(defun message-type (message)
  (lookup "type" message))

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
            (let* ((stream nil)
                   (id nil)
                   (command nil)

                   ;; ウィジェット。
                   (f (make-instance 'frame))
                   (f2 (make-instance 'frame))
                   (lf1 (make-instance 'labelframe :master f :text "アドレス"))
                   (lf2 (make-instance 'labelframe :master f :text "ポート"))
                   (lf3 (make-instance 'labelframe :master f :text "名前"))
                   (fr1 (make-instance 'labelframe :master f2 :text "画面"))
                   (start-btn (make-instance 'button :master f :text "接続"))
                   (gamen (make-instance 'label :master fr1 :text "hoge"
                                         :font "Takaoゴシック 14 normal"))
                   (addr (make-instance 'entry :master lf1 :width 13 :text "127.0.0.1"))
                   (port (make-instance 'entry :master lf2 :width 7 :text "12121"))
                   (name (make-instance 'entry :master lf3 :width 12 :text "もげ")))

              (pack (list f f2))
              (pack (list lf1 lf2 lf3 start-btn) :side :left)
              (pack (list addr port name))
              (pack fr1)
              (pack gamen)

              (labels
                  ((am-i-dead?
                    (map-message)

                    (let ((me (find-if (lambda (p) (equal id (lookup "id" p))) (lookup "players" map-message))))
                      (lookup "dead" me)))

                   (display-map
                    (data)

                    (setf (text gamen)
                          (let ((out (make-string-output-stream)))
                            (format out "ターン~a~%" (lookup "turn" data))
                            (game-show data out)
                            (get-output-stream-string out))))

                   (display-result
                    (result-message)

                    (let ((out (make-string-output-stream)))
                      (format out "結果:~%")
                      (loop for row in (lookup "ranking" result-message)
                            for i from 1
                            do
                            (format out "~a位 ~a スコア ~a~%" i
                                    (lookup "name" row)
                                    (lookup "score" row)))
                      (setf (text gamen) (get-output-stream-string out))))

                   (display-status
                    (status-message)

                    (let ((players (lookup "players" (lookup "map" status-message)))
                          (out (make-string-output-stream)))
                      (format out "参加者待ち(最大~a秒)~%" (lookup "timeout-seconds" status-message))
                      (format out "プレーヤー:~%")
                      (dolist (p players)
                        (format out "~a~%" (lookup "name" p)))

                      (setf (text gamen) (get-output-stream-string out))))

                   (wait-game-start
                    ()

                    (if (listen stream)
                        (progn
                          (let* ((message (read-message stream))
				 (mes (message-type message)))
			    ;;(case (message-type message)
			    ;;save-lisp-and-dieで作った実行ファイルだとエラーがでるので
			    ;;文字列比較にした
			    (cond
                              ((string= mes "status")
                               (display-status message)
                               (after 50 #'wait-game-start))
                              ((string= mes "map")
                               (display-map message)
                               (after-idle #'send-command))
                              (t
                               (error (format nil "予期しないメッセージタイプ: ~s"
                                              (message-type message)))))))
                      (after 50 #'wait-game-start)))

                   (send-command
                    ()

                    (if command
                        (progn
                          (format stream "~a~%" command)
                          (force-output stream)
                          (setf command nil)
                          (after-idle #'wait-map))
                      (after 50 #'send-command)))

                   (wait-map
                    ()

                    (if (listen stream)
                        (progn
                          (let* ((message (read-message stream))
				 (mes (message-type message)))
                            (cond
                              ((string= mes "map")
                               (display-map message)
                               (if (am-i-dead? message)
                                   (progn
                                     (after 50 #'wait-map))
                                 (progn
                                   (after-idle #'send-command))))
                              ((string= mes "result")
                               (display-result message)
                               (configure start-btn :state :normal))
                              (t
                               (error (format nil "予期しないメッセージタイプ: ~s"
                                              (message-type message)))))))
                      (after 50 #'wait-map))))

              (setf (command start-btn)
                    (lambda ()
                      (setf stream (make-socket-stream (text addr) (parse-integer (text port))))

                      (if stream
                          (progn
			    ;;改行追加
                            (loop for byte across (babel:string-to-octets (format nil "~A~%" (text name)) :encoding :utf-8) do
			      (write-byte byte stream))
                            (force-output stream)

                            (setf id (parse-integer (read-line stream)))

                            (setf (text gamen)
                                  "受付完了：ゲーム開始待ち中")

                            (configure start-btn :state :disabled)
                            (after-idle #'wait-game-start))
                        (progn
                          (do-msg "接続に失敗。")))))

              ;;キー入力イベント
              (mapc (lambda (key cmd)
                      (bind *tk* key
                            (lambda (event)
                              (declare (ignore event))

                              (setf command cmd))))
                    '("<Right>" "<Left>" "<Up>" "<Down>" "<End>")
                    '("RIGHT" "LEFT" "UP" "DOWN" "STAY"))

              ;;ループ
              (mainloop)))))

