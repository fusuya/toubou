
(load "maze-test.lisp")
(load "ai-ido.lisp")
(defparameter *tate* 11) ;;マップサイズ
(defparameter *yoko* 11)
(defparameter *ais* nil)

(defstruct player
  (posy 1)
  (posx 1)
  (end  0)
  (dead? nil)
  (name nil)
  (atama nil)
  (turn 0)
  (id 0))
  
(defstruct donjon
  (map nil)  ;;マップ
  (tate 13)  ;;縦幅
  (yoko 13)  ;;横幅
  (stop-list nil)) ;;行き止まりリスト

(defstruct ai
  command-line
  proc
  stream
  atama
  name
  (posx 6)
  (posy 6))

;;文字幅取得
(defun moge-char-width (char)
    (if (<= #x20 (char-code char) #x7e)
        1
	2))
;;string全体の文字幅
(defun string-width (string)
  (apply #'+ (map 'list #'moge-char-width string)))
;;最低n幅もったstring作成
(defun minimum-column (n string)
  (let ((pad (- n (string-width string))))
    (if (> pad 0)
	(concatenate 'string string (make-string pad :initial-element #\ ))
        string)))

(define-condition handshake-error (error) ())

(defun ascii->zenkaku (char)
  (code-char (+ 65248 (char-code char))))

(defun atama-of (name)
  (let ((atama (char name 0)))
    (cond
      ((= 2 (moge-char-width atama))
       (format nil "~C" atama))
      ((= 1 (moge-char-width atama))
       (format nil "~C" (ascii->zenkaku atama))))))

(defun receive-name (ai)
  (handler-case
      (let ((name (read-line (ai-stream ai))))
        (when (equal name "")
          (format t "AIの名前が空です。~%")
          (error 'handshake-error))
        (setf (ai-name ai) name
              (ai-atama ai) (atama-of name)))
    (end-of-file (c)
      (format t "~A~%" c)
      (error 'handshake-error))))

(defun load-ai (com)
  (destructuring-bind
        (command-name . args)
      (ppcre:split #\space com)
    (let* ((proc (sb-ext:run-program
                  command-name args
                  :input :stream
                  :output :stream
                  :wait nil
                  :search t))
           (stream (make-two-way-stream
                    (sb-ext:process-output proc)
                    (sb-ext:process-input proc)))
           (ai (make-ai :command-line com
                        :proc proc
                        :stream stream)))
      (receive-name ai)
      ai)))



;;勝利メッセージ
(defun victory-message ()
  (gamen-clear)
  (scr-format "~%~%")
  (scr-format "「大 勝 利 ！」~%~%")
  (scr-format "次へ = z")
  (read-command-char))


;;プレイヤーの生死判定
(defun player-dead (p)
  (player-dead? p))

;;n内の１以上の乱数
(defun randval (n)
  (1+ (random (max 1 n))))

;;a→ 0 b→ 1 c→ 2 ...
(defun ascii->number (x)
  (if (null (numberp x))
      (- (char-code (char (symbol-name x) 0)) 65)))

;; 1->a 2->b 3->c ...
(defun number->a (x)
  (code-char (+ x 96)))

;;-----------------------マップ------------------------------------------------------------
;;---------------------------------------------------------------------------------------
;;マップ移動




(defun map-type (num)
  (case num
    (30 "ロ") ;; 壁
    (0  "　")
    (1  "主") ;; プレイヤーの位置
    (5  "巣") ;; 巣
    ))

(defun player-list (p)
  (list :|player|
        (list :|pos| (list :|x| (player-posx p) :|y| (player-posy p)))))

(defun map-data-list (map)
  (let ((blocks nil)
        (su nil))
    (loop for i from 0 below (donjon-tate map) do
          (loop for j from 0 below (donjon-yoko map) do
                (let ((x (aref (donjon-map map) i j)))
                  (case x
                    (30 (push (list j i) blocks))
                    (5  (push (list j i) su))))))
    (list :|blocks| blocks :|su| su)))

(defun make-ai-data (ai-list)
  (list :|ais|
  (loop for ai in ai-list
        collect (list :|id| (player-id ai) :|pos| (list :|x| (player-posx ai) :|y| (player-posy ai))))))

(defun make-map-data (map p ai-list)
  (let ((map-data (map-data-list map))
        (player-data (player-list p))
        (ai-data  (make-ai-data ai-list)))
    (append map-data player-data ai-data)))

;;敵と重なったか
(defun encount-enemy (p ai-list)
  (let ((encount nil))
    (dolist (ai ai-list)
      (if (and (= (player-posx p) (player-posx ai))
	       (= (player-posy p) (player-posy ai)))
	  (setf encount t)))
    encount))

;;敵追加
(defun add-enemy (p ai-list)
  (let ((ai-num (length ai-list)))
    (when (zerop (mod (player-turn p) 20))
      (push (make-player :id ai-num :posx 6 :posy 6
			 :name "ハンター" :atama "ハ") ai-list))
    ai-list))

;;マップ表示
(defun show-map (map p ai-list)
  (gamen-clear)
  (scr-format "ターン:~d~%" (player-turn p))
  (loop for i from 0 below (donjon-tate map)
        do (loop for j from 0 below (donjon-yoko map)
                 do
                 (if (and (= i (player-posy p)) (= j (player-posx p)))
                     (scr-format "＠")
		     (let ((ai (find-if #'(lambda (ai) (and (= i (player-posy ai)) (= j (player-posx ai))))
					ai-list)))
		       (if ai
			   (scr-format (player-atama ai))
			   (scr-format (map-type (aref (donjon-map map) i j))))))
                 (if (= j (- (donjon-yoko map) 1))
                     (case i
                       (1 (scr-format "  r:終了~%"))
                       (otherwise
                        (scr-fresh-line)))))))

;;マップ表示
(defun show-map2 (map p ai-list)
  (let ((out (make-string-output-stream)))
    (format out "ターン:~d~%" (player-turn p))
    (loop for i from 0 below (donjon-tate map)
	  do (loop for j from 0 below (donjon-yoko map)
		   do
		      (if (and (= i (player-posy p)) (= j (player-posx p)))
			  (format out "＠")
			  (let ((ai (find-if #'(lambda (ai) (and (= i (player-posy ai)) (= j (player-posx ai))))
					     ai-list)))
			    (if ai
				(format out (player-atama ai))
				(format out (map-type (aref (donjon-map map) i j))))))
		      (if (= j (- (donjon-yoko map) 1))
			  (case i
			    (1 (format out "  r:終了~%"))
			    (otherwise
			     (fresh-line out))))))
    (get-output-stream-string out)))

(defun ending ()
  (scr-format "クリア！~%"))

(defun get-ai-command-line ()
  (with-open-file (in "ai.txt" :direction :input)
    (let ((num))
      (loop while (setf num (read-line in nil))
            collect num))))

(defun load-ais (command-lines)
  (setf *ais* (mapcar #'load-ai command-lines)))

(defun update-ais (ai-list map-data map)
  (dolist (ai ai-list)
    (let ((dir (map-mode (jonathan:parse (jonathan:to-json map-data) :as :alist) (player-id ai))))
      (cond
        ((equal dir "UP")    (update-map map ai -1 0))
        ((equal dir "DOWN")  (update-map map ai 1 0))
        ((equal dir "RIGHT") (update-map map ai 0 1))
        ((equal dir "LEFT")  (update-map map ai 0 -1))
        (t
         (error (format nil "マップモードで無効なコマンド: ~s (~a)" dir (player-name ai))))))))


;;コンティニューメッセージ
(defun continue-message ()
  (scr-format "もう一度挑戦しますか？(yes=1 or no=2)~%")
  (case (read-command-char)
    (1 (main))
    (2 nil)
    (otherwise (continue-message))))
;;ゲームオーバーメッセージ
(defun game-over-message ()
  (scr-format "Game Over.~%")
  (continue-message))

;;プレイヤーが死ぬか戦闘に入るか*end*=2になるまでループ
(defun main-game-loop (map p ai-list)
  (unless (or (= (player-end p) 2) (player-dead p))
    (map-move map p ai-list)
    (cond
      ((= (player-end p) 1) ;;ゲームクリア
       (ending))
      ((= (player-end p) 2) ;;ゲームーオーバー
       (Game-over-message))
      ((= (player-end p) 0) ;;ゲームループ
       (main-game-loop map p ai-list)))))
;;ゲーム開始
(defun main ()
  (init-charms)
  (setf *random-state* (make-random-state t))
  (let* ((p (make-player)) 
	 (map (make-donjon))
	 (ai1 (make-player :name "ハンター" :posx 6 :posy 6 :atama "ハ"))
         (ai-list (list ai1)))
    (setf (donjon-map map) *map100*) ;;マップ生成
    (main-game-loop map p ai-list)))



;;移動先選択
(defun ai-move (map p ai-list)
  (let ((map-data (make-map-data map p ai-list)))
    ;;(setf (text gamen) (show-map2 map p ai-list))
    ;;接触判定
    ;; (when (encount-enemy p ai-list)
    ;;   (setf (player-end p) 2)
    ;;   (return-from map-move2))
    ;;敵移動
    (update-ais ai-list map-data map)
    ;;接触判定
    (when (encount-enemy p ai-list)
      (setf (player-end p) 2)
      (return-from ai-move))))
    ;;ターン経過
    ;;(incf (player-turn p))
    ;;敵追加
;;(setf ai-list (add-enemy p ai-list))))


;;GUIメインループ
(defun game-start (gamen)
  (setf *random-state* (make-random-state t))
  (let* ((p (make-player)) 
	 (map (make-donjon))
	 (ai1 (make-player :name "ハンター" :posx 6 :posy 6 :atama "ハ"))
         (ai-list (list ai1))
	 (start-time (get-internal-real-time))
	 (timer 1))
    (setf (donjon-map map) *map100*) ;;マップ生成
    ;;キー入力イベント
    (bind *tk* "<Right>"
      (lambda (event) (update-map map p 0 1)
	(setf (text gamen) (show-map2 map p ai-list))))
    (bind *tk* "<Left>"
      (lambda (event) (update-map map p 0 -1)
	(setf (text gamen) (show-map2 map p ai-list))))
    (bind *tk* "<Up>"
      (lambda (event) (update-map map p -1 0)
	(setf (text gamen) (show-map2 map p ai-list))))
    (bind *tk* "<Down>"
      (lambda (event) (update-map map p 1 0)
	(setf (text gamen) (show-map2 map p ai-list))))
    ;;ループ
    (loop while (/= (player-end p) 2) do
      (when (zerop (mod timer 100))
	(ai-move map p ai-list)
	(setf (text gamen) (show-map2 map p ai-list)))
      (when (zerop (mod timer 1000))
	(setf ai-list (add-enemy p ai-list)))
      (incf timer) ;;敵の移動＆敵の追加に使うタイマー
      (sleep 0.01) ;;とりあえず入れておく
      (ltk:process-events))))
;;GUI
(defun gui-main ()
  (with-ltk
      ()
    (wm-title *tk* "GUI")
    (bind *tk* "<Alt-q>"
      (lambda (event)
	(declare (ignore event))
	(return-from gui-main)))
    (set-geometry *tk* 700 450 200 200)
    (configure *tk* :padx 16 :pady 16)
    (let* ((f (make-instance 'frame))
	   (fr1 (make-instance 'labelframe :master f :text "画面"))
	   (gamen  (make-instance 'label :master fr1
				  :font "Takaoゴシック 14 normal"))
	   (start-btn (make-instance 'button :master f :text "スタート")))
      (pack f)
      (pack fr1)
      (pack start-btn)
      (pack gamen)
      (setf (command start-btn)
	    (lambda ()
	      (game-start gamen))))))
    


;;プレイヤーの場所更新
(defun update-player-pos (p x y)
  (setf (player-posy p) (+ (player-posy p) y)
        (player-posx p) (+ (player-posx p) x)))
;;マップ設定
(defun set-map (map p moto)
  (loop for i from 0 below (donjon-tate map) do
    (loop for j from 0 below (donjon-yoko map) do
      (if (= (aref moto i j) 1)
	  (setf (player-posx p) j
		(player-posy p) i))
      (setf (aref (donjon-map map) i j) (aref moto i j)))))

;;移動後のマップ更新
(defun update-map (map p y x)
  (case (aref (donjon-map map) (+ (player-posy p) y) (+ (player-posx p) x))
    (30 ;;壁
     )
    (otherwise
     (update-player-pos p x y))))
     


