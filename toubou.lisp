(load "ai-ido.lisp")

;; ゲームの状態。
(defstruct game
  (donjon (make-donjon)) ;; マップ
  (players nil)          ;; プレーヤーのリスト
  (hunters nil)          ;; ハンターのリスト
  (turn 1))              ;; 何ターン目か

(defparameter *map*
  #2A((30 30 30 30 30 30 30 30 30 30 30 30 30)
      (30  0  0  0  0  0  0  0  0  0  0  0 30)
      (30  0 30 30  0  0 30  0  0 30 30  0 30)
      (30  0 30  0  0  0  0  0  0  0 30  0 30)
      (30  0  0  0 30 30  0 30 30  0  0  0 30)
      (30  0  0  0 30  0  0  0 30  0  0  0 30)
      (30  0 30  0  0  0  5  0  0  0 30  0 30)
      (30  0  0  0 30  0  0  0 30  0  0  0 30)
      (30  0  0  0 30 30  0 30 30  0  0  0 30)
      (30  0 30  0  0  0  0  0  0  0 30  0 30)
      (30  0 30 30  0  0 30  0  0 30 30  0 30)
      (30  0  0  0  0  0  0  0  0  0  0  0 30)
      (30 30 30 30 30 30 30 30 30 30 30 30 30)))

;; プレーヤーの初期位置。5人目が参加するときっとクラッシュする。
(defparameter *player-id-pos* '((1 1) (11 1) (1 11) (11 11)))

(defconstant +client-read-timeout+ 10)
(defconstant +registration-timeout+ 30) ;;一人目の参加から何秒でゲームを開始するか。

(defstruct actor
  (posy 1)
  (posx 1)
  (name nil)
  (atama nil))

(defstruct (hunter (:include actor))
  (id nil))

(defstruct (player (:include actor))
  (last-turn-alive nil)
  (command nil)
  (id nil))

(defstruct (remote-player (:include player))
  (socket nil)
  (stream nil))

(defstruct donjon
  (map nil)  ;;マップ
  (tate 13)  ;;縦幅
  (yoko 13)) ;;横幅

;;---------------------------------------------------------------------------------------

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

;;プレイヤーの生死判定
(defun player-dead? (p)
  (if (player-last-turn-alive p) t nil))

;;n内の１以上の乱数
(defun randval (n)
  (1+ (random (max 1 n))))

;;---------------------------------------------------------------------------------------

;;マップのセルに設定される整数を文字にする。
(defun map-type (num)
  (case num
    (30 "ロ") ;; 壁
    (0  "　")
    (1  "主") ;; プレイヤーの位置
    (5  "巣") ;; 巣
    ))

(defun json-true-false (v)
  (if v t :false))

(defun player-list (p)
  `(:|id| ,(player-id p)
    :|name| ,(player-name p)
    :|atama| ,(player-atama p)
    :|pos| (:|x| ,(player-posx p) :|y| ,(player-posy p))
    :|dead| ,(json-true-false (player-dead? p))))

(defun map-data-list (map)
  (let ((blocks nil)
        (su nil))
    (loop for i from 0 below (donjon-tate map) do
          (loop for j from 0 below (donjon-yoko map) do
                (case (aref (donjon-map map) i j)
                  (30 (push (list j i) blocks))

                  (5  (push (list j i) su)))))
    `(:|blocks| ,blocks :|su| ,su)))

(defun make-error-message (&key reason message)
  `(:|type| "error" :|reason| ,reason :|message| ,message))

(defun make-hunters-data (hunters)
  (list :|hunters|
        (loop for h in hunters
              collect `(:|id| ,(hunter-id h) :|pos| (:|x| ,(hunter-posx h) :|y| ,(hunter-posy h))))))

(defun make-map-data (g)
  (append `(:|type| "map")
          `(:|turn| ,(game-turn g))
          (map-data-list (game-donjon g))
          `(:|players| ,(mapcar #'player-list (game-players g)))
          (make-hunters-data (game-hunters g))))

;;敵と重なったか
(defun encount-enemy (p ai-list)
  (dolist (ai ai-list)
    (if (and (= (actor-posx p) (actor-posx ai))
             (= (actor-posy p) (actor-posy ai)))
        (return-from encount-enemy t)))
  nil)

;;マップ表示
(defmethod game-show ((g game) out)
  (labels ((show-turn
            ()
            (format out "ターン:~d~%" (game-turn g)))
           (show-map
            ()
            (let ((actors (append (game-players g) (game-hunters g))))
              (loop for i from 0 below (donjon-tate (game-donjon g))
                    do
                    (loop for j from 0 below (donjon-yoko (game-donjon g))
                          do
                          (let ((actor (find-if #'(lambda (a) (and (= i (actor-posy a)) (= j (actor-posx a))))
                                                actors)))
                            (if actor
                                (format out
                                        (if (actor-dead? actor) "墓" (actor-atama actor)))
                              (format out (map-type (aref (donjon-map (game-donjon g)) i j))))))
                    (fresh-line out))))
           (format-command
            (command)
            (if command command ""))
           (show-players
            ()
            (format out "プレーヤー:~%")
            (dolist (p (game-players g))
              (format out "~a ~a ~a~%" (player-id p) (player-name p) (format-command (player-command p))))))
    (show-turn)
    (show-map)
    (show-players)))

(defun update-ais (hunter-list map-data map)
  (dolist (hunter hunter-list)
    (let ((dir (map-mode (jonathan:parse (jonathan:to-json map-data) :as :alist) (hunter-id hunter))))
      (cond
        ((equal dir "UP")    (update-map map hunter -1 0))
        ((equal dir "DOWN")  (update-map map hunter 1 0))
        ((equal dir "RIGHT") (update-map map hunter 0 1))
        ((equal dir "LEFT")  (update-map map hunter 0 -1))
        (t
         (error (format nil "マップモードで無効なコマンド: ~s (~a)" dir (hunter-name hunter))))))))

;; プレーヤーを参加させる。ここでIDを割り当てる。
(defun game-add-player (g player)
  (let* ((id (length (game-players g)))
         (pos (nth id *player-id-pos*)))
    (setf (player-id player) id)
    (setf (player-posx player) (first pos)
          (player-posy player) (second pos))
    (setf (game-players g) (append (game-players g) (list player)))))

;; リモートプレーヤーとの接続を切る。ゲーム終了時の後処理。
(defun game-close-connections (g)
  (dolist (rp (game-remote-players g))
    (remote-player-close-stream rp)))

;; 敵とプレーヤーの接触判定。当たると死ぬ。
(defun game-encount (g)
  (dolist (p (game-players g))
    (if (and (not (player-dead? p))
             (encount-enemy p (game-hunters g)))
        (game-kill-player g p))))

;; プレーヤーが動く。全てのプレーヤーのコマンドが入力されてから呼び出
;; す。
(defun game-move-players (g)
  (dolist (p (game-players g))
    (when (not (player-dead? p))
      (cond
       ((equal (player-command p) "UP")    (update-map (game-donjon g) p -1 0))
       ((equal (player-command p) "DOWN")  (update-map (game-donjon g) p 1 0))
       ((equal (player-command p) "RIGHT") (update-map (game-donjon g) p 0 1))
       ((equal (player-command p) "LEFT")  (update-map (game-donjon g) p 0 -1))))))
;; ハンターが動く。
(defun game-move-hunters (g)
  (let ((map-data (make-map-data g)))
    ;;敵移動
    (update-ais (game-hunters g) map-data (game-donjon g))))
;; ハンターが巣から出てくる。
(defun game-add-hunter (g)
  (push (make-hunter :id (length (game-hunters g)) :posx 6 :posy 6
                     :name "ハンター" :atama "ハ")
        (game-hunters g)))

(defun make-server-socket ()
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp))
        (addr (make-inet-address "0.0.0.0")))
    (setf (sockopt-reuse-address s) t)
    (setf (non-blocking-mode s) t)
    (socket-bind s addr 12121)
    (socket-listen s 5)
    (v:info :server "~aで接続受け付け開始。" (socket-name-string s))
    s))

(defun chomp (line)
  (let ((last-char-pos
         (position-if (lambda (c) (and (not (equal c #\Return)) (not (equal c #\Linefeed))))
                      line :from-end t)))
    (if last-char-pos
        (subseq line 0 (1+ last-char-pos))
      "")))

(defun valid-command? (str)
  (or
   (equal "LEFT" str)
   (equal "RIGHT" str)
   (equal "UP" str)
   (equal "DOWN" str)
   (equal "STAY" str)))

(defmethod remote-player-close-stream ((rp remote-player))
  (handler-case
   (if (remote-player-stream rp)
       (progn
         (close (remote-player-stream rp))
         (setf (remote-player-stream rp) nil))
     (v:info :network "プレーヤー~aの存在しないストリームを閉じようとしました。"
             (player-name rp)))
   (sb-int:simple-stream-error
    (c)
    (declare (ignore c))

    (v:error :network "~aとの接続をクローズ時にストリームエラー。" (player-name rp)))))

;; handshake-error が発生する。
;; (defmethod remote-player-receive-name (rp)
;;   (handler-case
;;    (let ((name (chomp (read-line (remote-player-stream rp)))))
;;      (when (equal name "")
;;        (v:error :network "AIの名前が空です。")
;;        (error 'handshake-error))
;;      (setf (player-name rp) name
;;            (player-atama rp) (atama-of name)))
;;    (end-of-file (c)
;;                 (v:error :network "~A" c)
;;      (error 'handshake-error))))

(defmethod remote-player-receive-name (rp)
  (let ((len 0) (product nil) (name ""))
    (flet ((make-res-string (len product)
             (babel:octets-to-string
              (make-array len :element-type '(unsigned-byte 8) :initial-contents (nreverse product))
              :encoding :utf-8)))
      (handler-case
	  (let ((byte (read-byte (remote-player-stream rp) nil nil)))
	    (loop until (or (null byte) (= byte 10)) do
	      (push byte product)
	      (incf len)
	      (setf byte (read-byte (remote-player-stream rp) nil nil)))
	    ;;(format t "~S~%" product)
	    
	    (setf name (make-res-string len product))
	    (when (equal name "")
	      (v:error :network "AIの名前が空です。")
	      (error 'handshake-error))
	    (setf (player-name rp) name
		  (player-atama rp) (atama-of name)))
	(end-of-file (c)
	  (v:error :network "~A" c)
	  (error 'handshake-error))))))

(defmethod remote-player-send-id (player)
  (format (remote-player-stream player) "~a~%" (player-id player))
  (finish-output (remote-player-stream player)))

(defmethod remote-player-send-message ((rp remote-player) data)
  (when (remote-player-stream rp)
  (let ((json (jonathan:to-json data)))
      (format (remote-player-stream rp) "~a~%" json)
      (finish-output (remote-player-stream rp)))))

(defun remote-player-send-name-error (rp)
  (format (remote-player-stream rp) "change-name~%")
  (finish-output (remote-player-stream rp)))

;;名前の頭文字がかぶってないかチェック
(defun check-atama (g player)
  (dolist (p (game-players g))
    (when (equal (player-atama p) (player-atama player))
      (return-from check-atama t)))
  nil)

;; リモートプレーヤーから届いているコマンドを受け取る。
(defun try-read-remote-commands (g)
  (dolist (rp (game-remote-players g))
    (when (and (not (player-command rp))
               (listen (remote-player-stream rp)))
      ;; 読み込めるデータがある。1行全て読み込める
      ;; とは限らないが…。
      (v:debug :game "プレーヤー~aからコマンドの読み込み開始。" (player-name rp))
      (let ((cmd (chomp (read-line (remote-player-stream rp)))))
        (v:debug :game "プレーヤー~aからコマンドの読み込み完了。~a" (player-name rp) cmd)
        (if (valid-command? cmd)
            (progn
              (when (player-command rp)
                (v:warn :game "~aのコマンドは既に~aに設定されている。" (player-command rp)))
              (setf (player-command rp) cmd))
          (progn
            (when (not (player-dead? rp))
              (v:info :game "プレーヤー「~s」は不正なコマンド「~s」により反則負け。" (player-name rp) cmd)
              (remote-player-send-message rp (make-error-message :reason "protocol-error" :message (format nil "不正なコマンド~s" cmd)))
              (remote-player-close-stream rp)
              (game-kill-player g rp))))))))

;;リモートプレーヤーにゲーム状態のJSONを送る。
(defun game-broadcast-map (g)
  (game-broadcast-message g (make-map-data g)))

(defun socket-name-string (sock)
  (multiple-value-bind (addr port)
      (socket-name sock)
    (format nil "~a.~a.~a.~a:~a"
            (aref addr 0)
            (aref addr 1)
            (aref addr 2)
            (aref addr 3)
            port)))

(defun socket-peername-string (sock)
  (multiple-value-bind (addr port)
      (socket-peername sock)
    (format nil "~a.~a.~a.~a:~a"
            (aref addr 0)
            (aref addr 1)
            (aref addr 2)
            (aref addr 3)
            port)))

(defun new-game ()
  (let ((first-hunter (make-hunter :id 0 :name "ハンター" :posx 6 :posy 6 :atama "ハ")))
    (make-game :donjon (make-donjon :map *map*)
               :hunters (list first-hunter))))

(defun game-broadcast-message (g data)
  (dolist (rp (game-remote-players g))
    (handler-case
     (remote-player-send-message rp data)

     (sb-int:simple-stream-error
      (c)
      (declare (ignore c))

      (v:error :network "~aへのメッセージ送信時にストリームエラー。" (player-name rp))))))

(defun make-ranking-data (g)
  (flet ((ranking-item
          (p)
          `(:|name| ,(player-name p) :|score| ,(player-last-turn-alive p))
          ))
    (mapcar #'ranking-item
            (sort (game-players g) #'> :key #'player-last-turn-alive))))

(defun game-broadcast-result (g)
  (game-broadcast-message
   g
   `(:|type| "result"
      :|ranking| ,(make-ranking-data g))))

(defun game-broadcast-status (g timeout-seconds)
  (game-broadcast-message g `(:|type| "status" :|timeout-seconds| ,timeout-seconds :|map| ,(make-map-data g))))

;;頭文字が被ってたときに送るやつ
(defun game-broadcast-change-name (g rp timeout-seconds)
  (handler-case
      (remote-player-send-message rp `(:|type| "change-name" :|timeout-seconds| ,timeout-seconds :|map| ,(make-map-data g)))
  (sb-int:simple-stream-error
      (c)
      (declare (ignore c))

      (v:error :network "~aへのメッセージ送信時にストリームエラー。" (player-name rp)))))

  
(defun game-end? (g)
  (every #'player-dead? (game-players g)))

(defun game-kill-player (g p)
  (setf (player-last-turn-alive p) (game-turn g)))

(defun game-remote-players (g)
  (remove-if-not #'remote-player-p (game-players g)))

(defun server-main ()
  (let* ((server-socket (make-server-socket))
         (g (new-game))
         ;; app-func:
         ;;     player-registration プレーヤー登録受け付け中。
         ;;     playing ゲームプレイ中。
         (app-func nil)
         ;; first-registration-time: 最初の参加者が登録した時刻。
         (first-registration-time nil)
         (turn-start-time nil))

    (labels
        ((player-registration
          ()

          ;; 1. プレーヤーの登録処理。
          (let ((client (socket-accept server-socket)))
            (when client
              (v:info :network "クライアントから接続: ~a" (socket-peername-string client))
              ;; クライアントからの接続がある。
              (let* ((stream (socket-make-stream client
                                                 :input t
                                                 :output t
                                                 :element-type :default
                                                 :timeout +client-read-timeout+))
                     (player (make-remote-player :stream stream :socket client)))
                (handler-case
                 (remote-player-receive-name player)
                 (handshake-error
                  (c)
                  (declare (ignore c))

                  (v:error :network "~aとのハンドシェイクに失敗。" (socket-peername-string client))
                  (remote-player-close-stream player)
                  (return-from player-registration))
                 (sb-sys:io-timeout
                  (c)
                  (declare (ignore c))

                  (v:error :network "~aとのハンドシェイク中にタイムアウト。" (socket-peername-string client))
                  (remote-player-close-stream player)
                  (return-from player-registration)))
                ;; XXX: 同名のプレーヤーは登録しない措置が必要か。
                (when (check-atama g player) ;;名前の頭文字がかぶってないか判定
                  (let ((timeout (- +registration-timeout+
                                    (truncate (- (get-internal-real-time) first-registration-time) internal-time-units-per-second))))
                    (game-broadcast-change-name g player timeout)
                    (return-from player-registration)))
                
                (game-add-player g player)
                (when (not first-registration-time)
                  (setf first-registration-time (get-internal-real-time)))
                (v:info :game "プレーヤー~a(ID: ~a)を登録。" (player-name player) (player-id player))
                (remote-player-send-id player)
                (let ((timeout (- +registration-timeout+ (truncate (- (get-internal-real-time) first-registration-time) internal-time-units-per-second))))
                  (game-broadcast-status g timeout)))))

          ;; 2. 最初の参加から30秒あるいは4人揃っていたらゲーム開始。
          (let ((sec (if first-registration-time
                         (truncate (- (get-internal-real-time) first-registration-time) internal-time-units-per-second)
                       0)))
            (when (or (>= sec +registration-timeout+)
                      (<= 4 (length (game-players g))))
              (setf app-func #'playing)
              (setf first-registration-time nil)
              (v:info :game "ゲーム開始。")
              (game-broadcast-map g))))

         (playing
          ()

          (when (not turn-start-time)
            (setf turn-start-time (get-internal-real-time)))

          (let ((seconds-elapsed (truncate (- (get-internal-real-time) turn-start-time)
                                           internal-time-units-per-second)))
            (when (>= seconds-elapsed +client-read-timeout+)
              (dolist (rp (game-remote-players g))
                (when (and (not (player-dead? rp))
                           (not (player-command rp)))
                  (game-kill-player g rp)
                  ;;(remote-player-close-stream rp)
                  (v:error :game "プレーヤー~aから~a秒以内にコマンドを受けとれなかったので死亡扱い。" (player-name rp) +client-read-timeout+)))))

          (if (game-end? g)
              (progn
                (v:info :game "ゲーム終了。~a" (make-ranking-data g))
                (game-broadcast-result g)
                (game-close-connections g)
                (setf g (new-game))
                (setf turn-start-time nil)
                (setf app-func #'player-registration))
            (progn
              (try-read-remote-commands g)
              ;; ゲーム状態の更新。
              (when (every #'player-command (remove-if #'player-dead? (game-players g)))
                (game-move-players g)
                (dolist (p (game-players g))
                  (setf (player-command p) nil)) ;; プレーヤーのコマンドをクリア.
                (game-encount g) ;; 敵につっこんで死亡。
                ;; 全員死んでる状態で敵AIを動かすとコケるので。
                (when (not (every #'player-dead? (game-players g)))
                  (game-move-hunters g)
                  (game-encount g)) ;; 敵がつっこんできて死亡。
                ;; 敵を湧かす？
                 (when (zerop (mod (game-turn g) 20))
                     (game-add-hunter g))
                (incf (game-turn g))
                (game-broadcast-map g)
                (setf turn-start-time (get-internal-real-time)))))))

      (setf *random-state* (make-random-state t))
      (setf app-func #'player-registration)

      ;;ループ
      (loop
       (funcall app-func)
       (sleep 0.05)))))

;;プレイヤーorハンターの場所更新
(defun update-actor-pos (p x y)
  (incf (actor-posy p) y)
  (incf (actor-posx p) x))

;;マップ設定
(defun set-map (map p moto)
  (loop for i from 0 below (donjon-tate map) do
    (loop for j from 0 below (donjon-yoko map) do
      (if (= (aref moto i j) 1)
	  (setf (player-posx p) j
		(player-posy p) i))
      (setf (aref (donjon-map map) i j) (aref moto i j)))))

;;移動後のマップ更新
(defun update-map (map actor y x)
  (case (aref (donjon-map map) (+ (actor-posy actor) y) (+ (actor-posx actor) x))
    (30 ;;壁
     )
    (otherwise
     (update-actor-pos actor x y))))
