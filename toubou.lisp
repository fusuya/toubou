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

(defstruct actor
  (posy 1)
  (posx 1)
  (dead? nil)
  (name nil)
  (atama nil))

(defstruct (hunter (:include actor))
  (id nil))

(defstruct (player (:include actor))
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
(defun player-dead (p)
  (player-dead? p))

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

(defun make-hunters-data (hunters)
  (list :|hunters|
        (loop for h in hunters
              collect `(:|id| ,(hunter-id h) :|pos| (:|x| ,(hunter-posx h) :|y| ,(hunter-posy h))))))

(defun make-map-data (g)
  (append `(:|type| "map")
          (map-data-list (game-donjon g))
          `(:|players| ,(mapcar (lambda (p) (player-list p)) (game-players g)))
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
  (labels ((show-turn () (format out "ターン:~d~%" (game-turn g)))
           (show-map ()
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
           (format-command (command)
                           (if command command ""))
           (show-players ()
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
  (dolist (rp (remove-if-not #'remote-player-p (game-players g)))
    (close (remote-player-stream rp))))
;; 敵とプレーヤーの接触判定。当たると死ぬ。
(defun game-encount (g)
  (dolist (p (game-players g))
    (if (and (not (player-dead? p))
             (encount-enemy p (game-hunters g)))
        (setf (player-dead? p) t))))
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
    (socket-bind s addr 9999)
    (socket-listen s 5)
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

(defmethod remote-player-receive-name (rp)
  (handler-case
   (let ((name (chomp (read-line (remote-player-stream rp)))))
     (when (equal name "")
       (format t "AIの名前が空です。~%")
       (error 'handshake-error))
     (setf (player-name rp) name
           (player-atama rp) (atama-of name)))
   (end-of-file (c)
                (format t "~A~%" c)
                (error 'handshake-error))))

;; リモートプレーヤーから届いているコマンドを受け取る。
(defun try-read-remote-commands (g)
  (dolist (rp (game-players g))
    (when (and (remote-player-p rp)
               (not (player-command rp))
               (listen (remote-player-stream rp)))
      ;; 読み込めるデータがある。1行全て読み込める
      ;; とは限らないが…。
      (let ((cmd (chomp (read-line (remote-player-stream rp)))))
        (if (valid-command? cmd)
            (setf (player-command rp) cmd)
          (progn
            (when (not (player-dead? rp))
              (do-msg (format nil "プレーヤー「~s」は不正なコマンド「~s」により反則負け。"
                              (player-name rp)
                              cmd))
              (setf (player-dead? rp) t))))))))
;;リモートプレーヤーにゲーム状態のJSONを送る。
(defun game-send-to-remote-players (g)
  (let ((json (jonathan:to-json (make-map-data g))))
    (dolist (rp (remove-if-not #'remote-player-p (game-players g)))
      (format (remote-player-stream rp) "~a~%" json)
      (force-output (remote-player-stream rp)))))

;;GUI
(defun gui-main ()
  (with-ltk
   ()

   (let* ((server-socket (make-server-socket))
          (local-player nil)
          (g nil)
          (app-state 'player-registration)
          ;;     player-registration プレーヤー登録受け付け中。
          ;;     playing ゲームプレイ中。
          ;;     finished ゲーム終了。

          ;; ウィジェット。
          (f (make-instance 'frame))
          (fr1 (make-instance 'labelframe :master f :text "画面"))
          (gamen  (make-instance 'label :master fr1
                                 :font "Takaoゴシック 14 normal"))
          (start-btn (make-instance 'button :master f :text "スタート"))
          (local-player-btn (make-instance 'button :master f :text "ローカルプレーヤー追加"))
          (reset-btn (make-instance 'button :master f :text "リセット")))

     (labels ((init-game () ;; ゲーム初期化。
                         (let ((first-hunter (make-hunter :id 0 :name "ハンター" :posx 6 :posy 6 :atama "ハ")))
                           (setf g
                                 (make-game :donjon (make-donjon :map *map*)
                                            :hunters (list first-hunter)))))
              (format-app-state (s)
                                (case s
                                  (player-registration "エントリー受け付け中")
                                  (playing "プレイ中")
                                  (finished "ゲーム終了")))
              (update-screen ()
                             (setf (text gamen)
                                   (let ((out (make-string-output-stream)))
                                     (format out "~a~%" (format-app-state app-state))
                                     (game-show g out)
                                     (get-output-stream-string out))))
              (iter ()
                    (case
                        app-state
                      (player-registration
                       ;; プレーヤーとの接続が切れたら除外とかしたいけど動かない…。
                       ;; (dolist (rp (remove-if-not #'remote-player-p (game-players g)))
                       ;;   (when (not (socket-open-p (remote-player-socket rp)))
                       ;;     (setf (game-players g) (remove rp (game-players g)))))
                       (let ((client (socket-accept server-socket)))
                         (when client
                           ;; クライアントからの接続がある。
                           (let* ((stream (socket-make-stream client :input t :output t :element-type 'character))
                                  (player (make-remote-player :stream stream :socket client)))
                             (remote-player-receive-name player)
                             ;; XXX: 同名のプレーヤーは登録しない措置が必要か。
                             (game-add-player g player)
                             ;; プレーヤーIDを返す。
                             (format (remote-player-stream player) "~a~%" (player-id player))
                             (force-output (remote-player-stream player)))) ))
                      (playing
                       (try-read-remote-commands g)
                       (if (every #'player-dead? (game-players g))
                           (progn
                             (setf app-state 'finished)
                             (game-close-connections g))
                         (progn
                           ;; ゲーム状態の更新。
                           (when (every (lambda (p) (player-command p)) (game-players g))
                             (game-move-players g)
                             (dolist (p (game-players g))
                               (setf (player-command p) nil)) ;; プレーヤーのコマンドをクリア.
                             (game-encount g) ;; 敵につっこんで死亡。
                             ;; 全員死んでる状態で敵AIを動かすとコケるので。
                             (when (not (every #'player-dead? (game-players g)))
                               (game-move-hunters g)
                               (game-encount g)) ;; 敵がつっこんできて死亡。
                             ;; 敵を湧かす？
                             ;; (when (zerop (mod (game-turn g) 20))
                             ;;     (game-add-hunter g))
                             (incf (game-turn g))
                             (game-send-to-remote-players g)))))
                      (finished
                       ))

                    ;; ゲーム状態の描画。
                    (update-screen)
                    (sleep 0.03)
                    (after-idle #'iter)))

       (setf *random-state* (make-random-state t))
       (init-game)

       (wm-title *tk* "GUI")
       (bind *tk* "<Alt-q>"
             (lambda (event)
               (declare (ignore event))
               (process-events)
               (return-from gui-main)))
       (set-geometry *tk* 600 600 200 200)
       (configure *tk* :padx 16 :pady 16)

       (pack f)
       (pack fr1)
       (pack start-btn)
       (pack gamen)
       (pack local-player-btn)
       (pack reset-btn)

       (setf (command start-btn)
             (lambda ()
               (cond
                ((not (eq 'player-registration app-state))
                 (do-msg "新しいゲームを開始する前にリセットしてね。"))
                ((zerop (length (game-players g)))
                 (do-msg "プレーヤーが居ないので始められません。"))
                (t
                 (setf app-state 'playing)
                 (game-send-to-remote-players g)))))
       (setf (command local-player-btn)
             (lambda ()
               (if (eq app-state 'player-registration)
                 (if (and local-player (find local-player (game-players g)))
                     (do-msg "もうローカルプレーヤーは参加しています。")
                   (progn
                     (setf local-player (make-player :name "ワンコン" :atama "＠"))
                     (game-add-player g local-player)))
                 (do-msg "今は参加を受け付けていません。"))))
       (setf (command reset-btn)
             (lambda ()
               (game-close-connections g)
               (init-game)
               (setf local-player nil
                     app-state 'player-registration)))

       ;;キー入力イベント
       (mapc (lambda (key cmd)
               (bind *tk* key
                     (lambda (event)
                       (declare (ignore event))
                       (when local-player
                         (setf (player-command local-player) cmd)))))
             '("<Right>" "<Left>" "<Up>" "<Down>" "<End>")
             '("RIGHT" "LEFT" "UP" "DOWN" "STAY"))

       ;;ループ
       (after-idle #'iter)
       (mainloop)))))

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
