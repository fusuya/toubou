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

;;--------------------------------------------------------------------------------
;;AI移動
;;-------------A-star--------------------------------------------
(defstruct node
  (pos nil)
  (stable 0)
  (id 0)
  (g 0)
  (h 0)
  (f 0)
  (ham 0)
  (parent nil))

#|
CL-USER 10 > (minimum '((a 1) (b -1) (c -2)) #'< #'second)
(C -2)
|#
(defun minimum (list predicate key)
              (when list
                (let* ((m0 (first list))
                       (m1 (funcall key m0)))
                  (mapc (lambda (e0 &aux (e1 (funcall key e0)))
                          (when (funcall predicate e1 m1)
                            (psetf m0 e0 m1 e1)))
                        list)
                  m0)))

;;マンハッタン距離
(defun manhatan (pos goal)
  (+ (abs (- (car pos) (car goal))) (abs (- (cadr pos) (cadr goal)))))
;;ノードから道順をとり出す
(defun node-pick-pos (end pos-l)
  (if (null (node-parent end))
      pos-l
      (node-pick-pos (node-parent end) (cons (node-pos end) pos-l))))

(defun astar (start goal blocks)
  (let ((open (list (make-node :pos start :g 0 :h (manhatan start goal)
			       :f (manhatan start goal))))
	(close nil))
    (loop for i from 0 do
      (if (null open)
	  (return  "hoge"))
      (let ((n (minimum open #'< #'(lambda (x) (node-f x)))))
	(setf open (remove n open :test #'equalp))
	(push n close)
	(if (equal (node-pos n) goal)
	    (return (values (node-pick-pos n '()))))
	(setf (node-g n) (- (node-f n) (node-h n)))
	(loop for v in '((1 0) (0 1) (-1 0) (0 -1)) do
	  (let* ((next (mapcar #'+ (node-pos n) v)))
	    (if (not (find next blocks :test #'equal))
		(let ((m (find next open :test #'equal :key #'(lambda (x) (node-pos x))))
		      (dist 1))
		  (if m
		      (if (> (node-f m) (+ (node-g n) (node-h m) dist))
			  (setf (node-f m) (+ (node-g n) (node-h m) dist)
				(node-parent m) n))
		      (progn
			(setf m (find next close :test #'equal :key #'(lambda (x) (node-pos x))))
			(if m
			    (if (> (node-f m) (+ (node-g n) (node-h m) dist))
				(progn
				  (setf (node-f m) (+ (node-g n) (node-h m) dist)
					(node-parent m) n)
				  (setf close (remove m close :test #'equalp))
				  (push m open)))
			    (progn
			      (setf m (make-node))
			      (setf (node-pos m) next)
			      (setf (node-g m) dist)
			      (setf (node-h m) (manhatan next goal))
			      (setf (node-f m) (+ (node-g n) (node-h m) (node-g m)))
			      (setf (node-parent m) n)
			      (push m open)))))))))))))

;;移動方向を出力
(defun print-dir (now next)
  (let* ((diff (mapcar #'- next now)))
    (cond
      ((equal diff '(1 0))  (format nil "RIGHT"))
      ((equal diff '(-1 0)) (format nil "LEFT"))
      ((equal diff '(0 1))  (format nil "DOWN"))
      ((equal diff '(0 -1)) (format nil "UP"))
      (t (format nil "UP")))))

(defun lookup (key alist)
  (cdr (assoc key alist :test #'equal)))

;; (defparameter *alist-list* '((("ai-id" . 1) ("name" "もげ"))
;; 			     (("ai-id" . 2) ("name" "こげ"))))
  
  ;; (print
  ;;  (lookup "ai-id" '(("ai-id" 1))))
  ;; (print
  ;;  (find-if (lambda (alist) (= 1 (lookup "ai-id" alist))) *alist-list*))
(defun make-paths (pos p-pos other-ais blocks)
  (dolist (ai other-ais)
    (alist-bind
     (pos) ai
     (alist-bind
      (x y) pos
      (loop for m in '((0 0) (1 0) (0 1) (-1 0) (0 -1)) do
	(push (mapcar #'+ (list x y) m) blocks)))))
  (let ((path (astar pos p-pos blocks)))
    (if (listp path)
	(print-dir pos (car path))
	(format nil "UP"))))

;;移動モード
(defun map-mode (data id)
	      
  (let* ((blocks  (lookup "blocks" data))
	 (player  (lookup "player" data))
	 (p-posxy (lookup "pos" player))
	 (p-posx  (lookup "x"   p-posxy))
	 (p-posy  (lookup "y"   p-posxy))
	 (p-pos   (list p-posx p-posy))
	 (ais     (lookup "ais"    data))
	 (my-info (find-if (lambda (alist) (= id (lookup "id" alist))) ais))
	 (other-ais (remove my-info ais :test #'equal))
	 (posxy   (lookup "pos" my-info))
	 (posx    (lookup "x"   posxy))
	 (posy    (lookup "y"   posxy))
	 (pos     (list posx posy)))
    (make-paths pos p-pos other-ais blocks)))
    
