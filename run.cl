;;; vim: sts=2 sw=2 et sm lisp :

;; TODO: get a name
;; TODO: use a package
;; TODO: make this moddable

(declaim (optimize (debug 3)))

;;; QUICKLOADS
(ql:quickload :cl-liballegro)
(require "cffi")
(unless (and (al:init)
             (al:install-keyboard))
  (princ "ERROR: failed to init Allegro")
  (fresh-line)
  (exit))

;;; GLOBALS
(defparameter *pieces* '())
(defparameter *player* nil)

;;; required structs for Allegro
(cffi:defcstruct keyboard-state
   (display :pointer)
   (__key_down__internal__ :int :count 8)) ; ceil 227/32

(defmacro key-down-p (keyboard-state code)
  `(al:key-down ,keyboard-state
                (cffi:foreign-enum-value 'al::keycodes ,code)))

;;; PIECE CODE
;; TODO: pick some better weights
(defconstant +world-weight+ 3)
(defconstant +ship-weight+  2)
(defconstant +light-weight+ 1)

(defclass general-piece ()
  ((position   :documentation "")
   (pattern    :documentation "")
   (color      :documentation "")
   (weight     :documentation "")))

(defgeneric pieces-collide-p (p1 p2 dx dy)
            (:documentation ""))
(defgeneric point-collides-p (point this)
            (:documentation ""))

(defgeneric can-move-piece-by (this dx dy ignorelist stoplist)
            (:documentation ""))
(defgeneric now-move-piece-by (this dx dy ignorelist)
            (:documentation ""))
(defgeneric try-move-piece-by (this dx dy ignorelist stoplist)
            (:documentation ""))

(defgeneric tick-piece (this)
            (:documentation ""))

(defun piece-collides-with-any (piece dx dy ignorelist)
  (let ((retval nil))
    (dolist (other *pieces*)
      (unless (eql other piece)
        (unless (member other ignorelist)
          (when (pieces-collide-p piece other dx dy)
            (push other retval)))))
    retval))

(defun point-collides-with-any (point ignorelist)
  (let ((retval nil))
    (dolist (other *pieces*)
      (unless (member other ignorelist)
        (when (point-collides-p point other)
          (push other retval))))
    retval))

(defmethod pieces-collide-p ((p1 general-piece) (p2 general-piece) dx dy)
  (let* ((p1-position   (slot-value p1 'position))
         (p2-position   (slot-value p2 'position))
         (p1-pattern    (slot-value p1 'pattern ))
         (p2-pattern    (slot-value p2 'pattern ))
         (p1-xpos       (+ (nth 0 p1-position) dx))
         (p1-ypos       (+ (nth 1 p1-position) dy))
         (p2-xpos       (nth 0 p2-position))
         (p2-ypos       (nth 1 p2-position))
         (xoffs         (- p2-xpos p1-xpos))
         (yoffs         (- p2-ypos p1-ypos))
         (p1-xlen       (array-dimension p1-pattern 1))
         (p1-ylen       (array-dimension p1-pattern 0))
         (p2-xlen       (array-dimension p2-pattern 1))
         (p2-ylen       (array-dimension p2-pattern 0))
         (x1-min        (max 0 xoffs))
         (y1-min        (max 0 yoffs))
         (x2-min        (max 0 (- xoffs)))
         (y2-min        (max 0 (- yoffs)))
         (x1-max        (min p1-xlen (+ p2-xlen xoffs)))
         (y1-max        (min p1-ylen (+ p2-ylen yoffs)))
         (retval        nil))
    (do ((y1 y1-min (+ y1 1))
         (y2 y2-min (+ y2 1)))
      ((>= y1 y1-max))
      (do ((x1 x1-min (+ x1 1))
           (x2 x2-min (+ x2 1)))
        ((>= x1 x1-max))
        (cond ((eql (aref p1-pattern y1 x1) #\ ) nil)
              ((eql (aref p2-pattern y2 x2) #\ ) nil)
              (t (setf retval t)))))
    retval))

(defmethod point-collides-p (point (this general-piece))
  (let* ((this-position   (slot-value this 'position))
         (this-pattern    (slot-value this 'pattern ))
         (this-xpos       (nth 0 this-position))
         (this-ypos       (nth 1 this-position))
         (point-xpos      (nth 0 point))
         (point-ypos      (nth 1 point))
         (this-xlen       (array-dimension this-pattern 1))
         (this-ylen       (array-dimension this-pattern 0))
         (inner-x         (- point-xpos this-xpos))
         (inner-y         (- point-ypos this-ypos)))
    (cond ((<  inner-x 0) nil)
          ((<  inner-y 0) nil)
          ((>= inner-x this-xlen) nil)
          ((>= inner-y this-ylen) nil)
          ((eql (aref this-pattern inner-y inner-x) #\ ) nil)
          (t t))))


(defmethod now-move-piece-by ((this general-piece) dx dy ignorelist)
  (with-slots (position) this
    (incf (nth 0 position) dx)
    (incf (nth 1 position) dy)
    (let* ((collisions (piece-collides-with-any this 0 0 ignorelist))
           (new-ignores (list this)))
      (dolist (other collisions)
        (unless (member other ignorelist)
          (setf new-ignores 
                (now-move-piece-by
                  other dx dy
                  (append collisions new-ignores ignorelist)))
          (setf new-ignores (cons other new-ignores))))
      new-ignores)))


(defmethod can-move-piece-by ((this general-piece) dx dy ignorelist stoplist)
  (with-slots (position weight) this
    (let* ((collisions  (piece-collides-with-any this dx dy ignorelist))
           (new-ignores `(,this))
           (retval      t))
      (dolist (other collisions)
        (when retval  ; TODO: throw instead of drain
          (unless (member other ignorelist)
            (unless (and (not (member other stoplist))
                         (>= weight (slot-value other 'weight))
                         (can-move-piece-by
                           other dx dy
                           (append collisions new-ignores ignorelist)
                           stoplist))
              (setf retval nil))
            (setf new-ignores (cons other new-ignores)))))
      retval)))


(defmethod try-move-piece-by ((this general-piece) dx dy ignorelist stoplist)
  (if (can-move-piece-by this dx dy ignorelist stoplist)
    (progn
      (now-move-piece-by this dx dy ignorelist)
      t)
    nil))


(defun force-piece (this source dx dy)
  (with-slots ((this-weight weight)) this
    (with-slots ((source-weight weight)) source
      (unless (and (<= this-weight source-weight)
                   (try-move-piece-by this dx dy `() `(,source)))
        (when (>= this-weight source-weight)
          (try-move-piece-by source (- dx) (- dy) `() `(,this))
          )))))


(defmethod tick-piece ((this general-piece))
  (with-slots (position pattern) this
    (let* ((piece-x   (nth 0 position))
           (piece-y   (nth 1 position))
           (force-set '()))
      (macrolet ((force+ (s-force-set this dx dy rx ry)
                   (let* ((s-piece (gensym)))
                     `(let* ((collisions (point-collides-with-any
                                           (list ,rx ,ry)
                                           (list ,this))))
                        (dolist (,s-piece collisions)
                          (unless (assoc ,s-piece ,s-force-set)
                            (push (list ,s-piece 0 0) ,s-force-set))
                          (incf (second (assoc ,s-piece ,s-force-set)) ,dx)
                          (incf (third  (assoc ,s-piece ,s-force-set)) ,dy))))))
        (dotimes (sy (array-dimension pattern 0))
          (dotimes (sx (array-dimension pattern 1))
            (let* ((c     (aref pattern sy sx))
                   (rx    (+ piece-x sx))
                   (ry    (+ piece-y sy)))
              (cond ((eql c #\r)  ; Rotate clockwise
                     (force+ force-set this -1  0 (+ rx  0) (+ ry  1))
                     (force+ force-set this  0 -1 (+ rx -1) (+ ry  0))
                     (force+ force-set this  1  0 (+ rx  0) (+ ry -1))
                     (force+ force-set this  0  1 (+ rx  1) (+ ry  0)))
                    ((eql c #\R)  ; Rotate anticlockwise
                     (force+ force-set this  1  0 (+ rx  0) (+ ry  1))
                     (force+ force-set this  0  1 (+ rx -1) (+ ry  0))
                     (force+ force-set this -1  0 (+ rx  0) (+ ry -1))
                     (force+ force-set this  0 -1 (+ rx  1) (+ ry  0)))
                    (t t)))))
        (dolist (force force-set)
          (destructuring-bind (piece dx dy) force
            (force-piece piece this
                         (max -1 (min 1 dx))
                         (max -1 (min 1 dy)))))))))


(defun make-general-piece (&key position pattern color weight)
  (let ((this (make-instance 'general-piece)))
    (setf (slot-value this 'position) position)
    (setf (slot-value this 'pattern ) pattern )
    (setf (slot-value this 'color   ) color   )
    (setf (slot-value this 'weight  ) weight  )
    this))

(defmacro add-piece (x-position y-position parts
                     &key
                     (color (al:map-rgb-f 1.0 1.0 1.0))
                     (weight +light-weight+))
  (let* ((s-piece (gensym)))
    ;
    `(let ((,s-piece
             (make-general-piece
               :position '(,x-position ,y-position)
               :pattern   (make-array (list ,(length      parts )
                                            ,(length (car parts)))
                             :initial-contents ',(mapcar
                                                   #'(lambda (row)
                                                       (coerce row 'list))
                                                   parts))
               :color    ,color
               :weight   ,weight)))
       (push ,s-piece *pieces*)
       ,s-piece)))

;;; STATE DEFINITIONS
(add-piece 0 1 ("#############"
                "####        #"
                "            #"
                "#     #######"
                "#  #  #      "
                "   #         "
                "#######      ")
           :color (al:map-rgb-f 0.5 0.5 0.5)
           :weight +world-weight+)
(add-piece -1 6 ("###    ###"
                 "#        #"
                 "##########"
                 "   # #    ")
           :color (al:map-rgb-f 0.8 0.8 0.5)
           :weight +light-weight+)
(add-piece 0 2 ("         #"
                "##########")
           :color (al:map-rgb-f 0.8 0.8 0.8)
           :weight +light-weight+)
(add-piece 4 4 ("R"
                "#")
           :color (al:map-rgb-f 0.8 0.8 0.8)
           :weight +light-weight+)
(add-piece 1 5 ("r"
                "#")
           :color (al:map-rgb-f 0.8 0.8 0.8)
           :weight +light-weight+)

(setf *player* (add-piece 0 10 ("@")
                          :color (al:map-rgb-f 1.0 1.0 0.0)
                          :weight +ship-weight+))

;;; INITIALISATION
(defparameter *display* (al:create-display 1280 720))

;;; PIECE STUFF

;;; MAIN LOOP
(defun tick-game ()
  (al:clear-to-color (al:map-rgb 0 0 85))
  (dolist (piece (reverse *pieces*))
    (with-slots (position pattern color) piece
      (let* ((x-pos           (first  position))
             (y-pos           (second position))
             (x-len           (array-dimension pattern 1))
             (y-len           (array-dimension pattern 0))
             (player-position (slot-value *player* 'position))
             (x-cam           (- 1280/2 (* 1 32 (nth 0 player-position))))
             (y-cam           (-  720/2 (* 1 32 (nth 1 player-position)))))
        (dotimes (y-index y-len)
          (dotimes (x-index x-len)
            (ecase (aref pattern y-index x-index)
              ((#\ ) t)
              ((#\@) (al:draw-filled-rectangle
                       (+ x-cam (* (+ x-pos x-index 0) 32)  4)
                       (+ y-cam (* (+ y-pos y-index 0) 32)  4)
                       (+ x-cam (* (+ x-pos x-index 1) 32) -4)
                       (+ y-cam (* (+ y-pos y-index 1) 32) -4)
                       color)
                     (al:draw-filled-rectangle
                       (+ x-cam (* (+ x-pos x-index 0) 32)  8)
                       (+ y-cam (* (+ y-pos y-index 0) 32)  8)
                       (+ x-cam (* (+ x-pos x-index 0) 32) 12)
                       (+ y-cam (* (+ y-pos y-index 0) 32) 12)
                       (al:map-rgb-f 0.0 0.0 0.0))
                     (al:draw-filled-rectangle
                       (+ x-cam (* (+ x-pos x-index 1) 32) -12)
                       (+ y-cam (* (+ y-pos y-index 0) 32)   8)
                       (+ x-cam (* (+ x-pos x-index 1) 32)  -8)
                       (+ y-cam (* (+ y-pos y-index 0) 32)  12)
                       (al:map-rgb-f 0.0 0.0 0.0))
                     )
              ((#\R) (al:draw-filled-circle
                       (+ x-cam (* (+ x-pos x-index 1/2) 32))
                       (+ y-cam (* (+ y-pos y-index 1/2) 32))
                       32/2
                       color))
              ((#\r) (al:draw-filled-circle
                       (+ x-cam (* (+ x-pos x-index 1/2) 32))
                       (+ y-cam (* (+ y-pos y-index 1/2) 32))
                       32/2
                       color))
              ((#\#) (al:draw-filled-rectangle
                       (+ x-cam (* (+ x-pos x-index) 32))
                       (+ y-cam (* (+ y-pos y-index) 32))
                       (+ x-cam (* (+ x-pos x-index 1) 32) -1)
                       (+ y-cam (* (+ y-pos y-index 1) 32) -1)
                       color))))))))

  (al:flip-display))

(cffi:with-foreign-object (keyboard-state '(:struct keyboard-state))
  (al:get-keyboard-state keyboard-state)
  (do () ((key-down-p keyboard-state :escape))
    ;; Player movement
    (let* ((dx 0)
           (dy 0))
      (when (key-down-p keyboard-state :up)    (incf dy -1))
      (when (key-down-p keyboard-state :down)  (incf dy  1))
      (when (key-down-p keyboard-state :left)  (incf dx -1))
      (when (key-down-p keyboard-state :right) (incf dx  1))
      (when (or (/= dx 0)
                (/= dy 0))
        (try-move-piece-by *player* dx dy '() '())))

    ;; Per-piece applications
    (dolist (this *pieces*)
      (tick-piece this))

    ;; Other stuff
    (tick-game)
    (sleep 0.1)
    (al:get-keyboard-state keyboard-state)))

;;; EXIT
(exit)

