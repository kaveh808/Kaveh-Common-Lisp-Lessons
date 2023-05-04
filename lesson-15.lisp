#|
Lesson 15 -- Flexible dynamics
|#

;;;; utils ==============================================================

(defmethod p-norm ((p point))
  (let ((mag (p-mag p)))
    (if (= 0.0 mag)
	(p! 0 0)
	(p! (/ (x p) mag) (/ (y p) mag)))))
      
;;;; flex-vertex ========================================================

(defparameter *damping* 0.95)

(defclass flex-vertex ()
  ((point :accessor point :initarg :point :initform nil)
   (springs :accessor springs :initarg :springs :initform '())
   (pinned? :accessor pinned? :initarg :pinned? :initform nil)
   (velocity :accessor velocity :initarg :velocity :initform (p! 0 0))
   (mass :accessor mass :initarg :mass :initform 1.0)
   (elasticity :accessor elasticity :initarg :elasticity :initform 0.75)
   (friction :accessor friction :initarg :friction :initform 0.75)))

;; compute force on vertex
(defmethod compute-dynamics ((vertex flex-vertex) collisions?)
  (let* ((external-force (p+ *gravity* *wind*))
	 (internal-force (reduce #'p+
				 (mapcar (lambda (s) (spring-force s vertex))
					 (springs vertex))))
	 (force (p+ external-force internal-force))                     ;compute force
	 (acc (p/ force (mass vertex)))	                                ;compute acceleration
	 (vel (p+ (velocity vertex) acc))		                ;compute velocity
	 (pos (p+ (point vertex) (p* vel *time-step*)))) ;compute position
    (when collisions?		;handle collision
      (let ((elast (elasticity vertex))
	    (friction (friction vertex))
	    (lo -1.0)			;(+ -1.0 *collision-padding*))
	    (hi  1.0))			;(- 1.0 *collision-padding*)))
	(when (< (x pos) lo)
	  (setf (x pos) (+ lo (abs (- lo (x pos)))))
	  (setf (x vel) (* elast (- (x vel))))
	  (setf (y vel) (* friction (y vel))))
	(when (> (x pos) hi)
	  (setf (x pos) (- hi (abs (- hi (x pos)))))
	  (setf (x vel) (* elast (- (x vel))))
	  (setf (y vel) (* friction (y vel))))
	(when (< (y pos) lo)
	  (setf (y pos) (+ lo (abs (- lo (y pos)))))
	  (setf (y vel) (* elast (- (y vel))))
	  (setf (x vel) (* friction (x vel))))
	(when (> (y pos) hi)
	  (setf (y pos) (- hi (abs (- hi (y pos)))))
	  (setf (y vel) (* elast (- (y vel))))
	  (setf (x vel) (* friction (x vel))))))
    ;; update state
    (setf (velocity vertex) (p* vel *damping*))
    (setf (point vertex) pos)))

;;;; flex-spring ========================================================

(defclass flex-spring ()
  ((vertex1 :accessor vertex1 :initarg :vertex1 :initform nil)
   (vertex2 :accessor vertex2 :initarg :vertex2 :initform nil)
   (stiffness :accessor stiffness :initarg :stiffness :initform 30.0)
   (rest-length :accessor rest-length :initarg :rest-length :initform 1.0)))

(defmethod init-spring ((spring flex-spring))
  (setf (rest-length spring) (p-dist (point (vertex1 spring)) (point (vertex2 spring)))))

(defmethod spring-force ((spring flex-spring) (vertex flex-vertex))
  (let ((len (p-dist (point (vertex1 spring)) (point (vertex2 spring))))
	(dir (p-norm (if (eq vertex (vertex1 spring))
			 (p- (point (vertex2 spring)) (point (vertex1 spring)))
			 (p- (point (vertex1 spring)) (point (vertex2 spring)))))))
    (p* dir (* (- len (rest-length spring)) (stiffness spring)))))

;;;; flex-animator ======================================================

;;; (defclass flex-polygon-shape (polygon-shape) ...?

(defclass flex-animator (animator)
  ((vertices :accessor vertices :initarg :vertices :initform '())
   (springs :accessor springs :initarg :springs :initform '())
   (show-pinned? :accessor show-pinned? :initarg :show-pinned? :initform t)
   (show-springs? :accessor show-springs? :initarg :show-springs? :initform t)
   (collisions? :accessor collisions? :initarg :collisions? :initform t)))

(defmethod add-spring ((anim flex-animator) (vertex1 flex-vertex) (vertex2 flex-vertex))
  (let* ((len (p-dist (point vertex1) (point vertex2)))
	 (spring (make-instance 'flex-spring :vertex1 vertex1 :vertex2 vertex2 :rest-length len)))
    (push spring (springs anim))
    (push spring (springs vertex1))
    (push spring (springs vertex2))
    spring))

(defmethod init-anim :after ((anim flex-animator) (shape shape))
  )

(defmethod init-anim :after ((anim flex-animator) (shape polygon-shape))
  (setf (vertices anim) (mapcar (lambda (p) (make-instance 'flex-vertex :point p))
				(points shape)))
  (dotimes (i (length (vertices anim)))
    (dotimes (j (length (vertices anim)))
      (when (> i j)
	(add-spring anim (nth i (vertices anim)) (nth j (vertices anim)))))))

;;; >>> do standard update method, then do dynamics in :after method
(defmethod update-anim :after ((anim flex-animator) (shape shape))
  )

(defmethod update-anim :after ((anim flex-animator) (shape polygon-shape))
  ;; get points
  (loop for p in (points shape)
	for v in (vertices anim)
	do (setf (point v) p))
  ;; compute dynamics
  (dolist (v (vertices anim))
    (when (not (pinned? v))
      (compute-dynamics v (collisions? anim))))
  ;; set points
  (setf (points shape) (mapcar #'point (vertices anim))))

(defmethod make-flex ((shape shape))
  (setf (animator shape) (make-instance 'flex-animator))
  (init-anim (animator shape) shape)
  shape)

(defmethod make-flex ((group group))
  (do-hierarchy group
    (lambda (s) (make-flex s))
    :test #'is-leaf?)
  group)

(defmethod set-flex-vertex-attr ((group group) attr val &optional (indices '()))
  (do-hierarchy group
    (lambda (s) (set-flex-vertex-attr (animator s) attr val indices))
    :test #'is-leaf?)
  group)

(defmethod set-flex-vertex-attr ((anim flex-animator) attr val &optional (indices '()))
  (if indices
      (dotimes (i (length (vertices anim)))
	(when (member i indices)
	  (setf (slot-value (nth i (vertices anim)) attr) val)))
      (dolist (v (vertices anim))
	(setf (slot-value v attr) val))))

(defmethod set-flex-spring-attr ((group group) attr val)
  (traverse-hierarchy group
		      (lambda (s) (set-flex-spring-attr (animator s) attr val))
		      :test #'is-leaf?)
  group)

(defmethod set-flex-spring-attr ((anim flex-animator) attr val)
  (dolist (s (springs anim))
    (setf (slot-value s attr) val)))

(defmethod draw ((self shape))
  (when (animator self)
    (draw-animator (animator self))))

(defmethod draw ((self polygon-shape))
  (draw-geometry self)
  (when (animator self)
    (draw-animator (animator self))))

(defmethod draw-animator ((anim animator))
  ;; do nothing
  )

(defmethod draw-animator ((anim flex-animator))
  (when (show-springs? anim)
    (#_glLineWidth 1.0)
    (#_glColor3f 1.0 0.0 0.0)
    (#_glBegin #$GL_LINES)
    (dolist (spring (springs anim))
      (let ((p1 (point (vertex1 spring)))
	    (p2 (point (vertex2 spring))))
	(#_glVertex3f (x p1) (y p1) (z p1))
	(#_glVertex3f (x p2) (y p2) (z p2))))
    (#_glEnd))
  (when (show-pinned? anim)
    (#_glPointSize 15.0)
    (#_glColor3f 0.0 1.0 1.0)
    (#_glBegin #$GL_POINTS)
    (dolist (vertex (vertices anim))
      (when (pinned? vertex)
      (let ((p (point vertex)))
	(#_glVertex3f (x p) (y p) 0.0))))
    (#_glEnd)))

;;;; flex-mesh-animator =================================================

(defclass flex-mesh-animator (flex-animator)
  ((point-array :accessor point-array :initarg :point-array :initform nil)
   (cross-brace-1? :accessor cross-brace-1? :initarg :cross-brace-1? :initform nil)
   (cross-brace-2? :accessor cross-brace-2? :initarg :cross-brace-2? :initform nil)))

(defun grid-point-array (nx ny bounds-lo bounds-hi)
  (let ((point-array (make-array (list nx ny))))
    (dotimes (i nx)
      (let* ((fx (/ i (- nx 1.0)))
	     (x (lerp fx (x bounds-lo) (x bounds-hi))))
	(dotimes (j ny)
	  (let* ((fy (/ j (- ny 1.0)))
		 (y (lerp fy (y bounds-lo) (y bounds-hi))))
	    (setf (aref point-array i j) (p! x y))))))
    point-array))

(defun array-map (function array &optional (retval (make-array (array-dimensions array))))
  (dotimes (i (array-total-size array) retval)
    (setf (row-major-aref retval i)
          (funcall function (row-major-aref array i)))))

(defmethod init-anim :after ((anim flex-mesh-animator) (shape shape))
  (let ((nx (array-dimension (point-array anim) 0))
	(ny (array-dimension (point-array anim) 1))
	(vertex-array (array-map 
		       (lambda (p) (make-instance 'flex-vertex :point p))
		       (point-array anim))))
    (dotimes (i nx)
      (dotimes (j ny)
	(push (aref vertex-array i j) (vertices anim))))
    (let ((xb1 (cross-brace-1? anim))
	  (xb2 (cross-brace-2? anim)))
      (dotimes (i nx)
	(dotimes (j ny)
	  (when (< i (- nx 1))
	    (add-spring anim (aref vertex-array i j) (aref vertex-array (+ i 1) j)))
	  (when (< j (- ny 1))
	    (add-spring anim (aref vertex-array i j) (aref vertex-array i (+ j 1))))
	  (when (and xb1 (< i (- nx 1))  (< j (- ny 1)))
	    (add-spring anim (aref vertex-array i j) (aref vertex-array (+ i 1) (+ j 1))))
	  (when (and xb1 (> i 0)  (< j (- ny 1)))
	    (add-spring anim (aref vertex-array i j) (aref vertex-array (- i 1) (+ j 1))))
	  (when (and xb2 (< i (- nx 2))  (< j (- ny 2)))
	    (add-spring anim (aref vertex-array i j) (aref vertex-array (+ i 2) (+ j 2))))
	  (when (and xb2 (> i 1)  (< j (- ny 2)))
	    (add-spring anim (aref vertex-array i j) (aref vertex-array (- i 2) (+ j 2)))))))))


(defmethod update-anim :after ((anim flex-mesh-animator) (shape shape))
  ;; compute dynamics
  (dolist (v (vertices anim))
    (when (not (pinned? v))
      (compute-dynamics v (collisions? anim)))))


(defun make-mesh-anim-shape (nx ny bounds-lo bounds-hi
			     &key (cross-brace-1? nil) (cross-brace-2? nil))
  (let* ((anim (make-instance 'flex-mesh-animator
			      :point-array (grid-point-array nx ny bounds-lo bounds-hi)
			      :cross-brace-1? cross-brace-1?
			      :cross-brace-2? cross-brace-2?))
	 (shape (make-instance 'shape :animator anim)))
    (init-anim anim shape)
    shape))
    
#|

;;; square

(defparameter *group*
  (with-clear-and-redraw
    (add-shape *scene* (make-flex (make-group (make-square-shape 1.0))))))

(setf *time-step* 0.0005)
(setf *damping* 0.9)
(set-flex-vertex-attr *group* 'elasticity 0.8)

(set-flex-spring-attr *group* 'stiffness 100.0)

(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(0)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? nil '(0)))

;;; hexagon

(defparameter *group*
  (with-clear-and-redraw
    (add-shape *scene* (make-flex (make-group (make-circle-shape 1.0 6))))))

(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(5)))

(with-redraw (set-flex-vertex-attr *group* 'pinned? nil '(5)))

(setf *wind* (p! 5 0))
(setf *wind* (p! -10 0))
(setf *wind* (p! 0 0))

(set-flex-spring-attr *group* 'stiffness 100.0)

;;; circle

(setf *time-step* 0.0002)

(defparameter *group*
  (with-clear-and-redraw
    (add-shape *scene* (make-flex (make-group (make-circle-shape 1.0 16))))))
(set-flex-vertex-attr *group* 'elasticity 0.3)

(set-flex-spring-attr *group* 'stiffness 100.0)

(setf *wind* (p! 5 0))
(setf *wind* (p! -10 0))
(setf *wind* (p! 0 0))

(setf *gravity* (p! 0 0))

>>> implement friction
(set-flex-vertex-attr *group* 'friction 0.0)


(set-flex-spring-attr *group* 'stiffness 500.0) ; numerical insability

(setf *time-step* 0.00005)		; reduce simulation timestep


;;; mesh -- cross-brace-1

(setf *time-step* 0.0002)
(setf *damping* 0.9)

(defparameter *group*
  (with-clear-and-redraw
    (add-shape *scene* (make-group (make-mesh-anim-shape 7 7 (p! -.5 -.5) (p! .5 .5)
							 :cross-brace-1? t
							 :cross-brace-2? nil)))))

(set-flex-spring-attr *group* 'stiffness 200.0)
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(0 42)))

(setf *wind* (p! 5 0))
(setf *wind* (p! -10 0))
(setf *wind* (p! 0 0))

(with-redraw (set-flex-vertex-attr *group* 'pinned? nil '(42)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? nil '(0)))

(with-redraw (set-flex-vertex-attr *group* 'point (p! .8 .8) '(0)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(0)))

(with-redraw (set-flex-vertex-attr *group* 'point (p! -.8 .8) '(42)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(42)))

(set-flex-spring-attr *group* 'stiffness 500.0) ; induce numerical instability
(setf *time-step* 0.0001)

;;; mesh -- cross-brace-2

(setf *time-step* 0.0002)
(setf *damping* 0.9)

(defparameter *group*
  (with-clear-and-redraw
    (add-shape *scene* (make-group (make-mesh-anim-shape 7 7 (p! -.5 -.5) (p! .5 .5)
							 :cross-brace-1? t
							 :cross-brace-2? t)))))
(set-flex-spring-attr *group* 'stiffness 200.0)
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(0 42)))

(setf *wind* (p! 5 0))
(setf *wind* (p! -10 0))
(setf *wind* (p! 0 0))

(with-redraw (set-flex-vertex-attr *group* 'point (p! .8 .8) '(0)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(0)))

(with-redraw (set-flex-vertex-attr *group* 'point (p! -.8 .8) '(42)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(42)))
       
(with-redraw (set-flex-vertex-attr *group* 'point (p! 0 .8) '(21)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(21)))

|#
