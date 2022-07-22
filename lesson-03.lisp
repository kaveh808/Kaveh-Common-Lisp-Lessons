#|
Lesson 03 -- Transforms
|#

;;; if starting fresh, uncomment to load previous lesson code (modify pathname as needed)
;(load "~/Development/kaveh-common-lisp-lessons/lesson-02.lisp")
;;; and to create graphics window
;(run)

;;; use C-c C-k to evaluate this buffer

;;;; utils ==============================================================

;;; linear interpolation
(defun lerp (f lo hi)
  (+ lo (* f (- hi lo))))

;;;; point ==============================================================

;;; multimethods -- multiply by point or number
(defmethod p* ((p1 point) (p2 point))
  (p! (* (x p1) (x p2))
      (* (y p1) (y p2))))

(defmethod p* ((p1 point) (s number))
  (p! (* (x p1) s)
      (* (y p1) s)))

;;; magnitude (length) of point
(defun p-mag (p)
  (sqrt (+ (* (x p) (x p)) (* (y p) (y p)))))

;;; distance between two points
(defun p-dist (p1 p2)
  (sqrt (+ (expt (- (x p2) (x p1)) 2)
	   (expt (- (y p2) (y p1)) 2))))

;;;; transform ==========================================================

;;; >>> explanation of transforms
(defclass transform ()
  ((translate :accessor translate :initarg :translate :initform (p! 0.0 0.0))
   (rotate :accessor rotate :initarg :rotate :initform 0.0)
   (scale :accessor scale :initarg :scale :initform (p! 1.0 1.0))))

;;; >>> make sure rotate is single-float
(defmethod (setf rotate) (val (self transform))
  (setf (slot-value self 'rotate) (coerce val 'single-float)))

(defmethod translate-by ((self transform) (p point))
  (setf (translate self) (p+ (translate self) p)))

(defmethod rotate-by ((self transform) (r number))
  (setf (rotate self) (+ (rotate self) r)))

;;; multimethods: scale-by
(defmethod scale-by ((self transform) (p point))
  (setf (scale self) (p* (scale self) p)))

(defmethod scale-by ((self transform) (s number))
  (setf (scale self) (p* (scale self) s)))

(defmethod reset-transform ((self transform))
  (setf (translate self) (p! 0.0 0.0))
  (setf (rotate self) 0.0)
  (setf (scale self) (p! 1.0 1.0)))

(defmethod print-object ((self transform) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream ":TRANSLATE ~a :ROTATE ~a :SCALE ~a" (translate self) (rotate self) (scale self))))

;;;; shape enhancements =================================================

#|
>>> see shape before class redefinition
(defparameter *sh* (make-instance 'polygon-shape))
(inspect *sh*)
|#

;;; >>> redefine shape class with transform slot
(defclass shape ()
  ((transform :accessor transform :initarg :transform :initform (make-instance 'transform))))

#|
>>> refresh inspector
>>> see polygon-shape after superclass redefinition & note use of transform print method
|#

;;; utility method for transforming shapes
(defmethod translate-by ((self shape) (p point))
  (translate-by (transform self) p)
  self)

(defmethod rotate-by ((self shape) (r number))
  (rotate-by (transform self) r)
  self)

(defmethod scale-by ((self shape) (p point))
  (scale-by (transform self) p)
  self)

(defmethod scale-by ((self shape) (s number))
  (scale-by (transform self) s)
  self)

(defmethod reset-transform ((self shape))
  (reset-transform (transform self))
  self)

;;; >>> :before and :after method combinations
;;; push matrix and do transform operations before drawing
(defmethod draw :before ((self shape))
  (let ((xform (transform self)))
    (#_glPushMatrix)
    (#_glTranslatef (x (translate xform)) (y (translate xform)) 0.0)
    (#_glRotatef (rotate xform) 0.0 0.0 1.0)
    (#_glScalef (x (scale xform)) (y (scale xform)) 1.0)))

;;; pop matrix after drawing
(defmethod draw :after ((self shape))
  (#_glPopMatrix))

;;; >>> macro for operating on all shapes in a scene
(defmacro for-scene-shapes (func)
  `(mapcar ,func (shapes *scene*)))

#|
>>> make and transform a shape

(defparameter *square-1* (make-square-shape 1.0))
(with-clear-and-redraw
  (add-shape *scene* *square-1*))

>>> translate shape
(with-redraw
  (translate-by *square-1* (p! 0.05 0.01)))

>>> rotate shape
(with-redraw
  (rotate-by *square-1* 5.0))

>>> scale shape uniformly
(with-redraw
  (scale-by *square-1* 0.9))

>>> scale shape non-uniformly
(with-redraw
  (scale-by *square-1* (p! 0.9 1.2)))

>>> reset transform
(with-redraw
  (reset-transform *square-1*))
|#


#|
>>> method combination called:

SHAPE DRAW :BEFORE
  POLYGON-SHAPE DRAW
SHAPE DRAW :AFTER

>>> if we defined :before and :after draw methods on polygon-shape:

POLYGON-SHAPE DRAW :BEFORE
  SHAPE DRAW :BEFORE
    POLYGON-SHAPE DRAW
  SHAPE DRAW :AFTER
POLYGON-SHAPE DRAW :AFTER

>>> make scaled and rotated shapes
>>> use let* due to scale depending on factor
(with-clear-and-redraw
  (dotimes (i 10)
    (let* ((factor (/ i 9.0))
	   (hex (make-hexagon-shape 1.0))
	   (scale (lerp factor 0.1 1.5) ))
      (scale-by hex scale)
      (rotate-by hex (lerp factor 0 90))
      (add-shape *scene* hex))))

>>> make row of shapes
(with-clear-and-redraw
  (dotimes (i 10)
    (let ((factor (/ i 9.0))
	  (hex (make-hexagon-shape 0.2)))
      (translate-by hex (p! (lerp factor -0.9 0.9) 0))
      (add-shape *scene* hex))))

>>> randomly move shapes in y
(with-redraw
  (for-scene-shapes
   (lambda (s) (translate-by s (p! 0.0 (rand1 0.1))))))

>>> randomly rotate shapes
(with-redraw
  (for-scene-shapes
   (lambda (s) (rotate-by s (rand1 10.0)))))

>>> randomly scale shapes
(with-redraw
  (for-scene-shapes
   (lambda (s)
       (let ((scale (rand2 0.8 1.2)))
	 (scale-by s scale)))))

>>> reset transforms -- all shapes move back to origin
(with-redraw
  (for-scene-shapes
   #'reset-transform))
|#
