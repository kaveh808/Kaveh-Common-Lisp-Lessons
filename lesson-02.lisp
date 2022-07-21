#|
Lesson 02 -- Generating Shapes
|#

;;; if starting fresh, uncomment to load previous lesson code (modify pathname as needed)
;(load "~/Development/kaveh-common-lisp-lessons/lesson-01.lisp")
;;; and to create graphics window
;(run)

;;; use C-c C-k to evaluate this buffer

#|
>>> feedback

>>> C-c C-k to eval entire buffer

>>> C-c C-j to send result to repl

>>> #' not required before lambda

(print (mapcar (lambda (x) (* x 2))
               '(1 2 3 4 5)))

(print (mapcar #'oddp '(1 2 3 4 5)))

>>> prog1

(defmacro with-redraw-1 (&body body)
  `(prog1 ,@body
          (redraw)))

(with-redraw-1 (+ 1 2) (+ 3 4))

(defmacro with-redraw-1 (&body body)
  `(prog1 (progn ,@body)
          (redraw)))

(with-redraw-1 (+ 1 2) (+ 3 4))

>>> SICP

|#

;;;; use from lesson 01 ========================================================

(defmethod print-object ((self point) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "[~a, ~a]" (x self) (y self))))

;;;; generating shapes =========================================================

;;; circle - generate shape mathematically
;;; >>> optional args
(defun make-circle-shape (diameter &optional (num-points 64))
  (let ((radius (/ diameter 2.0))
	(angle-delta (/ (* 2 pi) num-points))
	(shape (make-instance 'polygon-shape)))
    (dotimes (i num-points)
      (let ((angle (* i angle-delta)))
	(add-point shape (p! (* (sin angle) radius) (* (cos angle) radius)))))
    shape))

#|
>>> make circle
(with-redraw
  (clear-shapes *scene*)
  (add-shape *scene* (make-circle-shape 1.0)))
|#

;;; with-clear-and-redraw: add macro variant that clears scene
(defmacro with-clear-and-redraw (&body body)
  `(progn
     (clear-shapes *scene*)
     (let ((result (progn ,@body)))
       (redraw)
       result)))
#|
>>> alternate definition using prog1
(defmacro with-clear-and-redraw (&body body)
  `(progn
     (clear-shapes *scene*)
     (prog1 (progn ,@body)
       (redraw))))
|#

#|
>>> make circles of varying precision
(with-clear-and-redraw
  (add-shape *scene* (make-circle-shape 1.0 64))
  (add-shape *scene* (make-circle-shape 0.75 32))
  (add-shape *scene* (make-circle-shape 0.5 16))
  (add-shape *scene* (make-circle-shape 0.25 8)))

>>> examine macroexpansion
(pprint (macroexpand-1 '(with-clear-and-redraw
			 (add-shape *scene* (make-circle-shape 1.0 64))
			 (add-shape *scene* (make-circle-shape 0.75 32))
			 (add-shape *scene* (make-circle-shape 0.5 16))
			 (add-shape *scene* (make-circle-shape 0.25 8)))))

>>> randomize all scene points -- note this is same code used for squares in Lesson-01
(with-redraw
  (dolist (shape (shapes *scene*))
    (randomize-points shape (p! 0.05 0.05))))
|#

;;; we can generate regular polygon shapes using the circle shape function
;;; >>> use of macro to abstract out a code pattern
(defmacro def-polygon-shape (name num)
  `(defun ,(symcat 'make- name '-shape) (size)
     (make-circle-shape size ,num)))

(def-polygon-shape eql-tri 3)		;make-eql-tri-shape
(def-polygon-shape diamond 4)		;make-diamond-shape
(def-polygon-shape pentagon 5)		;make-pentagon-shape
(def-polygon-shape hexagon 6)		;make-hexagon-shape
(def-polygon-shape heptagon 7)		;make-heptagon-shape
(def-polygon-shape octagon 8)		;make-octagon-shape

#|
>>> examine macroexpansion
(pprint (macroexpand-1 '(def-polygon-shape pentagon 5)))

>>> create all polygon shapes
(with-clear-and-redraw
  (add-shape *scene* (make-eql-tri-shape 1.0))
  (add-shape *scene* (make-diamond-shape 1.0))
  (add-shape *scene* (make-pentagon-shape 1.0))
  (add-shape *scene* (make-hexagon-shape 1.0))
  (add-shape *scene* (make-heptagon-shape 1.0))
  (add-shape *scene* (make-octagon-shape 1.0)))

>>> create all polygon shapes scaled
(with-clear-and-redraw
  (add-shape *scene* (make-eql-tri-shape 0.4))
  (add-shape *scene* (make-diamond-shape 0.6))
  (add-shape *scene* (make-pentagon-shape 0.8))
  (add-shape *scene* (make-hexagon-shape 1.0))
  (add-shape *scene* (make-heptagon-shape 1.2))
  (add-shape *scene* (make-octagon-shape 1.4)))

>>> randomize all scene points
(with-redraw
  (dolist (shape (shapes *scene*))
    (randomize-points shape (p! 0.025 0.025))))
|#

;;; sine curve - generate shape mathematically
(defun make-sine-curve-shape (&optional (num-segments 64))
  (let ((angle-delta (/ (* 2 pi) num-segments))
	(shape (make-instance 'polygon-shape :is-closed-shape? nil)))
    (dotimes (i (1+ num-segments))
      (let ((angle (* i angle-delta)))
	(add-point shape (p! (/ angle (* 2 pi)) (sin angle)))))
    shape))

#|
>>> create sine curve
(with-clear-and-redraw
  (add-shape *scene* (make-sine-curve-shape)))
|#

(defmacro def-trig-curve-shape (func-name func)
  `(defun ,(symcat 'make- func-name '-curve-shape) (&optional (num-points 64))
     (let ((angle-delta (/ (* 2 pi) (- num-points 1)))
	   (shape (make-instance 'polygon-shape :is-closed-shape? nil)))
       (dotimes (i num-points)
	 (let ((angle (* i angle-delta)))
	   (add-point shape (p! (/ angle (* 2 pi)) (,func angle)))))
       shape)))

#|
>>> create sine and cosine curve functions
(def-trig-curve-shape sine sin)
(def-trig-curve-shape cosine cos)
|#

#|
>>> create sine and cosine curves
(with-clear-and-redraw
  (add-shape *scene* (make-sine-curve-shape))
  (add-shape *scene* (make-cosine-curve-shape)))

>>> macroexpand
(pprint (macroexpand-1 '(def-trig-curve-shape sine sin)))
(pprint (macroexpand-1 '(def-trig-curve-shape cosine cos)))
|#

;;;; spirals ===================================================================

#|
>>> implement make-spiral-shape based on circle-shape code

(defun make-circle-shape (diameter &optional (num-points 64))
  (let ((radius (/ diameter 2.0))
	(angle-delta (/ (* 2 pi) num-points))
	(shape (make-instance 'polygon-shape)))
    (dotimes (i num-points)
      (let ((angle (* i angle-delta)))
	(add-point shape (p! (* (sin angle) radius)
                             (* (cos angle) radius)))))
    shape))
|#

(defun make-spiral-shape (diameter &optional (num-points 64) (num-loops 1.0))
  (let ((radius-delta (/ (/ diameter 2.0) (1- num-points)))
	(angle-delta (/ (* 2 pi) num-points))
	(shape (make-instance 'polygon-shape :is-closed-shape? nil)))
    (dotimes (i num-points)
      (let ((radius (* i radius-delta))
	    (angle (* i angle-delta num-loops)))
	(add-point shape (p! (* (sin angle) radius)
                             (* (cos angle) radius)))))
    shape))

#|
>>> various spirals
(with-clear-and-redraw
  (add-shape *scene* (make-spiral-shape 1.5)))

(with-clear-and-redraw
  (add-shape *scene* (make-spiral-shape 1.5 256 2)))

(with-clear-and-redraw
  (add-shape *scene* (make-spiral-shape 1.5 256 4)))
|#
