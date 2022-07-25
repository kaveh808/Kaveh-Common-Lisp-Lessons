#|
Lesson 04 -- Transforming Shapes
|#

;;; if starting fresh, uncomment to load previous lesson code (modify pathname as needed)
;(load "~/Development/kaveh-common-lisp-lessons/lesson-03.lisp")
;;; and to create graphics window
;(run)

;;; restarts -- recover and continue from error ================================

;;; unbound variable -- provide value
(+ 1 my-variable)

;;; undefined function -- define and proceed
(+ 2 (my-triple 5))

(defun my-triple (x) (* x 3))

;;; absolute transform operations ==============================================

(defmethod translate-to ((self transform) (p point))
  (setf (translate self) p))

(defmethod rotate-to ((self transform) (r number))
  (setf (rotate self) r))

(defmethod scale-to ((self transform) (s number))
  (setf (scale self) (p! s s)))

(defmethod scale-to ((self transform) (p point))
  (setf (scale self) p))

;;; utility method for transforming shapes
(defmethod translate-to ((self shape) (p point))
  (translate-to (transform self) p)
  self)

(defmethod rotate-to ((self shape) (r number))
  (rotate-to (transform self) r)
  self)

(defmethod scale-to ((self shape) (s number))
  (scale-to (transform self) s)
  self)

(defmethod scale-to ((self shape) (p point))
  (scale-to (transform self) p)
  self)

;;;; marker-shape ==============================================================

;;; this is an "implicit" shape -- the geometry is generated at drawing time and only the
;;; size attr is stored
(defclass marker-shape (shape)
  ((size :accessor size :initarg :size :initform 0.1)))

(defmethod draw ((self marker-shape))
  (#_glColor3f 1.0 0.0 0.0)
  (#_glLineWidth 3.0)
  (#_glBegin #$GL_LINES)
  (let ((s (/ (size self) 2.0)))
    (#_glVertex3f 0.0     s  0.0)
    (#_glVertex3f 0.0  (- s) 0.0)
    (#_glVertex3f    s  0.0  0.0)
    (#_glVertex3f (- s) 0.0  0.0))
  (#_glEnd))

;;; test with marker-shape
(with-clear-and-redraw
  (add-shape *scene* (make-instance 'marker-shape)))

;;; transform marker-shape
(with-redraw
  (for-scene-shapes
   #'(lambda (s)
       (translate-by s (p! 0.05 0.01))
       (rotate-by s 15.0)
       (scale-by s 1.5))))

;;; add randomly-places and sized markers to the scene
(with-clear-and-redraw
  (dotimes (i 100)
    (add-shape *scene*
               (translate-to (make-instance 'marker-shape :size (rand2 0.05 0.2))
                             (p! (rand1 0.8) (rand1 0.8))))))

;;; randomly rotate markers
(with-redraw
  (dolist (s (shapes *scene*))
    (rotate-by s (rand1 20))))

;;;; scattering shapes =========================================================

;;; make shapes at list of points
(defun make-shapes-at-points (shape-fn points)
  (dolist (p points)
    (let ((s (funcall shape-fn)))
      (translate-to s p)
      (add-shape *scene* s))))

;;; shapes on a circle
(with-clear-and-redraw
  (make-shapes-at-points (lambda () (make-hexagon-shape 0.2))
                         (points (make-circle-shape 1.5 32))))

;;; shapes on a sine curve
(with-clear-and-redraw
  (make-shapes-at-points (lambda () (make-hexagon-shape 0.2))
                         (mapcar (lambda (p) (p+ p (p! -0.5 0.0))) ;center sine curve by trans (-.5, 0)
                                 (points (make-sine-curve-shape 32)))))

;;; rewrite make-shapes-at-points as scatter-shapes
(defun scatter-shapes (shape-fn points)
  (mapc (lambda (p)
          (translate-to (add-shape *scene* (funcall shape-fn)) p))
        points))

;;; shapes on a spiral
(with-clear-and-redraw
  (scatter-shapes (lambda () (make-hexagon-shape 0.1))
                  (points (make-spiral-shape 1.5 128 4))))

;;;; generic functions =========================================================

;;; polymorphism
;;; defining macros implicitly creates generic functions
;;; e.g. p* from lesson-03
#|
(defmethod p* ((p1 point) (p2 point))
  (p! (* (x p1) (x p2))
      (* (y p1) (y p2))))

(defmethod p* ((p1 point) (s number))
  (p! (* (x p1) s)
      (* (y p1) s)))
|#

(type-of #'p*)
(class-of #'p*)
(describe #'p*)
(inspect #'p*)

;;; scatter-shapes is a regular (non-generic) function
(type-of #'scatter-shapes)

;;; a restart allows us to redefine scatter-shapes as a generic function
(defgeneric scatter-shapes (shape-fn locations)
  )

(type-of #'scatter-shapes)
(inspect #'scatter-shapes)

;;; methods do not "belong" to classes

;;; list arg
(defmethod scatter-shapes (shape-fn (points list))
  (mapc (lambda (p)
          (translate-to (add-shape *scene* (funcall shape-fn)) p))
        points))

(with-clear-and-redraw
  (scatter-shapes (lambda () (make-diamond-shape 0.2))
                  (list (p! -.5 -.5) (p! .5 -.5) (p! 0 .5))))

;;; shape arg
(defmethod scatter-shapes (shape-fn (shape polygon-shape))
  (mapc (lambda (p)
          (translate-to (add-shape *scene* (funcall shape-fn)) p))
        (points shape)))

(with-clear-and-redraw
  (scatter-shapes (lambda () (make-diamond-shape 0.2))
                  (make-circle-shape 1.5 32)))

;;; function arg
(defmethod scatter-shapes (shape-fn (points-fn function))
  (mapc (lambda (p)
          (translate-to (add-shape *scene* (funcall shape-fn)) p))
        (funcall points-fn)))

(with-clear-and-redraw
  (scatter-shapes (lambda () (make-diamond-shape 0.2))
                  (lambda () (let ((points '()))
                               (dotimes (i 20)
                                 (push (p! (rand1 1.0) (rand1 1.0))
                                       points))
                               points))))

(defun grid-points (nx ny bounds-lo bounds-hi)
  (let ((points '()))
    (dotimes (i nx)
      (let* ((fx (/ i (- nx 1.0)))
	     (x (lerp fx (x bounds-lo) (x bounds-hi))))
	(dotimes (j ny)
	  (let* ((fy (/ j (- ny 1.0)))
		 (y (lerp fy (y bounds-lo) (y bounds-hi))))
	    (push (p! x y) points)))))
    points))

;;; make grid of points
(grid-points 3 2 (p! -1.0 -0.5) (p! 1.0 0.5))

;;; make markers at grid points
(with-clear-and-redraw
  (scatter-shapes (lambda () (make-instance 'marker-shape))
                  (grid-points 6 4 (p! -0.8 -0.4) (p! 0.8 0.4))))

;;; make grid of squares
(with-clear-and-redraw
  (scatter-shapes (lambda () (make-square-shape 0.1))
                  (grid-points 14 7 (p! -0.8 -0.4) (p! 0.8 0.4))))

;;; randomize shape points
(with-redraw
  (for-scene-shapes (lambda (s) (randomize-points s (p! 0.01 0.01)))))

;;; grid of random polygons
(with-clear-and-redraw
  (scatter-shapes (lambda () (make-circle-shape 0.2 (floor (rand2 3 16))))
                  (grid-points 8 8 (p! -0.8 -0.8) (p! 0.8 0.8))))

;;; random points within bounds
(defun random-points (n bounds-lo bounds-hi)
  (let ((points '()))
    (dotimes (i n)
      (push (p! (rand2 (x bounds-lo) (x bounds-hi))
		(rand2 (y bounds-lo) (y bounds-hi)))
	    points))
    points))

;;; rewritten with loop macro
(defun random-points (n bounds-lo bounds-hi)
  (loop for i from 1 to n
        collect (p! (rand2 (x bounds-lo) (x bounds-hi))
                    (rand2 (y bounds-lo) (y bounds-hi)))))

(pprint (macroexpand-1 '(loop for i from 1 to n
                         collect (p! (rand2 (x bounds-lo) (x bounds-hi))
                                  (rand2 (y bounds-lo) (y bounds-hi))))))

;;; function arg -- with loop macro
(with-clear-and-redraw
  (scatter-shapes (lambda () (make-diamond-shape 0.2))
                  (lambda () (loop for i from 1 to 20
                                   collect (p! (rand1 1.0) (rand1 1.0))))))

;;; make markers at points
(with-clear-and-redraw
  (scatter-shapes (lambda () (make-instance 'marker-shape))
                  (random-points 100 (p! -0.8 -0.8) (p! 0.8 0.8))))

;;; shapes scaled by distance from origin
(with-clear-and-redraw
  (scatter-shapes (lambda () (make-eql-tri-shape 0.4))
                  (random-points 300 (p! -0.9 -0.9) (p! 0.9 0.9)))
  (for-scene-shapes
   (lambda (s)
     (let ((scale (- 1.0 (p-mag (translate (transform s))))))
       (scale-to s scale)))))

;;; shapes outside the unit circle get scaled by a negative amount (visible with triangles)
(with-redraw
  (add-shape *scene* (make-circle-shape 2.0)))

