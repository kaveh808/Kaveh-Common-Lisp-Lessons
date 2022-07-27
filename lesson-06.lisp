#|
Lesson 06 -- Hierarchies
|#

(load "~/Development/kaveh-common-lisp-lessons/lesson-05.lisp")

;;;; modeling with groups ===============================================

;;; make a row of groups all of which share the same shape
;;; we save memory by sharing the same shape
;;; this sharing is known as instancing
(defun make-row-group (n spacing shape)
  (let ((row '()))
    (dotimes (i n)
      (push (make-instance 'group :children (list shape)
				  :transform (make-instance 'transform :translate (p* spacing (p! i i))))
	    row))
    (make-instance 'group :children row)))

;;; version with loop macro
(defun make-row-group (n spacing shape)
  (make-instance 'group
                 :children (loop for i to n
                                 collect (make-instance
                                          'group
                                          :children (list shape)
                                          :transform (make-instance 'transform
                                                                    :translate (p* spacing (p! i i)))))))
  
;;; we use make-row to make a row group object
(progn
  (defparameter *row-group* nil)
  (with-clear-and-redraw
    (setf *row-group* (make-row-group 10 (p! 0.2 0.0) (make-circle-shape 0.2 16)))
    (translate-by *row-group* (p! -0.9 0.0))
    (add-shape *scene* *row-group*)))

;;; print hierarchy
(print-hierarchy *scene*)

;;; we then make another row of row groups to make a grid
(progn
  (defparameter *grid-group* nil)
  (with-clear-and-redraw
    (setf *grid-group* (make-row-group 10 (p! 0.0 0.2) *row-group*))
    (translate-by *grid-group* (p! 0.0 -0.9))
    (add-shape *scene* *grid-group*)))

;;; print hierarchy
(print-hierarchy *scene*)

;;; we generalize this approach by creating groups which insantiate shapes at specified points
(defun scatter-group (shape-fn points)
  (make-instance 'group :children (mapcar (lambda (p)
                                            (translate-to (funcall shape-fn) p))
                                          points)))

;;; test with three points
(with-clear-and-redraw
  (add-shape *scene*
	     (scatter-group (lambda () (make-hexagon-shape 1.0))
                            (list (p! -.5 -.433)
                                  (p!  .5 -.433)
                                  (p!   0  .433)))))

;;; make a recursive group
(defun make-hex-tri-group (levels)
  (scale-to (scatter-group (lambda ()
                             (if (= 0 levels)
                                 (make-hexagon-shape 1.0)
                                 (make-hex-tri-group (- levels 1))))
                           (list (p! -.5 -.433)
                                 (p!  .5 -.433)
                                 (p!   0  .433)))
            0.5))

(with-clear-and-redraw
  (add-shape *scene* (scale-to (make-hex-tri-group 0) 1.0)))

(with-clear-and-redraw
  (add-shape *scene* (scale-to (make-hex-tri-group 1) 1.0)))

(with-clear-and-redraw
  (add-shape *scene* (scale-to (make-hex-tri-group 2) 1.0)))

(with-clear-and-redraw
  (add-shape *scene* (scale-to (make-hex-tri-group 4) 1.0)))

(defun make-square-cross-group (levels)
  (scale-to (scatter-group (lambda ()
                             (if (= 0 levels)
                                 (make-square-shape 0.8)
                                 (make-square-cross-group (- levels 1))))
                           (list (p!  0.0  0.0)
                                 (p! -1.0  0.0)
                                 (p!  1.0  0.0)
                                 (p!  0.0 -1.0)
                                 (p!  0.0  1.0)
                                 (p! -2.0  0.0)
                                 (p!  2.0  0.0)
                                 (p!  0.0 -2.0)
                                 (p!  0.0  2.0)))
            0.25))

(with-clear-and-redraw
  (add-shape *scene* (scale-to (make-square-cross-group 0) 0.33)))

(with-clear-and-redraw
  (add-shape *scene* (scale-to (make-square-cross-group 1) 0.33)))

(with-clear-and-redraw
  (add-shape *scene* (scale-to (make-square-cross-group 2) 0.33)))

(defun make-square-x-group (levels)
  (scale-to (scatter-group (lambda ()
                             (if (= 0 levels)
                                 (make-square-shape 1.5)
                                 (make-square-x-group (- levels 1))))
                           (list (p! -1.0 -1.0)
                                 (p!  1.0  1.0)
                                 (p!  1.0 -1.0)
                                 (p! -1.0  1.0)
                                 (p! -2.0 -2.0)
                                 (p!  2.0  2.0)
                                 (p!  2.0 -2.0)
                                 (p! -2.0  2.0)))
            0.25))

(with-clear-and-redraw
  (add-shape *scene* (scale-to (make-square-x-group 0) 0.33)))

(with-clear-and-redraw
  (add-shape *scene* (scale-to (make-square-x-group 1) 0.33)))

(with-clear-and-redraw
  (add-shape *scene* (scale-to (make-square-x-group 2) 0.33)))

;;; generalized recursive group function
(defun make-recursive-group (levels base-shape-fn points scaling)
  (scale-to (scatter-group (lambda ()
                             (if (= 0 levels)
                                 (funcall base-shape-fn)
                                 (make-recursive-group (- levels 1)
                                                       base-shape-fn
                                                       points
                                                       scaling)))
                           points)
	    scaling))

(with-clear-and-redraw
  (add-shape *scene* (make-recursive-group 0
					   (lambda () (make-square-shape 0.8))
					   (points (make-hexagon-shape 4.0))
                                           0.333)))

(with-clear-and-redraw
  (add-shape *scene* (make-recursive-group 0
					   (lambda () (make-spiral-shape 0.5 16))
					   (points (make-spiral-shape 5.0 32 1))
					   0.3)))

(with-clear-and-redraw
  (add-shape *scene* (make-recursive-group 0
					   (lambda () (make-hexagon-shape 1.0))
					   (points (make-hexagon-shape 2.0))
					   0.5)))

(with-clear-and-redraw
  (add-shape *scene* (make-recursive-group 0
					   (lambda () (make-pentagon-shape 1.0))
					   (points (make-pentagon-shape 2.0))
					   0.4)))

(with-clear-and-redraw
  (add-shape *scene* (make-recursive-group 0
					   (lambda () (make-circle-shape 1.0 8))
					   (points (make-circle-shape 6.0 16))
					   0.25)))

;;; implement a tree walker with a predicate argument
#|
;;; add test predicate to do-hierarchy
(defmethod do-hierarchy ((self shape) func)
  (funcall func self))

(defmethod do-hierarchy :after ((self group) func)
  (dolist (child (children self))
    (do-hierarchy child func)))
|#

;;; use of keyword argument
(defmethod do-hierarchy ((self shape) func &key (test nil))
  (when (or (null test) (funcall test self))
    (funcall func self))
  self)

;;; use of :after method
(defmethod do-hierarchy :after ((self group) func &key (test nil))
  (dolist (child (children self))
    (do-hierarchy child func :test test))
  self)

(with-clear-and-redraw
  (add-shape *scene* *group-2*))

(with-redraw
  (do-hierarchy *group-2*
    (lambda (s) (scale-by s 1.2))
    :test (lambda (s) (and (typep s 'polygon-shape)
                           (= 3 (length (points s)))))))

(defmethod is-leaf? ((self shape))
  t)

(defmethod is-leaf? ((self group))
  nil)

(defun randomize-size (shape lo hi)
  (let* ((scale (rand2 lo hi)))
    (scale-by (transform shape) scale)))

(with-clear-and-redraw
  (add-shape *scene* (scale-to (make-hex-tri-group 2) 1.0)))

;;; apply to leaf nodes in hierarchy
(with-redraw
  (do-hierarchy (first (shapes *scene*))
		      (lambda (shape) (randomize-size shape 0.5 1.5))
		      :test #'is-leaf?))

;;; apply to all nodes in hierarchy
(with-redraw
  (do-hierarchy (first (shapes *scene*))
		      (lambda (shape) (randomize-size shape 0.8 1.2))))

;;; jittering (random offsetting) shape transform values
(defmethod jitter-translate ((self shape) (jitter point))
  (translate-by (transform self)
		(p! (rand1 (x jitter)) (rand1 (y jitter))))
  self)

(defmethod jitter-rotate ((self shape) (jitter number))
  (rotate-by (transform self) (rand1 jitter))
  self)

(defmethod jitter-scale ((self shape) (jitter number))
  (scale-by (transform self) (+ 1.0 (rand1 jitter)))
  self)

(defun make-recursive-group-2 (levels base-shape-fn points scaling translate rotate size)
  (let ((group (scatter-group (lambda ()
                                (if (= 0 levels)
                                    (funcall base-shape-fn)
                                    (make-recursive-group-2 (- levels 1)
                                                            base-shape-fn
                                                            points
                                                            scaling
                                                            translate
                                                            rotate
                                                            size)))
                              points)))
    (jitter-translate group translate)
    (jitter-rotate group rotate)
    (jitter-scale group size)
    (scale-by group scaling)))

(defun make-wobbly-cross (levels)
  (make-recursive-group-2 levels
			  (lambda () (make-square-shape 0.8))
			  (list (p!  0.0  0.0)
				(p! -1.0  0.0)
				(p!  1.0  0.0)
				(p!  0.0 -1.0)
				(p!  0.0  1.0)
				(p! -2.0  0.0)
				(p!  2.0  0.0)
				(p!  0.0 -2.0)
				(p!  0.0  2.0))
                          0.25
			  (p! 0.0 0.0)
			  10.0
			  0.5))

(defun randomize-leaf-sizes (group lo hi)
  (do-hierarchy group (lambda (shape) (randomize-size shape lo hi))
		      :test #'is-leaf?))

(defun randomize-node-sizes (group lo hi)
  (do-hierarchy group (lambda (shape) (randomize-size shape lo hi))))

(progn
  (defparameter *cross* (scale-to (make-wobbly-cross 2) .33))
  (with-clear-and-redraw
    (add-shape *scene* *cross*)))

(with-redraw
  (randomize-leaf-sizes *cross* 0.5 1.5))

(with-redraw
  (randomize-node-sizes *cross* 0.8 1.2))
