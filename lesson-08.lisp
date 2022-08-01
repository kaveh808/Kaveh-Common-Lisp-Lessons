#|
Lesson 08 -- Colors & Groups
|#

(load "~/Development/kaveh-common-lisp-lessons/lesson-07.lisp")

#|
update-instance-for-redefined-class
ensure-generic-function
|#

(defun randomize-colors (shape)
  (set-fill-color shape #'c-rand))

(defun randomize-colors-range (shape c1 c2)
  (set-fill-color shape (lambda () (c-rand2 c1 c2))))

;;; make grid
(defun make-squares-grid (n size)
  (scatter-group (lambda () (make-square-shape size))
                 (grid-points n n (p! -0.8 -0.8) (p! 0.8 0.8))))
  
(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene* (make-squares-grid 14 0.1))))

;;; adjust colors
(with-redraw
  (setf (bg-color *scene*) (c! 0.5 0.5 0.5)))

(with-redraw
  (set-outline-color *shape* (c! 0 0 0 1)))

(with-redraw
  (randomize-colors *shape*))

(with-redraw
  (set-outline-color *shape* (c! 0 0 0 0)))

(with-redraw
  (randomize-colors-range *shape* (c! 0.0 0. 0.0 0.25) (c! 0.0 0.0 1.0 0.25)))

(with-redraw
  (randomize-colors-range *shape* (c! 0.0 0. 0.0) (c! 0.0 0.0 1.0)))

;;; randomize points
(defun randomize-group-points (group delta)
  (do-hierarchy group
    (lambda (s) (randomize-points s (p! delta delta)))
    :test #'is-leaf?))

(with-redraw
  (randomize-group-points *shape* 0.01))

;;; make cloud of shapes & redo above tests
(defun make-random-squares (n)
  (scatter-group (lambda () (make-square-shape 0.4))
                 (random-points n (p! -0.8 -0.8) (p! 0.8 0.8))))
  
(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene* (make-random-squares 100))))

;;; can't do with set-fill-color -- modifies existing color
(defun jitter-group-color (group c-delta)
  (do-hierarchy group
    (lambda (s)
      (setf (fill-color (appearance s))
            (c-jitter (fill-color (appearance s)) c-delta)))
    :test #'is-leaf?))

;;; can't do with set-fill-color -- sets alpha only
(defun set-group-alpha (group alpha)
  (do-hierarchy group
    (lambda (s) (setf (aref (fill-color (appearance s)) 3) alpha))
    :test #'is-leaf?))

(with-redraw
  (set-group-alpha *shape* 0.1))

(with-redraw
  (jitter-group-color *shape* (c! 0.1 0.1 0.1 0.1)))

(with-redraw
  (set-outline-color *shape* (c! 1 1 1 0.5)))

(with-redraw
  (set-fill-color *shape* (c! 0.2 0.8 0.2 0.25)))

;;; unify setting colors, jittering color, and setting alpha

(defun c-add! (c1 c2 &key (alpha t))
  (incf (aref c1 0) (aref c2 0))
  (incf (aref c1 1) (aref c2 1))
  (incf (aref c1 2) (aref c2 2))
  (when alpha
    (incf (aref c1 3) (aref c2 3))))

(defun c-set-alpha! (c1 a)
  (setf (aref c1 3) (coerce a 'single-float)))

(defun c-add-alpha! (c1 a)
  (incf (aref c1 3) (coerce a 'single-float)))

(defgeneric set-shape-color (self value-or-fn &rest args)
  ;; value-or-fn -- color color-fn alpha alpha-fn
  ;; component -- :rgb :alpha :rgba
  ;; part -- :fill :outline
  ;; operation -- :set :add
  
  (:method ((self t) value-or-fn &key (component :rgba) (part :fill) (operation :set))
    (declare (ignore value-or-fn component part operation))) ;do nothing
  
  (:method ((self appearance) value-or-fn &key (component :rgba) (part :fill) (operation :set))
    (let ((value (if (functionp value-or-fn)
                     (funcall value-or-fn)
                     value-or-fn))
          (color (if (eq part :fill)
                     (fill-color self)
                     (outline-color self))))
      (cond ((eq component :rgba)
             (if (eq operation :set)
                 (c-set! color value :alpha t)
                 (c-add! color value :alpha t)))
            ((eq component :rgb)
             (if (eq operation :set)
                 (c-set! color value :alpha nil)
                 (c-add! color value :alpha nil)))
            ((eq component :alpha)
             (if (eq operation :set)
                 (c-set-alpha! color value)
                 (c-add-alpha! color value))))))

  (:method ((self polygon-shape) value-or-fn &key (component :rgba) (part :fill) (operation :set))
    (set-shape-color (appearance self) value-or-fn :component component :part part :operation operation))

  (:method ((self group) value-or-fn &key (component :rgba) (part :fill) (operation :set))
    (dolist (child (children self))
      (set-shape-color child value-or-fn :component component :part part :operation operation))))

;;; redefine in terms of set-shape-color
(defun randomize-colors (shape)
  (set-shape-color shape #'c-rand))

(defun randomize-colors-range (shape c1 c2)
  (set-shape-color shape (lambda () (c-rand2 c1 c2)) :component :rgb)) ;don't set alpha

(defun jitter-shape-color (shape c-delta)
  (set-shape-color shape (lambda () (c-rand1 c-delta)) :component :rgb :operation :add))

(defun set-shape-alpha (shape alpha)
  (set-shape-color shape alpha :component :alpha))

;;; test
(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene* (scale-to (make-hex-tri-group 3) (p! 1 1)))))

(with-redraw
  (randomize-group-points *shape* 0.2))

(with-redraw
  (do-hierarchy *shape*
    (lambda (shape) (randomize-size shape 0.8 1.5))
    :test #'is-leaf?))

(with-redraw
  (set-shape-color *shape* (c! 0 0 0 1)))

(with-redraw
  (randomize-colors *shape*))

(with-redraw
  (set-shape-color *shape* (c! 0 0 0 0) :part :outline))

(with-redraw
  (randomize-colors-range *shape* (c! 0.0 0. 0.0) (c! 0.0 0.0 1.0)))

(with-redraw
  (set-shape-color *shape* 0.25 :component :alpha))

(with-redraw
  (set-shape-alpha *shape* 1))

(with-redraw
  (set-shape-color *shape* (c! 1 1 1 0.5) :part :outline))

(with-redraw
  (set-shape-color *shape* (c! 0.2 0.8 0.2 0.25)))

(with-redraw
  (jitter-shape-color *shape* (c! 0.2 0.2 0.2)))

;;; test group & redo above tests
(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene* (scale-to (make-wobbly-cross 1) (p! .33 .33)))))

(defun make-cloud-group (num shape-fn bounds-lo bounds-hi &key (max-alpha 0.25) (scale? nil))
  (let ((group (scatter-group shape-fn
                              (random-points num bounds-lo bounds-hi)))
	(max-dist (/ (p-dist bounds-lo bounds-hi) 2)))
    (dolist (shape (children group))
      (let* ((factor (- 1.0 (/ (p-mag (translate (transform shape))) max-dist)))
	     (col (c! 1.0 1.0 1.0 (lerp factor 0.0 max-alpha))))
	(set-shape-color shape col)
	(when scale?
	  (scale-by shape factor))))
    (set-shape-color group (c! 0 0 0 0) :part :outline)
    group))

(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene*
	       (make-cloud-group 100
				 (lambda () (make-circle-shape 0.4))
				 (p! -0.8 -0.4)
				 (p! 0.8 0.4)
				 :max-alpha 0.5
				 :scale? nil))))

(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene*
	       (make-cloud-group 100
				 (lambda () (make-circle-shape 0.4))
				 (p! -0.8 -0.4)
				 (p! 0.8 0.4)
				 :max-alpha 0.5
				 :scale? t))))

(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene*
	       (make-cloud-group 1000
				 (lambda () (make-circle-shape 0.4))
				 (p! -0.8 -0.8)
				 (p! 0.8 0.8)
				 :max-alpha 0.05
				 :scale? t))))

(with-redraw
  (randomize-colors-range *shape* (c! 0 0 1) (c! 1 1 1)))

(with-redraw
  (randomize-colors *shape*))

