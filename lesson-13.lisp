#|
Lesson 13 -- Dynamics
|#

;;;; utils ==============================================================

(defmethod p/ ((p1 point) (n number))
  (p! (/ (x p1) n)
      (/ (y p1) n)))

(defmethod p/ ((p1 point) (p2 point))
  (p! (/ (x p1) (x p2))
      (/ (y p1) (y p2))))

(defmethod p-rand2 ((p1 point) (p2 point))
  (p! (rand2 (x p1) (x p2))
      (rand2 (y p1) (y p2))))
      
;;;; dynamics-animator ==================================================

(defparameter *gravity* (p! 0 -.01))
(defparameter *wind* (p! 0 0))
(defparameter *time-step* 1.0)
(defparameter *collision-padding* 0.1)

(defclass dynamics-animator (animator)
  ((velocity :accessor velocity :initarg :velocity :initform (p! 0 0))
   (mass :accessor mass :initarg :mass :initform 1.0)
   (elasticity :accessor elasticity :initarg :elasticity :initform 0.75)
   (collisions? :accessor collisions? :initarg :collisions? :initform t))
  (:default-initargs
   :update-fn #'update-dynamics))

(defun update-dynamics (anim)
  (let* ((force (p+ *gravity* *wind*))
	 (acc (p/ force (mass anim)))	;compute acceleration
	 (vel (p+ (velocity anim) acc)) ;compute velocity
	 (pos (p+ (translate (transform (shape anim))) (p* vel *time-step*)))) ;compute position
    (when (collisions? anim)            ; handle collision -- approximate
      (let ((elast (elasticity anim))
	    (lo (+ -1.0 *collision-padding*))
	    (hi (- 1.0 *collision-padding*)))
	(when (< (x pos) lo)
	  (setf (x pos) (+ lo (abs (- lo (x pos)))))
	  (setf (x vel) (* elast (- (x vel)))))
	(when (> (x pos) hi)
	  (setf (x pos) (- hi (abs (- hi (x pos)))))
	  (setf (x vel) (* elast (- (x vel)))))
	(when (< (y pos) lo)
	  (setf (y pos) (+ lo (abs (- lo (y pos)))))
	  (setf (y vel) (* elast (- (y vel)))))
	(when (> (y pos) hi)
	  (setf (y pos) (- hi (abs (- hi (y pos)))))
	  (setf (y vel) (* elast (- (y vel)))))))
    ;;; update state
    (setf (velocity anim) vel)
    (translate-to (shape anim) pos)))

(defparameter *shape*
  (with-clear-and-redraw
    (setf *collision-padding* 0.2)
    (add-shape *scene* (make-square-shape 0.4))))

(set-animator *scene* (make-instance 'dynamics-animator :shape *shape* :elasticity 0.8))

(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene*
	       (scatter-group (lambda () (make-square-shape 0.1))
                              (random-points 10 (p! -0.8 -0.4) (p! 0.8 0.4))))))

(progn
  (clear-animators *scene*)
  (setf *collision-padding* 0.05)
  (do-hierarchy *shape*
    (lambda (s)
      (let ((start-pos (translate (transform s)))
            (start-vel (p-rand1 0.05)))
        (add-animator *scene*
                      (make-instance 'dynamics-animator
                                     :shape s
                                     :init-fn (lambda (anim)
                                                (translate-to (shape anim) start-pos)
                                                (setf (velocity anim) start-vel))))))
    :test #'is-leaf?))

(setf *gravity* (p! 0 0))

(setf *gravity* (p! 0 -.01))

(defun make-dynamic (shape &optional (start-vel-fn nil))
  (do-hierarchy shape
    (lambda (s)
      (let ((start-pos (translate (transform s)))
            (start-vel (if start-vel-fn
                           (funcall start-vel-fn)
                           (p! 0 0))))
        (add-animator *scene*
                      (make-instance 'dynamics-animator
                                     :shape s
                                     :init-fn (lambda (anim)
                                                (translate-to (shape anim) start-pos)
                                                (setf (velocity anim) start-vel))))))
  :test #'is-leaf?))

(progn
  (defparameter *shape*
    (with-clear-and-redraw
      (setf *collision-padding* 0.2)
      (add-shape *scene*
                 (make-cloud-group 1000
                                   (lambda () (make-circle-shape 0.4 16))
                                   (p! -0.8 0.0)
                                   (p! 0.8 0.8)
                                   :max-alpha 0.05
                                   :scale? nil))))
  (make-dynamic *shape* (lambda () (p-rand1 0.05))))

(setf *gravity* (p! 0 0))

(setf *wind* (p! .01 0))

;;; 10000 circles
