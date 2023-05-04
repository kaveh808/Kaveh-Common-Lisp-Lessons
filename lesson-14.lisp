#|
Lesson 14 -- Particles
|#


;;;; particle-animator =========================================================

;;; updates one particle (shape)
(defclass particle-animator (dynamics-animator)
  ((expired? :accessor expired? :initarg :expired? :initform nil)
   (lifespan :accessor lifespan :initarg :lifespan :initform 100)
   (age :accessor age :initarg :age :initform 0)
   (start-color :accessor start-color :initarg :start-color :initform (c! 1 1 1 1))
   (end-color :accessor end-color :initarg :end-color :initform (c! 1 1 1 0))
   (start-size :accessor start-size :initarg :start-size :initform 1.0)
   (end-size :accessor end-size :initarg :end-size :initform 1.0)))

;;; after dynamics update
(defmethod update-animator :after ((anim particle-animator))
  (let ((shape (shape anim)))
    ;; update age and check if expired
    (if (> (incf (age anim)) (lifespan anim))
      (setf (expired? anim) t))
    (let ((relative-age (tween (age anim) 0 (lifespan anim))))
      ;; update color
      (set-shape-color shape (c-lerp relative-age (start-color anim) (end-color anim)))
      ;; update size
      (scale-to shape (lerp relative-age (start-size anim) (end-size anim))))))

;;;; particle-system ========================================================

;;; updates entire particle system (i.e. group)
(defclass particle-system (animator)
  ((animators :accessor animators :initarg :animators :initform '())
   (spawn-rate :accessor spawn-rate :initarg :spawn-rate :initform 1)
   (spawn-shape-fn :accessor spawn-shape-fn :initarg :spawn-shape-fn :initform nil)
   (collisions? :accessor collisions? :initarg :collisions? :initform t)
   (elasticity :accessor elasticity :initarg :elasticity :initform 0.75)
   (min-position :accessor min-position :initarg :min-position :initform (p! 0 0))
   (max-position :accessor max-position :initarg :max-position :initform (p! 0 0))
   (min-velocity :accessor min-velocity :initarg :min-velocity :initform (p! -.05 -.05))
   (max-velocity :accessor max-velocity :initarg :max-velocity :initform (p!  .05  .05))
   (min-lifespan :accessor min-lifespan :initarg :min-lifespan :initform 40)
   (max-lifespan :accessor max-lifespan :initarg :max-lifespan :initform 50)
   (start-color :accessor start-color :initarg :start-color :initform (c! 1 1 1 1))
   (end-color :accessor end-color :initarg :end-color :initform (c! 1 1 1 0))
   (start-size :accessor start-size :initarg :start-size :initform 1.0)
   (end-size :accessor end-size :initarg :end-size :initform 1.0))
  (:default-initargs
   :shape (make-group)
   :update-fn #'update-particle-system))

(defmethod update-particle-system ((psys particle-system))
  ;;; remove expired animators and particles
  (dolist (anim (copy-list (animators psys)))
    (when (expired? anim)
      (setf (animators psys) (delete anim (animators psys)))
      (setf (children (shape psys)) (delete (shape anim) (children (shape psys))))))
  ;;; add new particles
  (when (spawn-shape-fn psys)
    (spawn-particles psys (spawn-rate psys)))
  ;;; update particles
  (mapc #'update-animator (animators psys)))

(defmethod spawn-particles ((psys particle-system) num)
  (dotimes (i num)
    (let* ((shape (funcall (spawn-shape-fn psys)))
           (anim (make-instance 'particle-animator
                                :shape shape
                                :collisions? (collisions? psys)
                                :elasticity (elasticity psys)
                                :lifespan (rand2 (min-lifespan psys) (max-lifespan psys))
                                :velocity (p-rand2 (min-velocity psys) (max-velocity psys))
                                :start-color (start-color psys)
                                :end-color (end-color psys)
                                :start-size (start-size psys)
                                :end-size (end-size psys))))
      ;;; add spawned shape to children of shape
      (push shape (children (shape psys)))
      ;;; add animator to self
      (push anim (animators psys))
      ;;; set shape properties
      (translate-to shape (p-rand2 (min-position psys) (max-position psys)))
      (set-shape-color shape (start-color anim))
      (set-shape-color shape 0.0 :part :outline :component :alpha)))
  psys)

;;; test
(with-clear-and-redraw
  (setf *collision-padding* .05)
  (let ((psys (make-instance 'particle-system
                             :spawn-rate 10
                             :spawn-shape-fn (lambda () (make-circle-shape .1 16)))))
    (add-shape *scene* (shape psys))
    (set-animator *scene* psys)))
	      
;;; fountain
(with-clear-and-redraw
  (setf *collision-padding* 0.025)
  (setf *wind* (p! .0 0))
  (let ((psys (make-instance 'particle-system
                             :spawn-rate 20
                             :spawn-shape-fn (lambda () (make-circle-shape 0.05 16))
                             :collisions? nil
                             :min-position (p! 0 0)
                             :max-position (p! 0 0)
                             :min-velocity (p! -.01 .10)
                             :max-velocity (p!  .02 .15)
                             :start-color (c! .2 .2 1 .5)
                             :end-color (c! 1 1 1 0)
                             :start-size 1.0
                             :end-size 4.0
                             :min-lifespan 10
                             :max-lifespan 20)))
    (add-shape *scene* (shape psys))
    (set-animator *scene* psys)))

;;; mist
(with-clear-and-redraw
  (setf *collision-padding* 0.125)
  (let ((psys (make-instance 'particle-system
                             :spawn-rate 100
                             :spawn-shape-fn (lambda () (make-circle-shape 0.25 16))
                             :elasticity 0.5
                             :min-position (p! -0.8 0.6)
                             :max-position (p!  0.8 0.8)
                             :min-velocity (p! -.05 -.05)
                             :max-velocity (p!  .05  .05)
                             :start-color (c! 1 1 1 0.01)
                             :end-color (c! 1 1 1 0.0)
                             :start-size 1.0
                             :end-size 4.0
                             :min-lifespan 50
                             :max-lifespan 60)))
    (add-shape *scene* (shape psys))
    (set-animator *scene* psys)))

;;; gas
(with-clear-and-redraw
  (defparameter *gravity* (p! 0 0))
  (setf *collision-padding* 0.125)
  (let ((psys (make-instance 'particle-system
                             :spawn-rate 10
                             :spawn-shape-fn (lambda () (make-circle-shape 0.25 16))
                             :min-position (p! 0 0)
                             :max-position (p! 0 0)
                             :min-velocity (p! -.05 -.05)
                             :max-velocity (p!  .05  .05)
                             :start-color (c! .2 .8 .2 0.05)
                             :end-color (c! .2 .8 .2 0.0)
                             :start-size 1.0
                             :end-size 8.0
                             :min-lifespan 100
                             :max-lifespan 100)))
    (add-shape *scene* (shape psys))
    (set-animator *scene* psys)))

;;; fire
(with-clear-and-redraw
  (setf *gravity* (p! 0 .002))
  (let ((psys (make-instance 'particle-system
                             :collisions? nil
                             :spawn-rate 50
                             :spawn-shape-fn (lambda () (make-circle-shape 0.25 16))
                             :min-position (p! -0.9 -0.9)
                             :max-position (p!  0.9 -0.7)
                             :min-velocity (p! -.05  0)
                             :max-velocity (p!  .05  .05)
                             :start-color (c! 1 .8 0 0.05)
                             :end-color (c! 1 0 0 0.0)
                             :start-size 1.0
                             :end-size 0.0
                             :min-lifespan 50
                             :max-lifespan 60)))
    (add-shape *scene* (shape psys))
    (set-animator *scene* psys)))
    
(with-redraw
  (let ((shape (first (shapes *scene*))))
    (translate-to shape (p! 0 0))
    (rotate-to shape 45)
    (scale-to shape (p! .25 .5))))
