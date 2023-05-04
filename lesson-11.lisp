#|
Lesson 11 -- Animation
|#

;;;(load "~/Development/kaveh-common-lisp-lessons/lesson-10.lisp")

#|
what CL needs -- IDE, web, GUI, ...
|#

;;;; point =====================================================================

(defmethod p-rand1 ((p point))
  (p! (rand1 (x p)) (rand1 (y p))))

(defmethod p-rand1 ((val number))
  (p! (rand1 val) (rand1 val)))

;;;; animator ==================================================================

(defclass animator ()
  ((init-fn :accessor init-fn :initarg :init-fn :initform nil)
   (update-fn :accessor update-fn :initarg :update-fn :initform nil)
   (is-initialized? :accessor is-initialized? :initarg :is-initialized? :initform nil)
   (shape :accessor shape :initarg :shape :initform nil)))

(defmethod init-animator ((anim animator))
  (when (init-fn anim)
    (funcall (init-fn anim) anim)))

(defmethod init-animator :after ((anim animator))
  (setf (is-initialized? anim) t))

(defmethod update-animator :before ((anim animator))
  (when (not (is-initialized? anim))
    (init-animator anim)))

(defmethod update-animator ((anim animator))
  (when (update-fn anim)
    (funcall (update-fn anim) anim)))

;;;; scene =====================================================================

;;; add animators & current-frame to scene
(defclass scene ()
  ((shapes :accessor shapes :initarg :shapes :initform '())
   (bg-color :accessor bg-color :initarg :bg-color :initform (c! 0 0 0 0))
   (renderer :accessor renderer :initarg :renderer :initform nil)
   (animators :accessor animators :initarg :animators :initform '())
   (current-frame :accessor current-frame :initarg :current-frame :initform 0)))

(defmethod set-animator ((scene scene) (anim animator))
  (setf (animators scene) (list anim))
  anim)

(defmethod add-animator ((scene scene) (anim animator))
  (push anim (animators scene))
  anim)

(defmethod clear-animators ((scene scene))
  (setf (animators scene) '())
  scene)

(defmethod init-scene ((self scene))
  (setf (current-frame self) 0)
  (mapc #'init-animator (animators self)))

(defmethod update-scene ((self scene))
  (incf (current-frame self))
  (mapc #'update-animator (animators self)))

;;;; device ====================================================================

;;; do not redraw on first click
(objc:defmethod (#/acceptsFirstMouse: :<BOOL>) ((self scene-view) event)
  (declare (ignore event))
  nil)

;;; update scene when mouse click
(objc:defmethod (#/mouseDown: :void) ((self scene-view) event)
  (declare (ignore event))
  (when (scene self)
    (update-scene (scene self)))
  (#/setNeedsDisplay: self t))

;;; accept key events
(objc:defmethod (#/acceptsFirstResponder :<BOOL>) ((self scene-view))
   t)

;;; update scene when space key pressed
(objc:defmethod (#/keyDown: :void) ((self scene-view) event)
  (let* ((str (objc:lisp-string-from-nsstring (#/charactersIgnoringModifiers event)))
         (char (char str 0)))
    (case char
          (#\a (when (scene self) (init-scene (scene self))))
          (#\space (when (scene self) (update-scene (scene self))))))
  (#/setNeedsDisplay: self t))

;;; test view refresh
(progn
  (defparameter *shape* (make-hexagons))
  (with-clear-and-redraw
    (add-shape *scene*  *shape*)
    (setf (bg-color *scene*) (c! 1 1 1))
    (set-shape-color *shape* (c! 0 0 0) :part :outline)
    (set-shape-color *shape* (c! .8 .8 .8 .5) :part :fill)))

;;; hold down spacebar to animate scene -- squiggly-renderer
(with-redraw
  (setf (renderer *scene*) (make-instance 'squiggly-renderer :iterations 4 :displacement 0.02)))))

(with-redraw
  (setf (renderer *scene*) nil))

(defparameter *shape*
  (with-clear-and-redraw
    (setf (bg-color *scene*) (c! 0 0 0))
    (add-shape *scene* (make-square-shape 1.0))))

;;; set rotation animator
(set-animator *scene*
              (make-instance 'animator
                             :shape *shape*
                             :update-fn (lambda (anim) (rotate-by (shape anim) 5))))

;;; set jitter points
(set-animator *scene*
              (make-instance 'animator
                             :shape *shape*
                             :update-fn (lambda (anim) (randomize-points (shape anim) (p! 0.05 0.05)))))

;;; add rotation animator -- layering animations
(add-animator *scene*
              (make-instance 'animator
                             :shape *shape*
                             :update-fn (lambda (anim) (rotate-by (shape anim) 5))))

(clear-animators *scene*)

;;; set both init and update functions -- press 'a' key to init scene
;;; rotation animator
(set-animator *scene*
              (make-instance 'animator
                             :shape *shape*
                             :init-fn (lambda (anim) (rotate-to (shape anim) 0))
                             :update-fn (lambda (anim) (rotate-by (shape anim) 5))))

;;; animate shapes in group
(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene*
	       (scatter-group (lambda () (make-square-shape 0.1))
                              (grid-points 5 5 (p! -0.4 -0.4) (p! 0.4 0.4))))))

(add-animator *scene*
              (make-instance 'animator
                             :shape *shape*
                             :init-fn (lambda (anim) (do-hierarchy (shape anim)
                                                       (lambda (s) (rotate-to s 0))
                                                       :test #'is-leaf?))
                             :update-fn (lambda (anim) (do-hierarchy (shape anim)
                                                         (lambda (s) (rotate-by s 5))
                                                         :test #'is-leaf?))))

(with-redraw
  (randomize-leaf-sizes *shape* 0.8 2.0))

(set-animator *scene*
              (make-instance 'animator
                             :shape *shape*
                             :update-fn (lambda (anim) (do-hierarchy (shape anim)
                                                         (lambda (s) (translate-by s (p-rand1 0.05)))
                                                         :test #'is-leaf?))))

;;; add-animator...

;;; create an animator for each leaf shape
(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene* (make-hexagons))))

(progn
  (clear-animators *scene*)
  (do-hierarchy *shape*
    (lambda (s)
      (let ((angle (rand2 0 5)))
        (add-animator *scene*
                      (make-instance 'animator
                                     :shape s
                                     :update-fn (lambda (anim) (rotate-by (shape anim) angle))))))
    :test #'is-leaf?))
