#|
Lesson 12 -- Animation & Hierarchies
|#

;;;(load "~/Development/kaveh-common-lisp-lessons/lesson-11.lisp")

;;;; point =====================================================================

(defun tween (val start end)
  (cond ((<= val start) 0.0)
	((>= val end) 1.0)
	(t (/ (- val start) (- end start)))))

(defmethod p-lerp (f (p1 point) (p2 point))
  (p! (lerp f (x p1) (x p2))
      (lerp f (y p1) (y p2))))

(defmethod p-lerp (f (v1 number) (v2 number))
  (lerp f v1 v2))

(defun cubic-smooth (x)
  (+ (* -2.0 (* x x x)) (* 3.0 (* x x))))

(defmethod p-smooth-lerp (f (p1 point) (p2 point))
  (p-lerp (cubic-smooth f) p1 p2))

(defmethod p-smooth-lerp (f (v1 number) (v2 number))
  (p-lerp (cubic-smooth f) v1 v2)))

;;;; utils =====================================================================

(defun get-alist-value (key alist)
  (cdr (assoc key alist)))

(defun add-alist-value (key value alist)
  (acons key value alist))

(defun set-alist-value (key value alist)
  (let ((pair (assoc key alist)))
    (if pair
	(rplacd pair value)
	(error "Key ~a not found in alist ~a~%" key alist)))
  alist)

#|
;;; association lists (aka alists)

(defparameter *my-alist* '((:foo . 1) (:bar . 2) (:baz . "hi there!")))

*my-alist*
(get-alist-value :bar *my-alist*)
(set-alist-value :foo 99 *my-alist*)
*my-alist*
(get-alist-value :qux *my-alist*)
(set-alist-value :qux :something *my-alist*)
(setf *my-alist* (add-alist-value :qux '(3 4 5) *my-alist*))
*my-alist*
|#

;;;; animator ==================================================================

;;; add data slot
(defclass animator ()
  ((init-fn :accessor init-fn :initarg :init-fn :initform nil)
   (update-fn :accessor update-fn :initarg :update-fn :initform nil)
   (is-initialized? :accessor is-initialized? :initarg :is-initialized? :initform nil)
   (shape :accessor shape :initarg :shape :initform nil)
   (data :accessor data :initarg :data :initform '())))

(defmethod anim-data ((anim animator) key)
  (get-alist-value key (data anim)))

;;; create an animator for each leaf shape
;;; varying rotations for each shape by setting animator data
(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene* (make-hexagons))))

(progn
  (clear-animators *scene*)
  (do-hierarchy *shape*
    (lambda (s)
      (let ((init-rotate (rotate (transform s))))
        (add-animator *scene*
                      (make-instance 'animator
                                     :shape s
                                     :init-fn (lambda (anim) (rotate-to (shape anim) init-rotate))
                                     :update-fn (lambda (anim)
                                                  (rotate-by (shape anim) (anim-data anim :rotate)))
                                     :data `((:rotate . ,(rand1 5)))))))
    :test #'is-leaf?))

(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene*
	       (scatter-group (lambda () (make-square-shape 0.1))
                              (grid-points 5 5 (p! -0.4 -0.4) (p! 0.4 0.4))))))

(progn
  (clear-animators *scene*)
  (do-hierarchy *shape*
    (lambda (s)
      (let ((start-pos (translate (transform s))))
        (add-animator *scene*
                      (make-instance 'animator
                                     :shape s
                                     :init-fn (lambda (anim) (translate-to (shape anim) start-pos))
                                     :update-fn (lambda (anim)
                                                  (translate-by (shape anim) (anim-data anim :move)))
                                     :data `((:move . ,(p-rand1 0.01))))))))
    :test #'is-leaf?))

;;; keyframe interpolation
(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene* (make-square-shape .5))))

(let ((start-pos (p! -.5 -.5))
      (end-pos (p! .5 .5))
      (start-frame 0)
      (end-frame 60))
  (set-animator *scene*
                (make-instance 'animator
                               :shape *shape*
                               :init-fn (lambda (anim) (translate-to (shape anim) start-pos))
                               :update-fn (lambda (anim)
                                            (translate-to *shape*
                                                          (p-smooth-lerp ;ease in/out -- compare p-lerp
                                                           (tween (current-frame *scene*)
                                                                  (anim-data anim :start-frame)
                                                                  (anim-data anim :end-frame))
                                                           (anim-data anim :start-pos)
                                                           (anim-data anim :end-pos))))
                               :data `((:start-frame . ,start-frame) (:start-pos . ,start-pos)
                                       (:end-frame   . ,end-frame  ) (:end-pos   . ,end-pos)))))

;;; smooth lerp curve - generate shape mathematically
(defun make-cubic-curve-shape (&optional (num-points 64))
  (let ((shape (make-instance 'polygon-shape :is-closed-shape? nil)))
    (dotimes (i (+ num-points 1))
      (let ((x (/ i num-points)))
        (add-point shape (p! x (cubic-smooth x)))))
    shape))

(with-clear-and-redraw
  (add-shape *scene* (make-cubic-curve-shape)))

;;; macro
(defmacro def-keyframe-animator (operator)
  `(defun ,(symcat 'make- operator '-animator) (shape start-val end-val start-frame end-frame
                                                &key (interpolation :smooth)) ; or :linear
     (make-instance 'animator
                    :shape shape
                    :init-fn (lambda (anim)
                               (,(symcat operator '-to) (shape anim) start-val))
                    :update-fn (lambda (anim)
                                 (,(symcat operator '-to) (shape anim)
                                  (funcall (if (eq interpolation :smooth)
                                               #'p-smooth-lerp
                                               #'p-lerp)
                                           (tween (current-frame *scene*)
                                                  (anim-data anim :start-frame)
                                                  (anim-data anim :end-frame))
                                           (anim-data anim :start-val)
                                           (anim-data anim :end-val))))
                 :data `((:start-frame . ,start-frame) (:start-val . ,start-val)
                         (:end-frame   . ,end-frame  ) (:end-val   . ,end-val)))))

(pprint (macroexpand-1 '(def-keyframe-animator rotate)))

(def-keyframe-animator translate) ;;; make-translate-animator
(def-keyframe-animator rotate)    ;;; make-rotate-animator
(def-keyframe-animator scale)     ;;; make-scale-animator

;;; demo
(defparameter *shape*
  (with-clear-and-redraw
    (add-shape *scene* (make-square-shape .5))))

(progn
  (clear-animators *scene*)
  (add-animator *scene* (make-translate-animator *shape* (p! -.5 -.5) (p! .5 .5)  0 60 :interpolation :smooth))
  (add-animator *scene* (make-rotate-animator    *shape* 0            90          0 60 :interpolation :smooth))
  (add-animator *scene* (make-scale-animator     *shape* (p! 1 1)     (p! 1.8 .5) 0 60 :interpolation :smooth)))

(progn
  (clear-animators *scene*)
  (add-animator *scene* (make-translate-animator *shape* (p! -.5 -.5) (p! .5 .5)  0 60 :interpolation :linear))
  (add-animator *scene* (make-rotate-animator    *shape* 0            90          0 60 :interpolation :linear))
  (add-animator *scene* (make-scale-animator     *shape* (p! 1 1)     (p! 1.8 .5) 0 60 :interpolation :linear)))

;;;; shape =====================================================================

;;; add name slot to shape class
(defclass shape ()
  ((transform :accessor transform :initarg :transform :initform (make-instance 'transform))
   (show-axis? :accessor show-axis? :initarg :show-axis? :initform nil)
   (name :accessor name :initarg :name :initform nil)))

;;; animate robot arm using keyframes
(progn
  (defparameter *waist-shape* (make-circle-shape 0.3))
  (defparameter *torso-shape* (make-pentagon-shape 1.0))
  (defparameter *shoulder-shape* (make-circle-shape 1.0 16))
  (defparameter *upper-arm-shape* (make-square-shape 1.0))
  (defparameter *elbow-shape* (make-circle-shape 1.0 16))
  (defparameter *lower-arm-shape* (make-square-shape 1.0))
  (defparameter *wrist-shape* (make-circle-shape 1.0 16))
  (defparameter *hand-shape* (make-square-shape 1.0))

  (defparameter *wrist* (make-group *wrist-shape* *hand-shape*))
  (defparameter *elbow* (make-group *elbow-shape* *lower-arm-shape* *wrist*))
  (defparameter *shoulder* (make-group *shoulder-shape* *upper-arm-shape* *elbow*))
  (defparameter *torso* (make-group *shoulder-shape* *upper-arm-shape* *elbow*))
  (defparameter *waist* (make-group *waist-shape* *torso-shape* *shoulder*))

  (with-clear-and-redraw
    (add-shape *scene* *waist*)

    (translate-to *waist* (p! -0.6 -0.4))
    (translate-to *shoulder* (p! 0.4 0.8))
    (scale-to *shoulder* (p! 0.3 0.3))
    (translate-to *elbow* (p! 1.8 0.0))
    (scale-to *elbow* (p! 0.5 0.5))
    (translate-to *wrist* (p! 3.0 0.0))
    (scale-to *wrist* (p! 0.6 0.6))

    (translate-to *torso-shape* (p! 0.0 0.4))
    (translate-to *upper-arm-shape* (p! 1.0 0.0))
    (scale-to *upper-arm-shape* (p! 1.05 0.5))
    (translate-to *lower-arm-shape* (p! 1.6 0.0))
    (scale-to *lower-arm-shape* (p! 2.1 0.5))
    (translate-to *hand-shape* (p! 1.2 0.0))
    (scale-to *hand-shape* (p! 1.5 1.2))
    ))

;;; assign names
(progn
  (setf (name *wrist*) 'wrist)
  (setf (name *elbow*) 'elbow)
  (setf (name *shoulder*) 'shoulder)
  (setf (name *torso*) 'torso)
  (setf (name *waist*) 'waist))

(defun animate-part-rotation (root name start-angle end-angle start-frame end-frame)
  (do-hierarchy root
    (lambda (s)
      (add-animator *scene*
                    (make-rotate-animator s start-angle end-angle start-frame end-frame
                                          :interpolation :smooth)))
    :test (lambda (s) (eq name (name s)))))

(defun make-flex-animation (num-frames)
  (clear-animators *scene*)
  (animate-part-rotation *waist* 'shoulder 0.0 -60.0 0 num-frames)
  (animate-part-rotation *waist* 'elbow    0.0 120.0 0 num-frames)
  (animate-part-rotation *waist* 'wrist    0.0  90.0 0 num-frames)
  (animate-part-rotation *waist* 'waist    0.0 -20.0 0 num-frames))

(make-flex-animation 100)

(make-flex-animation 50)

(make-flex-animation 10)

