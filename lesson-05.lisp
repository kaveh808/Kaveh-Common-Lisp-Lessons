#|
Lesson 05 -- Groups
|#

;;;; types ==============================================================

;;; variables don't have types, values have types
(defparameter *var* 4.0)
(type-of *var*)
(setf *var* "Hello!")
(type-of *var*)

;;;; utils ==============================================================

(defun print-spaces (num)
  (dotimes (i num)
    (princ " ")))

;;;; group ==============================================================

;;; class for managing hierarchies of shapes
(defclass group (shape)
  ((children :accessor children :initarg :children :initform '())))

(defmethod add-child ((self group) (s shape))
  (push s (children self))
  s)

(defmethod draw ((self group))
  (mapc #'draw (children self)))

(defun make-group (&rest shapes)
  (make-instance 'group :children shapes))

;; for diagnostics, we can print out the scene hierarchy
(defmethod print-hierarchy ((self scene) &optional (indent 0))
  (print-spaces indent)
  (format t "~a~%" self)
  (dolist (shape (shapes self))
    (print-hierarchy shape (+ indent 2))))

(defmethod print-hierarchy ((self shape) &optional (indent 0))
  (print-spaces indent)
  (format t "~a~%" self))

;;; use :after method
(defmethod print-hierarchy :after ((self group) &optional (indent 0))
  (dolist (child (children self))
    (print-hierarchy child (+ indent 2))))

;;; show polygon-shape number of points
(defmethod print-object ((self polygon-shape) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "[~a]" (length (points self)))))

;;; create a group and print hierarchy
(progn
  (defparameter *hexagon* (make-hexagon-shape 0.4))
  (defparameter *square* (make-square-shape 0.4))
  (defparameter *triangle* (make-eql-tri-shape 0.4)))

(progn
  (defparameter *group-1* (make-group *square* *triangle*))
  (print-hierarchy *group-1*))

(progn
  (defparameter *group-2* (make-group *group-1* *hexagon*))
  (print-hierarchy *group-2*))

(with-clear-and-redraw
  (add-shape *scene* *group-2*))

;;; add show-axis? slot to shape class
(defclass shape ()
  ((transform :accessor transform :initarg :transform :initform (make-instance 'transform))
   (show-axis? :accessor show-axis? :initarg :show-axis? :initform nil)))

;;; draw axis and pop matrix after drawing
(defmethod draw :after ((self shape))
  (when (show-axis? self)
    (draw-axis self))
  (#_glPopMatrix))

(defmethod draw-axis ((self shape))
  (#_glLineWidth 3.0)
  (#_glBegin #$GL_LINES)
  ;; x axis (red)
  (#_glColor3f 0.8 0.2 0.2)
  (#_glVertex3f 0.0 0.0 0.0)
  (#_glVertex3f 0.25 0.0 0.0)
  ;; y axis (green)
  (#_glColor3f 0.2 0.8 0.2)
  (#_glVertex3f 0.0 0.0 0.0)
  (#_glVertex3f 0.0 0.25 0.0)
  (#_glEnd))

;;; traverse hierarchy and execute function
(defmethod do-hierarchy ((self shape) func)
  (funcall func self))

(defmethod do-hierarchy :after ((self group) func)
  (dolist (child (children self))
    (do-hierarchy child func)))

(with-redraw
  (do-hierarchy *group-2* (lambda (s) (setf (show-axis? s) t))))

(with-redraw
  (translate-by *triangle* (p! 0.0 0.05)))

(with-redraw
  (rotate-by *triangle* 10.0))

(with-redraw
  (translate-by *square* (p! 0.0 -0.05)))

(with-redraw
  (translate-by *group-1* (p! 0.05 0.0)))

(with-redraw
  (rotate-by *group-1* 10.0))

(with-redraw
  (rotate-by *group-2* 10.0))

(with-redraw
  (scale-by *group-2* 0.9))

(with-redraw
  (translate-by *group-1* (p! 0.05 0.0)))

(with-redraw
  (translate-by *group-2* (p! -0.05 0.0)))

;;; parent shape to transformed group - inherits transformation
(progn
  (defparameter *pentagon* (make-pentagon-shape 0.3))
  (with-redraw
    (add-child *group-1* *pentagon*)))

;;; instancing -- *pentagon* is in two places in the hierarchy
(with-redraw
  (add-child *group-2* *pentagon*)
  (print-hierarchy *group-2*))

;;; transforms are in parent's frame of reference (i.e. coordinate system)
(with-redraw
  (translate-by *pentagon* (p! 0.0 0.1)))

(with-redraw
  (rotate-by *pentagon* 10.0))

;;; make robot arm
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
  (defparameter *waist* (make-group *waist-shape* *torso-shape* *shoulder*)))

(print-hierarchy *waist*)

(with-clear-and-redraw
  (add-shape *scene* *waist*)

  (translate-to *waist* (p! -0.6 -0.4))
  (translate-to *shoulder* (p! 0.4 0.8))
  (scale-to *shoulder* 0.3)
  (translate-to *elbow* (p! 1.8 0.0))
  (scale-to *elbow* 0.5)
  (translate-to *wrist* (p! 3.0 0.0))
  (scale-to *wrist* 0.6)

  (translate-to *torso-shape* (p! 0.0 0.4))
  (translate-to *upper-arm-shape* (p! 1.0 0.0))
  (scale-to *upper-arm-shape* (p! 1.05 0.5))
  (translate-to *lower-arm-shape* (p! 1.6 0.0))
  (scale-to *lower-arm-shape* (p! 2.1 0.5))
  (translate-to *hand-shape* (p! 1.2 0.0))
  (scale-to *hand-shape* (p! 1.5 1.2))
)

(with-redraw
  (do-hierarchy *waist* (lambda (s) (setf (show-axis? s) t))))

(with-redraw
  (rotate-by *shoulder* -10))

(with-redraw
  (rotate-by *elbow* 10))

(with-redraw
  (rotate-by *wrist* 10))

(with-redraw
  (rotate-by *waist* -10))

(defun reset-pose ()
  (rotate-to *waist* 0)
  (rotate-to *shoulder* 0)
  (rotate-to *elbow* 0)
  (rotate-to *wrist* 0))

(with-redraw
  (reset-pose))

(defun flex ()
  (rotate-by *waist* -1)
  (rotate-by *shoulder* -5)
  (rotate-by *elbow* 15)
  (rotate-by *wrist* 10))

(with-redraw
  (flex))

