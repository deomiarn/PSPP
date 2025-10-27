;;
;; Code Samples
;;


;;
;; 1. Reducing a list
;;

;;
;; Mapping over a list
;;
(defun map-list (f seq)
  (cond ((null seq) nil)
        (t (cons (funcall f (car seq))
                 (map-list f (cdr seq))))))

;; or as an iterative implementation
;(defun map-list (f seq) 
;    (loop for el in seq collect (funcall f el)))


;;
;; Sum of all list elements
;;
(defun list-sum (seq) 
  (cond ((null seq) 0)
        (t (+ (car seq) (list-sum (cdr seq))))))


;;
;; Product of all list elements
;;
(defun list-mult (seq) 
  (cond ((null seq) 1)
        (t (* (car seq) (list-mult (cdr seq))))))


;;
;; Abstraction
;;
(defun reduce-list (f init seq)
  ; ...
  init)


;;
;; Another frequently used list abstraction
;; Find a better name
;;
(defun func (f seq) 
  (cond ((null seq) nil)
        ((funcall f (car seq))
         (cons (car seq) (func f (cdr seq))))
        (t (func f (cdr seq)))))




;;
;; 2. Range function
;;

;; Create range of integers
;; This is not a working range function
;;
(defun range (&rest args)
  args)


;; Minimalist test tool
;; (flet, handler-case are not part of the PSPP topics)
;;
(defun run-tests (tests)
  (flet ((run-one (test)
           (handler-case (apply (first test) (second test))
             (error (c) (cadr (list c "throws error"))))))
    (cond ((null tests) "all ok")
          (t (let ((test (car tests)))
               (if (equal (run-one test) (third test))
                   (run-tests (cdr tests))
                   (format t "~S should be: ~S but is: ~S~%" 
                           (cons (first test) (second test))
                           (third test)
                           (run-one test))))))))


;; Some tests for range function
;;
(run-tests '(
  (range (0 5) 
      (0 1 2 3 4))
  (range (3 5) 
      (3 4))
  (range (0 0) 
      nil)
  (range (0 10 2) 
      (0 2 4 6 8))
  (range (10 30 5) 
      (10 15 20 25))
  (range (5)
      (0 1 2 3 4))
  (range (5 10 -1) 
      nil)
  (range (10 5 -1) 
      (10 9 8 7 6))
))




;;
;; Try to define factorial using range
;;

(defun factorial (n)
  ; ...
  1)   







