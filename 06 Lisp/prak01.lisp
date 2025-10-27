;; a) Zahlen und Operationen
;; (print (- (* 10 (+ 4 3 3 1)) 20))
;; (print (42)) ;; Error: 42 is not a function
;; (print (pi)) ;; Error: pi is not a function
;; (print (/ 4 3)) ;; prints 4/3
;; (print (/ 12 3)) ;; prints 4
;; (print (/ 4 3.0)) ;; prints 1.3333333
;; (print (+ 0.5 1)) ;; prints 1.0
;; (print (* 35 1.16)) ;; prints 40.6
;; (print (sqrt 2)) ;; prints 1.4142135
;; (print (expt 12 32)) ;; prints 34182189187166852111368841966125056
;; (print (floor (/ 5 2))) ;; prints 2
;; (print (zerop 25)) ;; prints NIL
;; (print (zerop 0)) ;; prints T

;; b) Symbole und Listen
;; (print 'zitrone) ;; prints zitrone
;; (print (quote zitrone)) ;; prints zitrone
;; (print (list 'apfel 'banane 'zitrone)) ;; prints (APFEL BANANE ZITRONE)
;; (print '(apfel banane zitrone)) ;; prints (APFEL BANANE ZITRONE)
;; (print (car '(apfel banane zitrone))) ;; prints APFEL
;; (print (cdr '(apfel banane zitrone))) ;; prints (BANANE ZITRONE)
;; (print (caaddr '((apfel banane) zitrone (dattel erdbeere)))) ;; prints DATTEL
;; (print (length '((apfel banane) zitrone (dattel erdbeere)))) ;; prints 3
;; (print (append '(apfel banane zitrone) '(dattel erdbeere))) ;; prints (APFEL BANANE ZITRONE DATTEL ERDBEERE)
;; (print (cons 'zitrone '(apfel banane))) ;; prints (ZITRONE APFEL BANANE)
;; (print (reverse '(apfel banane zitrone))) ;; prints (ZITRONE BANANE APFEL)

;; c) car und cdr
;; ((w (x y)))
;; (print '((w (x y))))
;; (print (car '((w (x y)))))
;; (print (cdar '((w (x y)))))
;; (print (cdadar '((w (x y)))))

;; ((w u) y z)
;; (print '((w u) y z))
;; (print (cdr '((w u) y z)))
;; (print (cadr '((w u) y z)))

;; ((w (x) y) u)
;; (print '(((w (x) y) u)))
;; (print (car '((w (x) y) u)))
;; (print (caddar '((w (x) y) u)))

;; ((((y))) w)
;; (print '((((y))) w))
;; (print (caaaar '((((y))) w)))


;; 2.a. list-double

(defun dbl (x)
    (* 2 x))

(defun list-double (list)
    (mapcar (lambda (x) (* 2 x)) list))

(defun list-double-recursive (list)
    (if (null list)
        nil
        (cons (* 2 (car list)) (list-double-recursive (cdr list)))))
        
;; (print (list-double '(1 2 3 4)))
;; (print (list-double-recursive '(1 2 3 4)))


;; 2.b. sign
(defun sign (num)
    (cond ((> num 0) 1)
          ((= num 0) 0)
          ((< num 0) -1)))

;; (print (sign -62))


;; 2.c. list-sign
(defun list-sign (list)
    (mapcar 'sign list))


;; (print (list-sign '(5 2 -3 -1 0 3 -2)))


;; 3.

;; This method returns a new list where function f has been applied to each element of lst.
;; The recurisve call happens in the second part of the cons function, where we call map-list again with the rest of the list (cdr lst).
;; Funcall is used to call the function f

(defun map-list (f lst)
    (if (null lst) nil
    ;; else
    (cons (funcall f (car lst))
    (map-list f (cdr lst)))))

(defun sqr (x)
    (expt x 2))

(defun list-sqr (nums)
    (if (null nums) '() ;; fertig
    (cons (expt (car nums) 2)
    (list-sqr (cdr nums)))))


(print (map-list #'dbl '(1 2 3 4 5)))
(print (map-list #'sqr '(1 2 3 4 5)))
(print (map-list #'sign '(5 2 -3 -1 0 3 -2)))

;; 4.
(defun list-sum (nums)
    (if (null nums) 0
    ;; else
    (+ (car nums) (list-sum (cdr nums)))))

(defun list-mult (nums)
    (if (null nums) 1
    ;; else
    (* (car nums) (list-mult (cdr nums)))))

;; The functions distinguish between the + and * operations and the base case for an empty list.

(print (list-sum '(10 20 7)))
(print (list-mult '(10 20 7)))


(defun all-true (list)
    (if (null list) t
        (and (car list) (all-true (cdr list)))))

(print (all-true '()))
(print (all-true '(34 hallo (7))))
(print (all-true '(34 hallo ())))