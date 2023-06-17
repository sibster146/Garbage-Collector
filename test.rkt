#lang plai/gc2/mutator


(allocator-setup "gc-2-1.rkt" 200)

#|
(define (count-down n)
  (cond ((zero? n) 0)
        (else (count-down (- n 1)))))
(define (loop n)
  (if (zero? n) 'passed
      (begin (count-down 20)
             (loop (- n 1)))))
(loop 1000)



(define (mk-list n)
  (cond ((zero? n) '())
        (else (cons n (mk-list (- n 1))))))
(define (forever n)
  (if (zero? n) 'passed (begin (mk-list 10) (forever (- n 1)))))
(forever 1000)




(define (proc-lst n)
  (cond
   ((zero? n) (lambda () 0))
   (else (let ((n1 (proc-lst (- n 1)))) (lambda () (+ (n1) n))))))
(define (forever n)
  (if (zero? n) 'passed (begin ((proc-lst 10)) (forever (- n 1)))))
(forever 1000)



(define (build-one)
  (let* ((x0 13)
         (x1 (cons #f #f))
         (x2 (lambda (x) (if (= x 0) x0 x1)))
         (x3 (cons #f #f))
         (x4 (cons x1 #f))
         (x5 (cons x3 #f))
         (x6
          (lambda (x)
            (if (= x 0)
              x4
              (if (= x 1)
                x2
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x5
                    (if (= x 4)
                      x4
                      (if (= x 5)
                        x1
                        (if (= x 6)
                          x5
                          (if (= x 7) x5 (if (= x 8) x2 x4)))))))))))
         (x7 #t)
         (x8 'cons)
         (x9 (lambda (x) (if (= x 0) x6 (if (= x 1) x1 x2)))))
    (set-first! x1 x1)
    (set-rest! x1 x7)
    (set-first! x3 x9)
    (set-rest! x3 x4)
    (set-rest! x4 x5)
    (set-rest! x5 x9)
    x8))
(define (traverse-one x8) (symbol=? 'cons x8))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 'flat)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x0
                        (if (= x 6)
                          x0
                          (if (= x 7) x0 (if (= x 8) x0 x0)))))))))))
         (x2 'cons)
         (x3
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1) x2 (if (= x 2) x1 (if (= x 3) x0 x0))))))
         (x4
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x2
                    (if (= x 4)
                      x1
                      (if (= x 5) x3 (if (= x 6) x0 (if (= x 7) x3 x2))))))))))
         (x5 (cons x4 x4))
         (x6 (cons x4 #f))
         (x7 3)
         (x8 'clos))
    (set-rest! x6 x7)
    x8))
(define (traverse-one x8) (symbol=? 'clos x8))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)





(define (build-one)
  (let* ((x0 156) (x1 (lambda (x) x0)) (x2 (cons x0 x1)) (x3 4) (x4 4)) x2))
(define (traverse-one x2) (= 156 ((rest x2) 0)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 'free)
         (x1 (cons #f #f))
         (x2 (cons #f #f))
         (x3
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x1
                (if (= x 2)
                  x2
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x0
                        (if (= x 6)
                          x2
                          (if (= x 7) x2 (if (= x 8) x2 x0)))))))))))
         (x4 (cons #f x2))
         (x5
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1)
                x0
                (if (= x 2)
                  x2
                  (if (= x 3)
                    x4
                    (if (= x 4)
                      x1
                      (if (= x 5)
                        x2
                        (if (= x 6)
                          x0
                          (if (= x 7) x0 (if (= x 8) x3 x0)))))))))))
         (x6 (cons #f x2)))
    (set-first! x1 x1)
    (set-rest! x1 x6)
    (set-first! x2 x6)
    (set-rest! x2 x6)
    (set-first! x4 x6)
    (set-first! x6 x6)
    x5))
(define (traverse-one x5) (symbol=? 'free ((x5 8) 3)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 'free)
         (x1 (cons x0 #f))
         (x2 'clos)
         (x3
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1)
                x2
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x2
                    (if (= x 4) x0 (if (= x 5) x1 (if (= x 6) x0 x1)))))))))
         (x4 (cons x2 #f))
         (x5 (lambda (x) (if (= x 0) x0 x2)))
         (x6
          (lambda (x)
            (if (= x 0)
              x4
              (if (= x 1)
                x2
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x2
                    (if (= x 4)
                      x3
                      (if (= x 5)
                        x1
                        (if (= x 6)
                          x1
                          (if (= x 7) x5 (if (= x 8) x1 x3)))))))))))
         (x7 (lambda (x) (if (= x 0) x5 (if (= x 1) x3 x2))))
         (x8
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1) x7 (if (= x 2) x7 (if (= x 3) x6 x0)))))))
    (set-rest! x1 x2)
    (set-rest! x4 x8)
    x8))
(define (traverse-one x8)
  (symbol=? 'free ((((rest (((rest ((x8 3) 0)) 3) 0)) 3) 4) 4)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 13)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x0
                    (if (= x 4) x0 (if (= x 5) x0 (if (= x 6) x0 x0)))))))))
         (x2 13)
         (x3 12)
         (x4 (cons x2 #f))
         (x5 (lambda (x) (if (= x 0) x2 (if (= x 1) x4 x3))))
         (x6 0)
         (x7 0)
         (x8
          (lambda (x)
            (if (= x 0)
              x7
              (if (= x 1)
                x1
                (if (= x 2)
                  x4
                  (if (= x 3)
                    x6
                    (if (= x 4)
                      x3
                      (if (= x 5)
                        x4
                        (if (= x 6) x4 (if (= x 7) x2 x5)))))))))))
    (set-rest! x4 x5)
    x4))
(define (traverse-one x4) (= 13 (first x4)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)




(define (build-one)
  (let* ((x0 4)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x0 (if (= x 2) x0 (if (= x 3) x0 x0))))))
         (x2 (lambda (x) (if (= x 0) x1 x0)))
         (x3 (cons #f x2))
         (x4 52)
         (x5
          (lambda (x)
            (if (= x 0)
              x4
              (if (= x 1) x2 (if (= x 2) x4 (if (= x 3) x4 x1))))))
         (x6
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1)
                x3
                (if (= x 2)
                  x1
                  (if (= x 3) x5 (if (= x 4) x1 (if (= x 5) x1 x4))))))))
         (x7 'cons)
         (x8 4)
         (x9
          (lambda (x)
            (if (= x 0)
              x7
              (if (= x 1)
                x6
                (if (= x 2)
                  x0
                  (if (= x 3) x4 (if (= x 4) x0 (if (= x 5) x5 x4)))))))))
    (set-first! x3 x5)
    x0))
(define (traverse-one x0) (= 4 x0))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one) (let* ((x0 13) (x1 (lambda (x) (if (= x 0) x0 x0)))) x1))
(define (traverse-one x1) (= 13 (x1 0)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)




(define (build-one)
  (let* ((x0 39) (x1 (lambda (x) x0)) (x2 (cons #f x0)) (x3 (cons #f x2)))
    (set-first! x2 x2)
    (set-first! x3 x3)
    x2))
(define (traverse-one x2) (= 39 (rest (first x2))))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 'flat)
         (x1 (cons x0 #f))
         (x2 2)
         (x3 (cons #f #f))
         (x4 (lambda (x) (if (= x 0) x2 x3)))
         (x5 0)
         (x6 (cons #f x4))
         (x7 (lambda (x) (if (= x 0) x2 (if (= x 1) x6 (if (= x 2) x2 x6)))))
         (x8 4)
         (x9 1))
    (set-rest! x1 x6)
    (set-first! x3 x9)
    (set-rest! x3 x3)
    (set-first! x6 x9)
    x9))
(define (traverse-one x9) (= 1 x9))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)

(define (build-one)
  (let* ((x0 'cons)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x0
                        (if (= x 6) x0 (if (= x 7) x0 x0)))))))))))
    x1))
(define (traverse-one x1) (symbol=? 'cons (x1 6)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)

(define (build-one)
  (let* ((x0 13)
         (x1 (lambda (x) (if (= x 0) x0 (if (= x 1) x0 (if (= x 2) x0 x0)))))
         (x2 4)
         (x3
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1)
                x1
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x2
                    (if (= x 4)
                      x2
                      (if (= x 5) x2 (if (= x 6) x0 (if (= x 7) x2 x1))))))))))
         (x4 39)
         (x5 (cons x3 x2))
         (x6 13))
    x2))
(define (traverse-one x2) (= 4 x2))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 13) (x1 (cons #f #f)) (x2 (lambda (x) x1)) (x3 3))
    (set-first! x1 x1)
    (set-rest! x1 x1)
    x0))
(define (traverse-one x0) (= 13 x0))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)




(define (build-one)
  (let* ((x0 'cons)
         (x1 2)
         (x2 (cons x1 #f))
         (x3
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1) x2 (if (= x 2) x0 (if (= x 3) x2 x2))))))
         (x4 'clos)
         (x5 (cons x0 x3))
         (x6 39)
         (x7
          (lambda (x)
            (if (= x 0)
              x6
              (if (= x 1) x3 (if (= x 2) x4 (if (= x 3) x2 x0))))))
         (x8 (lambda (x) (if (= x 0) x5 (if (= x 1) x4 (if (= x 2) x4 x0))))))
    (set-rest! x2 x5)
    x1))
(define (traverse-one x1) (= 2 x1))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)

(define (build-one)
  (let* ((x0 #f)
         (x1 13)
         (x2 'cons)
         (x3 0)
         (x4
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x1
                    (if (= x 4) x1 (if (= x 5) x1 (if (= x 6) x0 x2)))))))))
         (x5 #t)
         (x6 (cons #f x4))
         (x7
          (lambda (x)
            (if (= x 0)
              x6
              (if (= x 1)
                x2
                (if (= x 2)
                  x4
                  (if (= x 3)
                    x6
                    (if (= x 4)
                      x1
                      (if (= x 5)
                        x0
                        (if (= x 6)
                          x0
                          (if (= x 7) x0 (if (= x 8) x6 x0))))))))))))
    (set-first! x6 x6)
    x5))
(define (traverse-one x5) (let ((res x5)) (if (boolean? res) res #f)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 13)
         (x1 (lambda (x) (if (= x 0) x0 (if (= x 1) x0 (if (= x 2) x0 x0)))))
         (x2 (lambda (x) (if (= x 0) x1 (if (= x 1) x0 (if (= x 2) x0 x1)))))
         (x3 (cons x0 x0)))
    x2))
(define (traverse-one x2) (= 13 ((x2 0) 1)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)

(define (build-one)
  (let* ((x0 4)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x0
                        (if (= x 6)
                          x0
                          (if (= x 7) x0 (if (= x 8) x0 x0)))))))))))
         (x2
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x0
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x0
                        (if (= x 6)
                          x1
                          (if (= x 7) x0 (if (= x 8) x0 x0))))))))))))
    x1))
(define (traverse-one x1) (= 4 (x1 1)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 156)
         (x1 2)
         (x2 (cons #f #f))
         (x3 (cons #f x2))
         (x4 1)
         (x5 (cons x0 x2))
         (x6 'flat)
         (x7 (lambda (x) (if (= x 0) x1 (if (= x 1) x3 x3)))))
    (set-first! x2 x5)
    (set-rest! x2 x5)
    (set-first! x3 x3)
    x6))
(define (traverse-one x6) (symbol=? 'flat x6))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 'clos)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2) x0 (if (= x 3) x0 (if (= x 4) x0 x0)))))))
         (x2 (cons #f #f))
         (x3 (cons #f #f))
         (x4 (cons #f x1))
         (x5
          (lambda (x)
            (if (= x 0)
              x3
              (if (= x 1)
                x3
                (if (= x 2)
                  x3
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x2
                      (if (= x 5) x3 (if (= x 6) x1 (if (= x 7) x1 x0))))))))))
         (x6 4)
         (x7 2))
    (set-first! x2 x4)
    (set-rest! x2 x7)
    (set-first! x3 x4)
    (set-rest! x3 x7)
    (set-first! x4 x5)
    x4))
(define (traverse-one x4) (= 2 (rest ((first x4) 4))))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)



(define (build-one) (let* ((x0 'cons)) x0))
(define (traverse-one x0) (symbol=? 'cons x0))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 52) (x1 (cons #f #f))) (set-first! x1 x1) (set-rest! x1 x1) x0))
(define (traverse-one x0) (= 52 x0))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 0)
         (x1 (cons #f #f))
         (x2 #f)
         (x3 'forward)
         (x4 (cons x0 #f))
         (x5 12)
         (x6 (cons #f x1)))
    (set-first! x1 x6)
    (set-rest! x1 x1)
    (set-rest! x4 x6)
    (set-first! x6 x6)
    x3))
(define (traverse-one x3) (symbol=? 'forward x3))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 'cons)
         (x1 0)
         (x2
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x1
                (if (= x 2) x0 (if (= x 3) x0 (if (= x 4) x1 x0)))))))
         (x3 (lambda (x) (if (= x 0) x1 (if (= x 1) x0 x2))))
         (x4 (cons x2 #f))
         (x5 'to)
         (x6 (lambda (x) (if (= x 0) x5 (if (= x 1) x0 (if (= x 2) x0 x5)))))
         (x7 3))
    (set-rest! x4 x7)
    x3))
(define (traverse-one x3) (= 0 (x3 0)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 3) (x1 (lambda (x) x0)) (x2 52) (x3 52) (x4 156)) x3))
(define (traverse-one x3) (= 52 x3))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 'cons)
         (x1 (cons x0 x0))
         (x2 156)
         (x3
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x2
                (if (= x 2) x2 (if (= x 3) x2 (if (= x 4) x2 x1)))))))
         (x4 (cons #f x2))
         (x5 (cons x2 #f))
         (x6
          (lambda (x)
            (if (= x 0)
              x3
              (if (= x 1)
                x4
                (if (= x 2) x0 (if (= x 3) x4 (if (= x 4) x5 x5)))))))
         (x7 (cons #f x5))
         (x8 (cons x4 x6))
         (x9 'cons))
    (set-first! x4 x4)
    (set-rest! x5 x8)
    (set-first! x7 x9)
    x1))
(define (traverse-one x1) (symbol=? 'cons (rest x1)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)



(define (build-one)
  (let* ((x0 'flat) (x1 (cons #f #f)))
    (set-first! x1 x1)
    (set-rest! x1 x1)
    x0))
(define (traverse-one x0) (symbol=? 'flat x0))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)



(define (build-one)
  (let* ((x0 'cons)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x0 (if (= x 2) x0 (if (= x 3) x0 x0))))))
         (x2 (cons x0 x0)))
    x1))
(define (traverse-one x1) (symbol=? 'cons (x1 1)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)



(define (build-one)
  (let* ((x0 0)
         (x1 (lambda (x) (if (= x 0) x0 (if (= x 1) x0 (if (= x 2) x0 x0)))))
         (x2 (cons x0 x0))
         (x3 52)
         (x4
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1)
                x3
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x2
                      (if (= x 5) x1 (if (= x 6) x3 (if (= x 7) x2 x1))))))))))
         (x5
          (lambda (x)
            (if (= x 0)
              x3
              (if (= x 1)
                x3
                (if (= x 2) x1 (if (= x 3) x1 (if (= x 4) x3 x4))))))))
    x5))
(define (traverse-one x5) (= 0 (((x5 5) 5) 2)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 'to) (x1 (lambda (x) x0)) (x2 (cons x0 x0))) x1))
(define (traverse-one x1) (symbol=? 'to (x1 0)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)



(define (build-one)
  (let* ((x0 156)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x0 (if (= x 2) x0 (if (= x 3) x0 x0)))))))
    x1))
(define (traverse-one x1) (= 156 (x1 4)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 2)
         (x1 (cons #f x0))
         (x2
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x1
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x1
                    (if (= x 4) x0 (if (= x 5) x0 (if (= x 6) x0 x1)))))))))
         (x3 2)
         (x4 'free))
    (set-first! x1 x3)
    x4))
(define (traverse-one x4) (symbol=? 'free x4))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 'free)
         (x1 (cons x0 #f))
         (x2 2)
         (x3 (cons x1 #f))
         (x4
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1) x0 (if (= x 2) x0 (if (= x 3) x2 x3))))))
         (x5 (lambda (x) (if (= x 0) x1 x2)))
         (x6
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x5
                (if (= x 2)
                  x4
                  (if (= x 3) x2 (if (= x 4) x0 (if (= x 5) x0 x0)))))))))
    (set-rest! x1 x6)
    (set-rest! x3 x6)
    x6))
(define (traverse-one x6) (= 2 ((rest ((x6 1) 0)) 3)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)



(define (build-one)
  (let* ((x0 1)
         (x1 52)
         (x2 39)
         (x3 (cons #f x1))
         (x4 (cons x3 #f))
         (x5
          (lambda (x)
            (if (= x 0)
              x4
              (if (= x 1) x1 (if (= x 2) x1 (if (= x 3) x4 x2))))))
         (x6 (cons #f x3)))
    (set-first! x3 x3)
    (set-rest! x4 x4)
    (set-first! x6 x6)
    x0))
(define (traverse-one x0) (= 1 x0))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)



(define (build-one) (let* ((x0 39) (x1 (cons x0 #f))) (set-rest! x1 x1) x0))
(define (traverse-one x0) (= 39 x0))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)



(define (build-one)
  (let* ((x0 'clos)
         (x1 (cons #f x0))
         (x2
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x1 (if (= x 2) x0 (if (= x 3) x1 x1))))))
         (x3 39)
         (x4 4)
         (x5 'clos)
         (x6 (lambda (x) (if (= x 0) x0 (if (= x 1) x2 x0))))
         (x7 (cons x1 x2)))
    (set-first! x1 x4)
    x5))
(define (traverse-one x5) (symbol=? 'clos x5))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 'cons)
         (x1 (cons #f #f))
         (x2
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1) x1 (if (= x 2) x1 (if (= x 3) x0 x1))))))
         (x3 'clos))
    (set-first! x1 x3)
    (set-rest! x1 x3)
    x2))
(define (traverse-one x2) (symbol=? 'cons (x2 3)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 1)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x0
                    (if (= x 4) x0 (if (= x 5) x0 (if (= x 6) x0 x0)))))))))
         (x2 'cons)
         (x3 (cons #f x0))
         (x4 (lambda (x) (if (= x 0) x0 x0)))
         (x5 26)
         (x6
          (lambda (x)
            (if (= x 0)
              x3
              (if (= x 1)
                x0
                (if (= x 2) x4 (if (= x 3) x1 (if (= x 4) x1 x1)))))))
         (x7 'to)
         (x8 'forward)
         (x9
          (lambda (x)
            (if (= x 0)
              x3
              (if (= x 1)
                x8
                (if (= x 2)
                  x6
                  (if (= x 3)
                    x3
                    (if (= x 4) x8 (if (= x 5) x0 (if (= x 6) x1 x1))))))))))
    (set-first! x3 x5)
    x9))
(define (traverse-one x9) (symbol=? 'forward (x9 1)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 0)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x0
                        (if (= x 6)
                          x0
                          (if (= x 7) x0 (if (= x 8) x0 x0)))))))))))
         (x2 (cons x1 #f))
         (x3 (cons #f #f))
         (x4 (lambda (x) (if (= x 0) x0 (if (= x 1) x0 x2))))
         (x5 (cons x1 x4))
         (x6 (cons x1 x1)))
    (set-rest! x2 x4)
    (set-first! x3 x3)
    (set-rest! x3 x6)
    x4))
(define (traverse-one x4) (= 0 ((first (x4 2)) 2)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 3) (x1 39) (x2 (cons x0 #f))) (set-rest! x2 x2) x1))
(define (traverse-one x1) (= 39 x1))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 26) (x1 156) (x2 (cons #f x1)) (x3 (cons #f x0)))
    (set-first! x2 x2)
    (set-first! x3 x3)
    x3))
(define (traverse-one x3) (= 26 (rest (first (first (first x3))))))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 #t)
         (x1 (cons #f #f))
         (x2
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x0
                        (if (= x 6)
                          x1
                          (if (= x 7) x0 (if (= x 8) x1 x1)))))))))))
         (x3
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x2
                (if (= x 2)
                  x1
                  (if (= x 3) x2 (if (= x 4) x2 (if (= x 5) x1 x2))))))))
         (x4 #f)
         (x5 13)
         (x6 2)
         (x7 (cons x2 x3)))
    (set-first! x1 x2)
    (set-rest! x1 x4)
    x5))
(define (traverse-one x5) (= 13 x5))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)

(define (build-one)
  (let* ((x0 #t)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3) x0 (if (= x 4) x0 (if (= x 5) x0 x0))))))))
         (x2
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x0 (if (= x 2) x1 (if (= x 3) x1 x0))))))
         (x3 4)
         (x4 (cons x0 x2))
         (x5 2)
         (x6 (cons #f x1))
         (x7
          (lambda (x)
            (if (= x 0)
              x4
              (if (= x 1)
                x2
                (if (= x 2)
                  x2
                  (if (= x 3)
                    x5
                    (if (= x 4)
                      x5
                      (if (= x 5)
                        x3
                        (if (= x 6)
                          x2
                          (if (= x 7) x2 (if (= x 8) x4 x6)))))))))))
         (x8 (lambda (x) (if (= x 0) x2 (if (= x 1) x2 x4)))))
    (set-first! x6 x8)
    x7))
(define (traverse-one x7) (let ((res ((x7 1) 4))) (if (boolean? res) res #f)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)

(define (build-one)
  (let* ((x0 'forward)
         (x1 3)
         (x2
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x0 (if (= x 2) x0 (if (= x 3) x1 x1))))))
         (x3 2)
         (x4 (lambda (x) (if (= x 0) x3 x2)))
         (x5 (cons x3 x4))
         (x6 'clos)
         (x7 (lambda (x) (if (= x 0) x3 (if (= x 1) x1 (if (= x 2) x5 x5))))))
    x0))
(define (traverse-one x0) (symbol=? 'forward x0))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one) (let* ((x0 0)) x0))
(define (traverse-one x0) (= 0 x0))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)

|#

(define (build-one)
  (let* ((x0 52)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x0
                      (if (= x 5) x0 (if (= x 6) x0 (if (= x 7) x0 x0))))))))))
         (x2 (cons #f #f))
         (x3 'free)
         (x4 (lambda (x) (if (= x 0) x2 (if (= x 1) x2 (if (= x 2) x0 x3)))))
         (x5 'free)
         (x6 (lambda (x) (if (= x 0) x0 x5)))
         (x7 (cons x5 x2))
         (x8 2)
         (x9
          (lambda (x)
            (if (= x 0)
              x5
              (if (= x 1)
                x7
                (if (= x 2) x2 (if (= x 3) x5 (if (= x 4) x7 x1))))))))
    (set-first! x2 x8)
    (set-rest! x2 x6)
    x9))
(define (traverse-one x9) (symbol=? 'free (first (x9 4))))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)

#|
(define (build-one)
  (let* ((x0 12)
         (x1 (cons #f #f))
         (x2 (cons x0 #f))
         (x3 (lambda (x) (if (= x 0) x2 x2)))
         (x4
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x0 (if (= x 2) x0 (if (= x 3) x0 x3))))))
         (x5 52)
         (x6 (cons #f x5))
         (x7 (cons #f x6)))
    (set-first! x1 x2)
    (set-rest! x1 x4)
    (set-rest! x2 x2)
    (set-first! x6 x6)
    (set-first! x7 x7)
    x1))
(define (traverse-one x1) (= 12 (first (first x1))))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)

(define (build-one)
  (let* ((x0 4)
         (x1 (lambda (x) (if (= x 0) x0 x0)))
         (x2 39)
         (x3 (lambda (x) (if (= x 0) x2 (if (= x 1) x2 (if (= x 2) x2 x1)))))
         (x4 1)
         (x5 (cons x0 x3))
         (x6 'cons)
         (x7 (lambda (x) x1))
         (x8 (cons x5 x1)))
    x4))
(define (traverse-one x4) (= 1 x4))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)

(define (build-one)
  (let* ((x0 'flat) (x1 (cons x0 #f)) (x2 (cons x0 #f)) (x3 #t))
    (set-rest! x1 x3)
    (set-rest! x2 x3)
    x1))
(define (traverse-one x1) (symbol=? 'flat (first x1)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)


(define (build-one)
  (let* ((x0 2)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3) x0 (if (= x 4) x0 (if (= x 5) x0 x0))))))))
         (x2 1)
         (x3
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1) x2 (if (= x 2) x0 (if (= x 3) x2 x0))))))
         (x4 4))
    x3))
(define (traverse-one x3) (= 1 (x3 1)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)



(define (build-one)
  (let* ((x0 4)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3) x0 (if (= x 4) x0 (if (= x 5) x0 x0))))))))
         (x2 26)
         (x3
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x1
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x2
                        (if (= x 6)
                          x0
                          (if (= x 7) x1 (if (= x 8) x0 x0)))))))))))
         (x4
          (lambda (x)
            (if (= x 0)
              x3
              (if (= x 1)
                x2
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x1
                        (if (= x 6)
                          x2
                          (if (= x 7) x3 (if (= x 8) x2 x3)))))))))))
         (x5
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1)
                x3
                (if (= x 2)
                  x4
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x2
                      (if (= x 5)
                        x2
                        (if (= x 6)
                          x0
                          (if (= x 7) x3 (if (= x 8) x1 x1))))))))))))
    x5))
(define (traverse-one x5) (= 4 ((x5 2) 3)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 500)

|#
