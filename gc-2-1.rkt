#lang plai/gc2/collector

(print-only-errors)

(define eight-principles
  (list
   "Know your rights."
   "Acknowledge your sources."
   "Protect your work."
   "Avoid suspicion."
   "Do your own work."
   "Never falsify a record or permit another person to do so."
   "Never fabricate data, citations, or experimental results."
   "Always tell the truth when discussing your work with your instructor."))




#|
heap:    | 'free | 'free | 'free | ...                          NEW!
flat:    ... | 'flat | <payload>   | ...
pair:    ... | 'cons | <first-ptr> | <rest-ptr> | ...
closure: ... | 'clos | <code-ptr> | <n-free-vars> | <fv0> | <fv1> | ... | ...  
|#

(define (init-allocator)
  (when (< (heap-size) 12)
    (error 'init-allocator "Heap to small: ~a" (heap-size)))
  (define oneHeapSize (quotient(-(heap-size) 4)2))
  (for ([i (range 4 (+ 4 oneHeapSize))])
    (heap-set! i 'free))
  (for ([j (range (+ 4 oneHeapSize)(heap-size))])
    (heap-set! j 'to))
  (heap-set! 0 4)
  (heap-set! 1 (+ 4 oneHeapSize))
  (heap-set! 2 (+ 4 oneHeapSize))
  (heap-set! 3 (+ 4 oneHeapSize)))

;; malloc : size -> address
(define (malloc n root1 root2)
  (define address (find-free-spaces n (heap-ref 0)))
  (cond [(integer? address)
         (heap-set! 0 (+ address n))
         address]
        [else
         (collect-garbage root1 root2)
         (define address (find-free-spaces n (heap-ref 0)))
         (cond [(integer? address)
                (heap-set! 0 (+ address n))
                address]
               [else (error 'malloc "out of memory")])]))




;; find-free-space : int? int? -> (or/c location false?)
(define (find-free-spaces n start)
  (if (>= start (heap-ref 1))
      #f
      (case (heap-ref start)
        [(free) (if (n-free-spaces? n start)
                    start
                    (find-free-spaces n (+ start 1)))]
        [(flat) (find-free-spaces n (+ start 2))]
        [(cons) (find-free-spaces n (+ start 3))]
        [(clos) (find-free-spaces
                 n
                 (+ start 3 (heap-ref (+ start 2))))]
        [else (error 'find-free-spaces "unexpected tag @ ~a" start)])))


;; n-free-spaces? : int? int? -> boolean?
(define (n-free-spaces? n start)
  (cond
    [(= n 0)
     #t]
    [(>= start (heap-ref 1))
     #f]
    [(equal? (heap-ref start) 'free)
     (n-free-spaces? (- n 1) (+ start 1))]
    [else
     #f]))


(define (collect-garbage root1 root2)
  (define oneHeapSize (quotient(-(heap-size) 4)2))
  (when (equal? (heap-ref 1) (heap-size))
    ;to free
    (validate-from-heap (+ 4 oneHeapSize))
    (validate-to-heap 4))
  (when (equal? (heap-ref 1)(+ 4 oneHeapSize))
    ;free to
    (validate-from-heap 4)
    (validate-to-heap (+ 4 oneHeapSize)))
  
  (traverse/roots (get-root-set))
 
  
  (define res1 (traverse/roots root1))
  (define res2 (traverse/roots root2))

  (traverse-rest-roots)
  (update-ptrs)
  
  (when (equal? (heap-ref 1)(+ 4 oneHeapSize))
    (validate-from-heap 4)
    (validate-to-heap (+ 4 oneHeapSize)))
  (when (equal? (heap-ref 1) (heap-size))
    (validate-from-heap (+ 4 oneHeapSize))
    (validate-to-heap 4)))




(define (update-ptrs)
  (define oneHeapSize (quotient(-(heap-size) 4)2))
  (cond
    [(equal? (heap-ref 1) (+ 4 oneHeapSize))
     (heap-set! 0 (heap-ref 3))
     (heap-set! 1 (heap-size))
     (for ([i (range (heap-ref 3)(heap-size))])
       (heap-set! i 'free))
     (for ([j (range 4(+ 4 oneHeapSize))])
       (heap-set! j 'to))
     (heap-set! 2 4)
     (heap-set! 3 4)]
    
    [(equal? (heap-ref 1)(heap-size))
     (heap-set! 0 (heap-ref 3))
     (heap-set! 1 (+ 4 oneHeapSize))
     (for ([i (range (heap-ref 3)(+ 4 oneHeapSize))])
       (heap-set! i 'free))
     (for ([j (range (+ 4 oneHeapSize)(heap-size))])
       (heap-set! j 'to))
     (heap-set! 2 (+ 4 oneHeapSize))
     (heap-set! 3 (+ 4 oneHeapSize))]))
     

(define (traverse-rest-roots)
  (define left-ptr (heap-ref 2))
  (define right-ptr (heap-ref 3))
  (unless (equal? left-ptr right-ptr)
    (case (heap-ref left-ptr)
      [(cons)(traverse-rest-loc (+ left-ptr 1))
             (traverse-rest-loc (+ left-ptr 2))
             (heap-set! 2 (+ 3 left-ptr))
             (traverse-rest-roots)]
      [(flat)(heap-set! 2 (+ 2 left-ptr))
             (traverse-rest-roots)]
      [(clos)(define n (heap-ref (+ left-ptr 2)))
             (for ([i (in-range n)])
               (traverse-rest-loc (+ left-ptr 3 i)))
             (heap-set! 2 (+ left-ptr 3 n))
             (traverse-rest-roots)]
      [(free)(heap-set! 2 (+ left-ptr 1))
             (traverse-rest-roots)]
      [else (error 'traverse-rest-roots "unexpected tag @ ~a" left-ptr)])))


(define (traverse-rest-loc to-ptr)
  (define ptr (heap-ref to-ptr))
  (case (heap-ref ptr)
    [(forward)(heap-set! to-ptr (heap-ref (+ ptr 1)))
              (heap-ref (+ ptr 1))]
    [(cons)(define right-ptr (heap-ref 3))
           ;copying to to-heap
           (heap-set! right-ptr 'cons)
           (heap-set! (+ 1 right-ptr) (heap-ref (+ ptr 1)))
           (heap-set! (+ 2 right-ptr) (heap-ref (+ ptr 2)))
           ;update in to-heap
           (heap-set! to-ptr right-ptr)
           ;update in from-heap
           (heap-set! ptr 'forward)
           (heap-set! (+ ptr 1) right-ptr)
           ;update right pointer
           (heap-set! 3 (+ right-ptr 3))]    
    [(flat)(define right-ptr (heap-ref 3))
           ;copying to to-heap
           (heap-set! right-ptr 'flat)
           (heap-set! (+ right-ptr 1) (heap-ref (+ ptr 1)))
           ;update in to-heap
           (heap-set! to-ptr right-ptr)
           ;update in from-heap
           (heap-set! ptr 'forward)
           (heap-set! (+ ptr 1) right-ptr)
           ; update right pointer
           (heap-set! 3 (+ right-ptr 2))]
    [(clos)(define right-ptr (heap-ref 3))
           (define n (heap-ref (+ ptr 2)))
           ;copying to to-heap
           (heap-set! right-ptr 'clos)
           (heap-set! (+ right-ptr 1) (heap-ref (+ ptr 1)))
           (heap-set! (+ right-ptr 2) n)
           (for ([i (in-range n)])
             (heap-set! (+ right-ptr 3 i) (heap-ref (+ ptr 3 i))))
           ;update in to-heap
           (heap-set! to-ptr right-ptr)
           ;update in from-heap
           (heap-set! ptr 'forward)
           (heap-set! (+ ptr 1) right-ptr)
           ;update right pointer
           (heap-set! 3 (+ right-ptr 3 n))]
    [(free) (error 'traverse-rest-loc "dangling pointer")]
    [else (error 'traverse-rest-loc "unexpected tag @ ~a" ptr)]))
           
 
 

(define (traverse/roots root1)
  (cond [(false? root1)
         false]   
        [(list? root1)
         (cond [(empty? root1)false]
               [else
                (for ([r (in-list root1)])
                  (traverse/loc (read-root r) r))
                true])]
        [(root? root1)
         (traverse/loc (read-root root1)root1)
         true]
        [else
         (error 'traverse/roots "unexpected root: ~a" root1)]))
  

(define (traverse/loc ptr root)
  (case (heap-ref ptr)
    [(forward)(define right-ptr (heap-ref 3))
           (define forw-ptr (heap-ref (+ ptr 1)))
           (case (heap-ref forw-ptr)
             [(flat);copy everything to to-heap
                    (heap-set! right-ptr 'flat)
                    (heap-set! (+ right-ptr 1) (heap-ref (+ forw-ptr 1)))
                    ;update new right pointer
                    (heap-set! 3 (+ right-ptr 2))
                    ;update root
                    (set-root! root right-ptr)]
             [(cons);copy everything to to-heap
                    (heap-set! right-ptr 'cons)
                    (heap-set! (+ right-ptr 1) (heap-ref (+ forw-ptr 1)))
                    (heap-set! (+ right-ptr 2) (heap-ref (+ forw-ptr 2)))
                    ;update new right pointer
                    (heap-set! 3 (+ right-ptr 3))
                    ;update root
                    (set-root! root right-ptr)]
             [(clos);copy everything to the to-heap
                    (define n-ds (heap-ref (+ forw-ptr 2)))
                    (heap-set! right-ptr 'clos)
                    (heap-set! (+ right-ptr 1) (heap-ref (+ forw-ptr 1)))
                    (heap-set! (+ right-ptr 2) (heap-ref (+ forw-ptr 2)))
                    (for ([i (in-range n-ds)])
                      (heap-set! (+ right-ptr 3 i) (heap-ref (+ forw-ptr i 3))))
                    ;update new right pointer
                    (heap-set! 3 (+ right-ptr 3 n-ds))
                    ;update root
                    (set-root! root right-ptr)])]
    [(cons)(define right-ptr (heap-ref 3))
           ;copying everything to to-heap
           (heap-set! right-ptr 'cons)
           (heap-set! (+ right-ptr 1) (heap-ref (+ ptr 1)))
           (heap-set! (+ right-ptr 2) (heap-ref (+ ptr 2)))
           ;putting in forward pointer
           (heap-set! ptr 'forward)
           (heap-set! (+ ptr 1) right-ptr)
           ;update new right pointer
           (heap-set! 3 (+ right-ptr 3))
           ;updating root
           (set-root! root right-ptr)]
    [(flat)(define right-ptr (heap-ref 3))
           ;copying everything to to-heap
           (heap-set! right-ptr 'flat)
           (heap-set! (+ right-ptr 1) (heap-ref (+ ptr 1)))
           ;putting in forward pointer
           (heap-set! ptr 'forward)
           (heap-set! (+ ptr 1) right-ptr)
           ;updating right pointer
           (heap-set! 3 (+ 2 right-ptr))
           ;possible updating root
           (set-root! root right-ptr)]
    [(clos) (define right-ptr (heap-ref 3))
            (define n-ds (heap-ref (+ ptr 2)))
            ;copying everything to to-heap
            (heap-set! right-ptr 'clos)
            (heap-set! (+ right-ptr 1) (heap-ref (+ 1 ptr)))
            (heap-set! (+ right-ptr 2) n-ds)
            (for ([i (in-range n-ds)])
              (heap-set! (+ right-ptr 3 i) (heap-ref (+ ptr 3 i))))
            ;putting in forward pointer
            (heap-set! ptr 'forward)
            (heap-set! (+ ptr 1) right-ptr)
            ;updating right pointer
            (heap-set! 3 (+ right-ptr 3 n-ds))
            ;possible updating root
            (set-root! root right-ptr)]
    [(free)(error 'traverse/loc "dangling pointer")]
    [else (error 'traverse/loc "unexpected tag @ ~a" ptr)]))
    






(define (validate-pointer a)
  (define ptr (heap-ref a))
  (unless (and (integer? ptr)
               (ptr . >= . 0)
               (ptr . < . (heap-ref 1))
               (member (heap-ref ptr) '(flat cons clos)))
    (error 'validate-pointer "invalid pointer @ ~a" a)))

(define (validate-to-heap start)
  (define oneHeapSize (quotient(-(heap-size) 4)2))
  (define end 0)
  (when (equal? (heap-ref 1)(+ 4 oneHeapSize))
    (set! end (heap-size)))
  (when (equal? (heap-ref 1)(heap-size))
    (set! end (+ 4 oneHeapSize)))
  
  (unless (>= start end)
    (case (heap-ref start)
      [(to) (validate-to-heap (+ start 1))]
      [else (error 'validate-to-heap "unexpected tag @ ~a" start)])))

(define (validate-from-heap start)
  (define oneHeapSize (quotient(-(heap-size) 4)2))
  (define end 0)
  (when (equal? (heap-ref 1)(+ 4 oneHeapSize))
    (set! end (+ 4 oneHeapSize)))
  (when (equal? (heap-ref 1)(heap-size))
    (set! end (heap-size)))
  (unless (>= start end)
    (case (heap-ref start)
      [(flat) (validate-from-heap (+ start 2))]
      [(cons) (validate-pointer (+ start 1))
              (validate-pointer (+ start 2))
              (validate-from-heap (+ start 3))]
      [(clos) (for ([i (in-range (heap-ref (+ start 2)))])
                (validate-pointer (+ start 3 i)))
              (validate-from-heap (+ start 3 (heap-ref (+ start 2))))]
      [(free) (validate-from-heap (+ start 1))]
      [else (error 'validate-from-heap "unexpected tag @ ~a" start)])))
  




#|
flat:    ... | 'flat | <payload> | ...
|#
;; gc:alloc-flat : flat-value -> location
(define (gc:alloc-flat value)
  (define address (malloc 2 #f #f))
  (heap-set! address 'flat)
  (heap-set! (+ address 1) value)
  address)
;; gc:flat? : location -> boolean
(define (gc:flat? address)
  (equal? (heap-ref address) 'flat))
;; gc:deref : location -> flat-value
(define (gc:deref address)
  (unless (gc:flat? address)
    (error 'gc:deref "expected flat @ ~a" address))
  (heap-ref (+ address 1)))


#|
pair:    ... | 'cons | <first-ptr> | <rest-ptr> | ...
|#

;; gc:cons : root root -> location
(define (gc:cons root1 root2)
  (define oneHeapSize (quotient(-(heap-size) 4)2))
  (define address (malloc 3 root1 root2))
  (heap-set! address 'cons)
  (heap-set! (+ address 1) (read-root root1))
  (heap-set! (+ address 2) (read-root root2))
  address)
;; gc:cons? : location -> boolean
(define (gc:cons? address)
  (equal? (heap-ref address) 'cons))
;; gc:first : location -> location
(define (gc:first address)
  (unless (gc:cons? address)
    (error 'gc:first "expected cons @ ~a" address))
  (heap-ref (+ address 1)))
;; gc:rest : location -> location
(define (gc:rest address)
  (unless (gc:cons? address)
    (error 'gc:rest "expected cons @ ~a" address))
  (heap-ref (+ address 2)))
;; gc:set-first! : location location -> void
(define (gc:set-first! address new-value-address)
  (unless (gc:cons? address)
    (error 'gc:set-first! "expected cons @ ~a" address))
  (heap-set! (+ address 1) new-value-address))
;; gc:set-rest! : location location -> void
(define (gc:set-rest! address new-value-address)
  (unless (gc:cons? address)
    (error 'gc:set-rest! "expected cons @ ~a" address))
  (heap-set! (+ address 2) new-value-address))


#|
closure: ... | 'clos | <code-ptr> | <n-free-vars> | <fv0> | <fv1> | ... | ...
|#
;; gc:closure : opaque-value (listof root) ->  location
(define (gc:closure code-ptr free-vars)
  (define address (malloc (+ 3 (length free-vars))
                          free-vars #f))
  (heap-set! address 'clos)
  (heap-set! (+ address 1) code-ptr)
  (heap-set! (+ address 2) (length free-vars))
  (for ([i  (in-range (length free-vars))]
        [fv (in-list free-vars)])
    (heap-set! (+ address 3 i) (read-root fv)))
  address)
;; gc:closure? :  location -> boolean
(define (gc:closure? address)
  (equal? (heap-ref address) 'clos))
;; gc:closure-code-ptr : location -> opaque-value
(define (gc:closure-code-ptr address)
  (unless (gc:closure? address)
    (error 'gc:closure-code-ptr "expected closure @ ~a" address))
  (heap-ref (+ address 1)))
;; gc:closure-env-ref : location integer -> location
(define (gc:closure-env-ref address i)
  (unless (gc:closure? address)
    (error 'gc:closure-env-ref "expected closure @ ~a" address))
  (heap-ref (+ address 3 i)))



#|


(test (let ([h (vector 'x 'x 'x 'x 'x 'x 'x 'x 'x 'x 'x 'x)]) ; size 12
        (with-heap h
          (init-allocator)
          h))
      (vector 4 8 8 8
              'free 'free 'free 'free
              'to 'to 'to 'to))



(test (let ([h (vector 'x 'x 'x 'x 'x 'x 'x 'x 'x 'x 'x 'x 'x 'x 'x 'x 'x 'x)]) ; size 12
        (with-heap h
          (init-allocator)
          (define f (gc:alloc-flat 5))
          (define g (gc:alloc-flat 6))
          (gc:cons (simple-root f)(simple-root g))
          h))
      (vector 11 11 11 11
              'flat 5 'flat 6 'cons 4 6
              'to 'to 'to 'to 'to 'to 'to))


(test (let ([h (vector 'x 'x 'x 'x 'x 'x 'x 'x
                       'x 'x 'x 'x 'x 'x 'x 'x
                       'x 'x 'x 'x 'x 'x 'x 'x)]) ; size 24
        (with-heap h
          (init-allocator)
          (define f (gc:alloc-flat 5))
          (define g (gc:alloc-flat 6))
          (define i (gc:alloc-flat 7))
          (define j(gc:cons (simple-root f)(simple-root g)))
       ;   (gc:cons (simple-root f)(simple-root j))
          h))
      (vector 13 14 14 14
              'flat 5 'flat 6 'flat 7 'cons 4 6 'free
              'to 'to 'to 'to 'to 'to 'to 'to 'to 'to))



(test (let ([h (vector 'x 'x 'x 'x 'x 'x 'x 'x
                       'x 'x 'x 'x 'x 'x 'x 'x
                       'x 'x 'x 'x 'x 'x 'x 'x)]) ; size 24
        (with-heap h
          (init-allocator)
          (define f (gc:alloc-flat 5))
          (define g (gc:alloc-flat 6))
          (define i (gc:alloc-flat 7))
          (define j(gc:cons (simple-root f)(simple-root g)))
          (gc:cons (simple-root f)(simple-root j))
          h))
      (vector 24 24 4 4
              'to 'to 'to 'to 'to 'to 'to 'to 'to 'to
              'flat 5 'cons 14 19 'flat 6 'cons 14 16))




(test (let ([h (vector 14 24 4 4
                       'to 'to 'to 'to 'to 'to 'to 'to 'to 'to
                       'free 'free 'free 'free 'free 'free 'free 'free 'free 'free)]) ; size 24
        (with-heap h
          (define f (gc:alloc-flat 5))
          (define g (gc:alloc-flat 6))
          (define i (gc:alloc-flat 7))
          (define j(gc:cons (simple-root f)(simple-root g)))
        ;  (gc:cons (simple-root f)(simple-root j))
          h))
      (vector 23 24 4 4
              'to 'to 'to 'to 'to 'to 'to 'to 'to 'to
              'flat 5 'flat 6 'flat 7 'cons 14 16 'free))



(test (let ([h (vector 14 24 4 4
                       'to 'to 'to 'to 'to 'to 'to 'to 'to 'to
                       'free 'free 'free 'free 'free 'free 'free 'free 'free 'free)]) ; size 24
        (with-heap h
          (define f (gc:alloc-flat 5))
          (define g (gc:alloc-flat 6))
          (define i (gc:alloc-flat 7))
          (define j(gc:cons (simple-root f)(simple-root g)))
          (gc:cons (simple-root f)(simple-root j))
          h))
      (vector 14 14 14 14
              'flat 5 'cons 4 9 'flat 6 'cons 4 6
              'to 'to 'to 'to 'to 'to 'to 'to 'to 'to))
        
         


(test (let ([h (vector 'x 'x 'x 'x
                       'x 'x 'x 'x
                       'x 'x 'x 'x)]) ; size 24
        (with-heap h
          (init-allocator)
          (define f (gc:alloc-flat 5))
          (define g (gc:alloc-flat 6))
          h))
      (vector 8 8 8 8
              'flat 5 'flat 6
              'to 'to 'to 'to))


(test (let ([h (vector 'x 'x 'x 'x
                       'x 'x 'x 'x
                       'x 'x 'x 'x)]) ; size 24
        (with-heap h
          (init-allocator)
          (define f (gc:alloc-flat 5))
          (define g (gc:alloc-flat 6))
          (gc:alloc-flat 7)
          h))
      (vector 10 12 4 4
              'to 'to 'to 'to
              'flat 7 'free 'free))

|#


