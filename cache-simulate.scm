;; ----------------------------
;; Super Simple Cache Simulator
;; Gavin Gray, 05.2022

(import (srfi :42) ;; eager-comprehensions
        (srfi :1)) ;; alist

;; Utils
(define (make-bit-vector n) (- (expt 2 n) 1))
(define land bitwise-and)
(define lor bitwise-ior)
(define lsr bitwise-arithmetic-shift-right)
(define lsl bitwise-arithmetic-shift-left)

(define-record-type cache
  (fields s e b S B (mutable tags)))

;; (define-cache <name> <s> <E> <b>)
(define-syntax define-cache
  (syntax-rules ()
    [(_ cname s e b)
     (define cname
       (let ((S (expt 2 s))
             (B (expt 2 b))
             (cold-block (list-ec (: i e) '(#f . #f))))
         (make-cache s e b S B
                     (vector-ec (: i S) cold-block))))]))

;; NOTE associations are ordered as
;; '(o_1 o_2 ... o_e) LRU -> MRU
(define (update-set-assoc assocs tag dsp hit?)
  (define ts (if hit?
                 (alist-delete tag assocs)
                 (cdr assocs)))
  (append ts (list (cons tag dsp))))

(define (display-cache c)
  (begin (format #t "Cache State~%")
         (let ((state (cache-tags c)))
           (do-ec (: i (vector-length state))
                  (let ((assocs (vector-ref state i)))
                    (format #t "~a: ~a~%" i assocs))))))

(define access!
  (let ((helper (lambda (cache a tag-display)
                  (let* ((set (land (make-bit-vector
                                     (cache-s cache))
                                    (lsr a (cache-b cache))))
                         (tag (lsr a (+ (cache-s cache)
                                        (cache-b cache))))
                         (tags (cache-tags cache))
                         (assocs (vector-ref tags set))
                         (hit? (assoc tag assocs))
                         (as (update-set-assoc
                              assocs tag tag-display hit?)))
                    (vector-set! tags set as)
                    (cache-tags-set! cache tags)
                    hit?))))
    (case-lambda [(cache a tag) (helper cache a tag)]
                 [(cache a) (helper cache a "")])))

(define (run-sequence! cache seq)
  (length (filter (lambda (a)
                    (let ((hit? (apply access! (cons cache a))))
                      ;; NOTE uncomment below for cache trace
                      (begin ;; (display-cache cache)
                             ;; (format #t "hit? ~a~%~%" hit?)
                             hit?))) seq)))

(define-syntax ref
  (syntax-rules ()
    [(_ accessor row col)
     (let ((a (+ (accessor offs)
                 (* (+ (* row array-len) col) struct-size)))
           (t (format #f "~a-~a-~a" row col (quote accessor))))
       (list a t))]))

(define (run! cache seq)
  (define hit-count (run-sequence! cache seq))
  (display-cache cache)
  (format #t "Hit rate: ~a~%~%" (/ hit-count (length seq))))

;; ----------------------------
;; Define how the cache is used
;; The following symbols must be declared
;; [1] offs        - struct of byte offsets
;; [2] struct-size - the total size in bytes of the struct
;; [3] array-len   - the length of the row (in case of matrix)

;; ---------------
;; Example C code
;; ---------------
;;
;; struct pair_t {
;;   double a;
;;   double b;
;;   uint64_t u [3];
;; };
;
;; void comp ( pair_t A [3][3]) {
;;   double t1 , t2 ;
;;
;;   for (int i = 0; i < 2; i ++) {
;;     for (int j = 0; j < 3; j ++) {
;;       t1 = A[i+1][j]. a;
;;       t2 = A[i][(j +1)%3]. a;
;;       A[i][j]. b = t1 + t2 ;
;;     }
;;   }
;;
;;   for (int i = 0; i < 2; i ++) {
;;     for (int j = 0; j < 3; j ++) {
;;       t1 = A [(j+1)%3][i]. a;
;;       t2 = A[j][ i +1]. a;
;;       A[j][i].b = t1 + t2 ;
;;     }
;;   }
;; };


;; Direct-mapped cache: 16 byte blocks, 128 byte total capacity, LRU replacement
;; Each struct has (double double double[3]) => 40 Bytes
(define-record-type pair-t (fields a b u0 u1 u2))
(define offs (apply make-pair-t (list-ec (: i 5) (* i 8))))
(define struct-size 40)
(define array-len 3)

(define-cache q2a 3 1 4)
(run! q2a
      (append-ec (: i 2)
                 (: j 3)
                 (list (ref pair-t-a (+ i 1) j)
                       (ref pair-t-a i (modulo (+ j 1) 3))
                       (ref pair-t-b i j))))
(run! q2a
      (append-ec (: i 2)
                 (: j 3)
                 (list (ref pair-t-a (modulo (+ j 1) 3) i)
                       (ref pair-t-a j (+ i 1))
                       (ref pair-t-b j i))))

;; Same cache specs with 2-way set associative
(define-cache q2b 2 2 4)
(run! q2b
      (append-ec (: i 2)
                 (: j 3)
                 (list (ref pair-t-a (+ i 1) j)
                       (ref pair-t-a i (modulo (+ j 1) 3))
                       (ref pair-t-b i j))))
(run! q2b
      (append-ec (: i 2)
                 (: j 3)
                 (list (ref pair-t-a (modulo (+ j 1) 3) i)
                       (ref pair-t-a j (+ i 1))
                       (ref pair-t-b j i))))
