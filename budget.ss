#! /usr/local/Cellar/chezscheme/9.5.6/bin/chez --script

;; Gavin Gray, budget keeping command line tool

(define file "data/budget.datum")

(define amounts
  (let* ((p (open-input-file file))
         (dtm (get-datum p)))
    (close-input-port p)
    dtm))

;; --- actions ---

(define-syntax define-monthly
  (syntax-rules ()
    [(_ (action wallet args ...) body ...)
     (define (action wallet args ...)
       (rebuild `(monthly
                  ,(let ((wallet (monthly wallet)))
                     body ...)) wallet))]))

(define-monthly (reset wallet)
  (map (lambda (a)
         `(,(car a) 0 ,(caddr a))) wallet))

(define-monthly (spend wallet category amount)
  (define a (assoc category wallet))
  (define new-assoc `(,category ,(+ (cadr a) amount) ,(caddr a)))
  (rebuild new-assoc wallet))

(define (payoff wallet amount)
  (define d (debt wallet)) ;; payoffs don't reset the date
  (rebuild `(debt ,(- (car d) amount) ,(cadr d)) wallet))

(define (payout wallet)
  (define old-debt (car (debt wallet)))
  (define new-debt
    (fold-left (lambda (d a)
                 (+ d (apply - (cdr a))))
               old-debt (monthly wallet)))
  (when (< new-debt old-debt)
    (printf "You came in under budget!~%"))
  (rebuild `(debt ,new-debt ,(date-and-time)) (reset wallet)))

;; --- utilities ---

(define (monthly dtm)
  (cadr (assoc 'monthly dtm)))

(define (debt dtm)
  (cdr (assoc 'debt dtm)))

(define (rebuild new-a as)
  (map (lambda (a)
         (if (equal? (car a) (car new-a))
             new-a
             a)) as))

(define (display-amounts wallet)
  (define-values (d m)
    (values (debt wallet) (monthly wallet)))
  (printf "~%~10,,,'-s ~10a ~10,,,'-s ~%" '- "Budget Report" '-)
  (for-each (lambda (row)
              (printf "~10s ~10,2f ~10,2f~%"
                      (car row) (cadr row) (caddr row))) m)
  (printf "~%current debt: ~,2f~%last payout: ~a~%~%" (car d) (cadr d)))

(define (commit dtm)
  (let ((p (open-output-file file 'replace)))
    (display-amounts dtm)
    (put-datum p dtm)
    (flush-output-port p)
    (close-port p)))

(define (normalized-args)
  (define args (command-line))
   (values (eval (string->symbol (cadr args)))
           (map (lambda (v)
                  (define n (string->number v))
                  (if n
                      n
                      (string->symbol v))) (cddr args))))

(define-syntax run-cmd
  (syntax-rules ()
    [(_ vals)
     (call-with-values vals
       (lambda (f args)
         (commit (apply f (cons amounts args)))))]))

(run-cmd normalized-args)
