#lang racket
(require racket/cmdline)
(require test-engine/racket-tests)
(require racket/match)
(require control)

#|
   Usage:
      supply a file with lambda calculus to the cli and get a .py file!
|#

#|
   Braden Beck (bnbeck@calpoly.edu)
   CPE 430 assignment 1
   LC = num
      | id
      | (λ (id) LC)
      | (LC LC)
      | (+ LC LC)
      | (* LC LC)
      | (ifleq0 LC LC LC)
      | (println LC)
|#

;next step lambda calculus to this
;https://en.wikipedia.org/wiki/Whitespace_(programming_language://en.wikipedia.org/wiki/Whitespace_(programming_language)

;python printing function
(define print_f "__print_f")
(define println (string-append "def " print_f "(item):" "\n"
                               "\t" "print item" "\n"))

(define (python-id id)
  (match id
    [(? symbol? s)
     (symbol->string id)]
    [else (error (string-append "expected id but was " (~a id)))]))

(define (to-python lc)
  (match lc
    [(? number? n)
     (number->string lc)]
    [(? symbol? s)
     (symbol->string lc)]
    [`(λ (,id) ,body)
      (string-append "lambda " (python-id id) ": " (to-python body))]
    [`(+ ,fst ,snd)
      (string-append (to-python fst) " + " (to-python snd))]
    [`(* ,fst ,snd)
      (string-append (to-python fst) " * " (to-python snd))]
    [`(ifleq0 ,cnd ,then ,els)
      (string-append (to-python then) " if "
                     (to-python cnd) " <= 0"
                     " else "
                     (to-python els))]
    [`(println ,str)
      (string-append print_f "(" (to-python str) ")")]
    [`(,fst ,snd)
      (string-append "(" (to-python fst) ")(" (to-python snd) ")")]
    [else (error (string-append (~a lc) " invalid syntax"))]))

(define (lc->python lc) (string-append println (to-python lc) "\n"))

;lc file -> .py file
(local [(define cli (vector->list (current-command-line-arguments)))]
  (cond
    [(not (empty? cli))
     (local [(define in (open-input-file (car cli)))]
       (local [(define out (open-output-file (string-append (car (string-split (car cli) ".")) ".py")
                                             #:exists 'replace))]
         (begin
           (display println out)

           (let ([ln (read in)])
             (while (not (eof-object? ln))
                    (display (string-append (to-python ln) "\n") out)
                    (set! ln (read in))))

           (close-input-port in)
           (close-output-port out))))]
    [else (begin
            (display "No lambda calculus file to parse :(\n")
            (exit))]))

;tests
(define (tst-case t) (string-append println t "\n"))
(check-expect (lc->python '(println 4)) (string-append println print_f "(4)" "\n"))
(check-expect (lc->python '(λ (x) (+ x 4))) (tst-case "lambda x: x + 4"))
(check-expect (lc->python '(λ (x) (+ (ifleq0 x (+ 4 x) x) 4)))
              (tst-case "lambda x: 4 + x if x <= 0 else x + 4"))
(check-expect (lc->python '((λ (x) (+ x 4)) 4)) (tst-case "(lambda x: x + 4)(4)"))
(check-expect
  (lc->python '((λ (x) (ifleq0 x (println (+ x 3)) ((λ (y) (println y)) (* x 2))))  4))
  (tst-case
    "(lambda x: __print_f(x + 3) if x <= 0 else (lambda y: __print_f(y))(x * 2))(4)"))
(check-error (lc->python '(λ (x))) "(x) invalid syntax")
(check-error (python-id '4) "expected id but was 4")

(test)
