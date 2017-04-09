#lang racket
(require racket/cmdline)
(require test-engine/racket-tests)
(require racket/match)

#|
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

(define (parse-file file) (print file))

(display (current-command-line-arguments))
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
     (string-append (number->string lc))]
    [(? symbol? s)
     (string-append (symbol->string lc))]
    [`(λ (,id) ,body)
      (string-append "lambda " (python-id id) ": " (to-python body))]
    [`(+ ,fst ,snd)
      (string-append (to-python fst) " + " (to-python snd))]
    [`(* ,fst ,snd)
      (string-append (to-python fst) " * " (to-python snd))]
    [`(ifleq0 ,cnd ,then ,els)
      (string-append (to-python cnd) " <= 0 if "
                     (to-python then)
                     " else "
                     (to-python els))]
    [`(println ,str)
      (string-append print_f "(" (to-python str) ")")]
    [`(,fst ,snd)
      (string-append "(" (to-python fst) ")(" (to-python snd) ")")]
    [else (error (string-append (~a lc) " invalid syntax"))]))

(define (lc->python lc) (string-append println (to-python lc) "\n"))

(define (tst-case t) (string-append println t "\n"))

(check-expect (lc->python '(println 4)) (string-append println print_f "(4)" "\n"))
(check-expect (lc->python '(λ (x) (+ x 4))) (tst-case "lambda x: x + 4"))
(check-expect (lc->python '(λ (x) (+ (ifleq0 x (+ 4 x) x) 4)))
              (tst-case "lambda x: x <= 0 if 4 + x else x + 4"))
(check-expect (lc->python '((λ (x) (+ x 4)) 4)) (tst-case "(lambda x: x + 4)(4)"))
(check-expect
  (lc->python '((λ (x) (ifleq0 x (println (+ x 3)) ((λ (y) (println y)) (* x 2))))  4))
  (tst-case
    "(lambda x: x <= 0 if __print_f(x + 3) else (lambda y: __print_f(y))(x * 2))(4)"))
(check-error (lc->python '(λ (x))) "(x) invalid syntax")
(check-error (python-id '4) "expected id but was 4")

(test)
