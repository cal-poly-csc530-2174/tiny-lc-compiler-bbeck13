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

;https://en.wikipedia.org/wiki/Whitespace_(programming_language://en.wikipedia.org/wiki/Whitespace_(programming_language)

(define (parse-file file) (print file))

(define (to-python-no-print lc) (match lc
                                  [(? number? n)
                                   (number->string lc)]
                                  [(? symbol? s)
                                   (symbol->string lc)]
                                  [`(λ (,id) ,body)
                                    (string-append "lambda " (to-python id) ": " (to-python body))]
                                  [`(+ ,fst ,snd)
                                    (string-append (to-python fst) " + " (to-python snd))]
                                  [`(* ,fst ,snd)
                                    (string-append (to-python fst) " * " (to-python snd))]
                                  [`(,fst ,snd)
                                    (string-append "(" (to-python fst) ")(" (to-python snd) ")")]
                                  [`(ifleq0 ,cnd ,then ,els)
                                    (string-append (to-python cnd) " <= 0 if "
                                                   (to-python then)
                                                   " else "
                                                   (to-python els))]
                                  [else (error (string-append (~a lc) " invalid syntax "))]))

(define (to-python lc) (match lc
                         [`(println ,str)
                           (string-append "print " (to-python-no-print str))]
                         [else (to-python-no-print lc)]))

(check-expect (to-python '(println 4)) "print 4")
(check-expect (to-python '(println 4)) "print 4")
(check-expect (to-python '(λ (x) (+ x 4))) "lambda x: x + 4")
(check-expect (to-python '(λ (x) (+ (ifleq0 x (+ 4 x) x) 4))) "lambda x: x <= 0 if 4 + x else x + 4")

(test)
