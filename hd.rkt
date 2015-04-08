(require racket/gui/base) ; to have TV
(require plot/no-gui) ; to have plot/dc
;(require racket/draw) ; to draw
;(require racket/math) ; to draw arc
;(require math) ; to have mean
(require math/base) ; to have sum
(plot-new-window? #t)

(define-struct automaton (init-claim hh hd dh dd))

(define all-hawks (make-automaton 1 1 1 1 1))
(define x1 (make-automaton 1 1 1 1 0))
(define pavlov (make-automaton 1 0 1 0 1))
(define x5 (make-automaton 1 0 1 1 1))
(define x6 (make-automaton 1 0 1 1 1))
(define tit-for-tat (make-automaton 0 1 0 1 0))
(define x9 (make-automaton 1 0 1 1 0))
(define x10 (make-automaton 0 0 1 1 0))
(define all-doves (make-automaton 0 0 0 0 0))


(define contestants
  (list all-hawks x1 all-hawks pavlov x5 x6
        tit-for-tat tit-for-tat x9 x10 all-doves))

(define contestants-2
  (list pavlov x5 x5 all-hawks x1 all-hawks x1 x1 x1 x9 all-doves
        tit-for-tat tit-for-tat))

(define (contest automaton contestant-list)
  (sum
   (map first
   (map (lambda (x) (take-sums (match-pair (list automaton x) 200)))
       contestant-list))))

;; 1 = playing hawk
;; 0 = playing dove

(define (identify automaton)
  (map (lambda (f) (f automaton))
       (list
        automaton-init-claim
        automaton-hh
        automaton-hd
        automaton-dh
        automaton-dd)))

(define (next-claim automaton previous-claims)
  (let ([look-up
         (cond
          [(equal? previous-claims '(1 1)) automaton-hh]
          [(equal? previous-claims '(1 0)) automaton-hd]
          [(equal? previous-claims '(0 1)) automaton-dh]
          [(equal? previous-claims '(0 0)) automaton-dd])])
    (look-up automaton)))

(define (match-claims claims)
  (cond [(equal? claims '(1 1)) (list -1 -1)]
        [(equal? claims '(1 0)) (list 4 0)]
        [(equal? claims '(0 1)) (list 0 4)]
        [(equal? claims '(0 0)) (list 2 2)]))

(define (match-pair* au1 au2 results previous-claims countdown)
  (if (zero? countdown)
      results
      (match-pair* au1 au2
                   (append results (list (match-claims previous-claims)))
                   (list (next-claim au1 previous-claims)
                         (next-claim au2 (reverse previous-claims)))
                   (sub1 countdown))))

(define (match-pair automaton-pair rounds-per-match)
  (match-pair* (first automaton-pair)
               (second automaton-pair)
               '()
               (map automaton-init-claim automaton-pair)
               rounds-per-match))

(define (base10->base2 n)
  (~r n #:base 2 #:min-width 5 #:pad-string "0"))

(define (char->digit c)
  (case c
    [(#\0) 0]
    [(#\1) 1]))

(define (base2->digits a-string)
  (map char->digit (string->list a-string)))

(define (number->automaton n)
  (apply make-automaton (base2->digits (base10->base2 n))))

(define (take-sums round-results)
  (map (lambda (f) (sum (map f round-results)))
       (list first second)))
