(require racket/gui/base) ; to have TV
(require plot/no-gui) ; to have plot/dc
(require racket/draw) ; to draw
(require racket/math) ; to draw arc
(require math) ; to have mean
(plot-new-window? #t)

(define (vector-first a-vector)
  (vector-ref a-vector 0))
(define (vector-second a-vector)
  (vector-ref a-vector 1))
(define (vector-third a-vector)
  (vector-ref a-vector 2))
(define (vector-fourth a-vector)
  (vector-ref a-vector 3))
(define (vector-last a-vector)
  (vector-ref a-vector (- (vector-length a-vector) 1)))
(define (vector-rest a-vector)
  (vector-drop a-vector 1))

;; 1 = playing hawk
;; 0 = playing dove
;; the first number is the initial strategy
;; the second number is the previous strategy
;; the list is the switching strategy rule
;; outcome HH HD DH DD

(define (h) (vector 1 1 (list 1 1 1 1))) ; always hawk
(define (d) (vector 0 0 (list 0 0 0 0))) ; always dove
(define (tft) (vector 0 0 (list 1 0 1 0))) ; tit for tat
(define (ctft) (vector 1 1 (list 1 0 1 0))) ; cautious tit for tat
(define (wsls) (vector 1 1 (list 0 1 0 1))) ; the pavlov guy =))

(define 5-type-list (list (h) (d) (tft) (ctft) (wsls)))

;; support function for automata
(define (morph an-auto)
  (let ([listed (vector->list an-auto)])
    (append (take listed 2)
            (drop listed 2))))
(define (clone an-auto)
  (let* ([morphed (morph an-auto)]
	 [vectored (list->vector morphed)])
    (vector-append (vector-take vectored 2)
		   (vector-drop vectored 2))))


(define (h? s)
  (equal? s 1))

(define (d? s)
  (equal? s 0))

(define (match-strat s1 s2)
  (if (h? s1)
      (if (h? s2)
          (list -1 -1)
          (list 4 0))
      (if (h? s2)
          (list 0 4)
          (list 2 2))))

(define (init-strat a)
  (vector-first a))

(define (pre-strat a)
  (vector-second a))

(define (move-rules a)
  (vector-third a))

(define (set-cond-moves! a1 a2)
  (let ([pre-1 (pre-strat a1)]
        [pre-2 (pre-strat a2)]
        [rule-1 (move-rules a1)]
        [rule-2 (move-rules a2)])
    (if (h? pre-1)
        (if (h? pre-2)
            (begin
              (vector-set! a1 1 (first rule-1))
              (vector-set! a2 1 (first rule-2)))
            (begin
              (vector-set! a1 1 (second rule-1))
              (vector-set! a2 1 (third rule-2))))
        (if (h? pre-2)
            (begin
              (vector-set! a1 1 (third rule-1))
              (vector-set! a2 1 (second rule-2)))
            (begin
              (vector-set! a1 1 (fourth rule-1))
              (vector-set! a2 1 (fourth rule-2)))))))

(define (match-auto a1 a2 r)
  (let ([init-1 (init-strat a1)]
        [init-2 (init-strat a2)]
        [rule-1 (move-rules a1)]
        [rule-2 (move-rules a2)])
    (begin
      (vector-set! a1 1 init-1)
      (vector-set! a2 1 init-2)
      (append
       (list (match-strat (vector-second a1) (vector-second a2)))
       (for/list ([n (sub1 r)])
         (begin
           (set-cond-moves! a1 a2)
           (match-strat (vector-second a1)
                        (vector-second a2))))))))

(define (reset-auto! a)
  (vector-set! a 1 (init-strat a)))

;; create world

;; support function

(define (shuffle a-vector)
  (do
      ([n (vector-length a-vector) (- n 1)])
      ((zero? n) a-vector)
    (let* ([r (random n)]
	   [t (vector-ref a-vector r)])
      (vector-set! a-vector r (vector-ref a-vector (- n 1)))
      (vector-set! a-vector (- n 1) t))))

;; create a world of p slots
(define (create-world p)
  (for/vector ([i p]) 0))
(define N 100)
(define A (create-world N)) ; A is population
(define B (create-world N)) ; B is payoff book
(define B+ (create-world N)) ; B+ is the positive payoff book
(define T (create-world 5)) ; C is the type counting
(define F (create-world 5)) ; D is the type fitness
(define S (vector 0)) ; S is the payoff total
(define h-series (list (vector 0 0)))
(define d-series (list (vector 0 0)))
(define t-series (list (vector 0 0)))
(define c-series (list (vector 0 0)))
(define w-series (list (vector 0 0)))

;; create population to fill in the world (all positive numbers)
(define (create-population world hawk dove t4t ct4t pavlov)
                                        ;; (800 100 50 1 49)
  (let ([auto-population
         (shuffle
          (list->vector
           (append
            (for/list
                ([i hawk])
              (clone (h)))
            (for/list
                ([j dove])
              (clone (d)))
            (for/list
                ([k t4t])
              (clone (tft)))
            (for/list
                ([m ct4t])
              (clone (ctft)))
            (for/list
                ([n pavlov])
              (clone (wsls))))))])
    (begin
      (set! h-series (list (vector 0 hawk)))
      (set! d-series (list (vector 0 dove)))
      (set! t-series (list (vector 0 t4t)))
      (set! c-series (list (vector 0 ct4t)))
      (set! w-series (list (vector 0 pavlov)))
      (for ([n (vector-length world)])
        (vector-set! world n (vector-ref auto-population n))))))


(define (mean-pay posn pay-list)
  (mean (map posn pay-list)))

(define (accum a-list)
  (for/list ([n (length a-list)])
    (sum (take a-list (+ n 1)))))

;; payoff book
(define (set-payoff! population i1 i2 r pay-book)
  (let ([payoff (match-auto (vector-ref population i1)
                             (vector-ref population i2)
                             r)])
    (vector-set! pay-book i1  (mean-pay first payoff))
    (vector-set! pay-book i2  (mean-pay second payoff))))

(define (match-population! population r pay-book)
  (begin
    (for ([n (/ (vector-length population) 2)])
      (set-payoff! population (* 2 n) (add1 (* 2 n)) r pay-book))
    (vector-map reset-auto! population)))



(define (add2! pay-book posi-book)
  (for ([i (vector-length pay-book)])
    (vector-set! posi-book i
                 (+ 2
                  (vector-ref
                   pay-book i)))))

(define (v-sum! posi-book sum-book)
  (vector-set! sum-book 0
	       (sum (vector->list posi-book))))

(define (reset-book! book)
  (for ([n (vector-length book)])
    (vector-set! book n 0)))

(define (identify-auto auto)
  (cond [(equal? auto #(1 1 (1 1 1 1))) 0] ; all-hawk
        [(equal? auto #(0 0 (0 0 0 0))) 1] ; all-dove
        [(equal? auto #(0 0 (1 0 1 0))) 2] ; tit for tat
        [(equal? auto #(1 1 (1 0 1 0))) 3] ; cautious tit for tat
        [else 4])) ; the pavlov guy haha


(define (hawk? auto)
  (equal? auto #(1 1 (1 1 1 1))))
(define (dove? auto)
  (equal? auto #(0 0 (0 0 0 0))))
(define (tft? auto)
  (equal? auto #(0 0 (1 0 1 0))))
(define (ctft? auto)
  (equal? auto #(1 1 (1 0 1 0))))
(define (wsls? auto)
  (equal? auto #(1 1 (0 1 0 1))))

(define (count-types population type-book)
  (begin
    (vector-set! type-book 0 (vector-count hawk? population))
    (vector-set! type-book 1 (vector-count dove? population))
    (vector-set! type-book 2 (vector-count tft? population))
    (vector-set! type-book 3 (vector-count ctft? population))
    (vector-set! type-book 4 (vector-count wsls? population))))

(define (extract-payoff type? population posi-book)
  (for/list ([i (vector-length population)])
    (and
     (type? (vector-ref population i))
     (vector-ref posi-book i))))

(define (true? x)
  (not (false? x)))

(define (extract-fit type? population posi-book sum-book)
    (/ (sum (filter true? (extract-payoff type? population posi-book)))
       (vector-first sum-book)))

(define (calculate-type-fitness population posi-book fitness-book sum-book)
  (begin
    (vector-set! fitness-book 0 (extract-fit hawk? population posi-book sum-book))
    (vector-set! fitness-book 1 (extract-fit dove? population posi-book sum-book))
    (vector-set! fitness-book 2 (extract-fit tft? population posi-book sum-book))
    (vector-set! fitness-book 3 (extract-fit ctft? population posi-book sum-book))
    (vector-set! fitness-book 4 (extract-fit wsls? population posi-book sum-book))))

(define (extract-average type? population posi-book)
  (if (zero? (vector-count type? population))
      0
      (/ (extract-fit type? population posi-book)
         (vector-count type? population))))

(define (type-average population posi-book)
  (vector (extract-average hawk? population posi-book)
          (extract-average dove? population posi-book)
          (extract-average tft? population posi-book)
          (extract-average ctft? population posi-book)
          (extract-average wsls? population posi-book)))

(define (do-cal! population pay-book posi-book sum-book type-book fitness-book)
  (begin
    (add2! pay-book posi-book)
    (v-sum! posi-book sum-book)
    (count-types population type-book)
    (calculate-type-fitness population posi-book fitness-book sum-book)))

(define (abridged-report type-book fitness-book)
  (list
   (vector->list type-book)
   (accum (vector->list fitness-book))))

(define (regenerate population speed type-book fitness-book)
  (let ([accum-fitness (second (abridged-report type-book fitness-book))])
    (for ([i speed])
     (vector-set! population i
                  (let ([r (random)])
                    (cond [(< r (first accum-fitness)) (clone (h))]
                          [(and (>= r (first accum-fitness))
                                (< r (second accum-fitness))) (clone (d))]
                          [(and (>= r (second accum-fitness))
                                (< r (third accum-fitness))) (clone (tft))]
                          [(and (>= r (third accum-fitness))
                                (< r (fourth accum-fitness))) (clone (ctft))]
                          [else (clone (wsls))]))))))

(define (fitness-test speed type-book fitness-book)
  (let ([accum-fitness (second (abridged-report type-book fitness-book))])
    (for/list ([i speed])
      (let ([r (random)])
        (cond [(< r (first accum-fitness)) (clone (h))]
              [(and (>= r (first accum-fitness))
                    (< r (second accum-fitness))) (clone (d))]
              [(and (>= r (second accum-fitness))
                    (< r (third accum-fitness))) (clone (tft))]
              [(and (>= r (third accum-fitness))
                    (< r (fourth accum-fitness))) (clone (ctft))]
              [else (clone (wsls))])))))

(define type-test (vector 0 0 0 0 0))

(define (export-data path txt)
  (call-with-output-file path
    (lambda (output-port)
      (write txt output-port))
    #:exists 'append))


(define (shuffle! population)
  (let ([new-popu (shuffle population)])
    (for ([i (vector-length population)])
      (vector-set! population i (vector-ref new-popu i)))))


;; TV
(define demographic-frame (new frame% [label "population demographic over time"]
                           [width 600]
                           [height 400]))
(define demo-canvas (new canvas% [parent demographic-frame]))
(define dc-demo (send demo-canvas get-dc))

(define (add-pair! ith-cycle type-book)
  (begin
    (set! h-series (append h-series (list
                               (vector ith-cycle
                                       (vector-first type-book)))))
    (set! d-series (append d-series (list
                                   (vector ith-cycle
                                           (vector-second type-book)))))
    (set! t-series (append t-series (list
                                   (vector ith-cycle
                                           (vector-third type-book)))))
    (set! c-series (append c-series (list
                                   (vector ith-cycle
                                           (vector-fourth type-book)))))
    (set! w-series (append w-series (list
                                   (vector ith-cycle
                                           (vector-last type-book)))))
    ))

(define (plot-demographic ith-cycle type-book)
  (begin
    (add-pair! ith-cycle type-book)
    (plot/dc (list
              (lines h-series
                     #:x-min 0 #:x-max 1000
                     #:y-min 0 #:y-max N #:color 1 #:label "h")
              (lines d-series
                     #:color 2 #:label "d")
              (lines t-series
                     #:color 3 #:label "t4t")
              (lines c-series
                     #:color 4 #:label "ct4t")
              (lines w-series
                     #:color 6 #:label "pavlov :D")
              )
             dc-demo
             0 0
             600 400)))


;;create population A at ratio...
(define (evolve-population cycles speed pause)
  (for/and ([n cycles])
    (match-population! A 100 B)
    (sleep pause)
    (do-cal! A B B+ S T F)
    (plot-demographic n T)
    (sleep pause)
    (regenerate A speed T F)
    (shuffle! A)
                                        ; (export-data "report.rkt"
                                        ;              (vector-take (count-types A T) 2))
    ))

(send demographic-frame show #t)
