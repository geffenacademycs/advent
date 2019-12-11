(require 2htdp/batch-io)

(define (backwards-list num)
  (local [(define n 1)]
    (cond
      [(equal? (quotient num (expt 10 n)) 0) (cons num empty)]
      (else (cons (remainder num (expt 10 n))
                  (backwards-list (quotient num (expt 10 n))))))))

; num->list : num => list
; takes a num and returs a list of digits

(define (num->list num)
  (reverse (backwards-list num)))

; checks if the code is increasing
(define (increasing? lon)
  (cond
    [(empty? lon) empty]
    [(empty? (rest lon)) #true]
    [(<= (first lon) (second lon)) (increasing? (rest lon))]
    [(> (first lon) (second lon)) #false]))

; checks if there is at least 1 double num
(define (double-num? lon)
  (cond
    [(empty? lon) empty]
    [(empty? (rest lon)) #false]
    [(equal? (first lon) (second lon)) #true]
    [(> (first lon) (second lon)) (double-num? (rest lon))]
    [(< (first lon) (second lon)) (double-num? (rest lon))]))

; checks if the code is six digits
(define (six-digit? lon)
  (equal? (length lon) 6))

(define (pass-finder num)
  (and (double-num? (num->list num)) (increasing? (num->list num)) (six-digit? (num->list num))))

; num1 should be less than num2
; calcs the num of usable passwords
(define (pass-calc num1 num2)
  (cond
    [(equal? num1 num2) (if (pass-finder num1) 1 0)]
    (else (if (pass-finder num1)
              (+ 1 (pass-calc (+ 1 num1) num2))
              (pass-calc (+ 1 num1) num2)))))

; aoc-day4-1 : num num => num
; takes the two input numbers and returns the solution to day 4, part 1

(define (aoc-day4-1 num1 num2)
  (pass-calc num1 num2))

; checks if there are only 2 digits repeating in the 6 digit code
(define (only-double-num? lon)
  (cond
    [(empty? lon) empty]
    [(empty? (rest lon)) #false]
    [(equal? (first lon) (second lon)) (cond
                                         [(empty? (rest (rest lon))) #true]
                                         [(equal? (first lon) (third lon))(cond
                                                                             [(empty? (rest (rest (rest lon)))) #false]
                                                                             [(equal? (first lon) (fourth lon))(cond
                                                                                                                 [(empty? (rest (rest (rest (rest lon))))) #false]
                                                                                                                 [(equal? (first lon) (fifth lon)) #false]
                                                                                                                 (else (only-double-num? (rest (rest (rest (rest lon)))))))]
                                                                             (else (only-double-num? (rest (rest (rest lon))))))]
                                         (else #true))]
    (else (only-double-num? (rest lon)))))


(define (pass-finder2 num)
  (and (only-double-num? (num->list num)) (increasing? (num->list num)) (six-digit? (num->list num))))

; calcs the number of usable passwords
(define (pass-calc2 num1 num2)
  (cond
    [(equal? num1 num2) (if (pass-finder2 num1) 1 0)]
    (else (if (pass-finder2 num1)
              (+ 1 (pass-calc2 (+ 1 num1) num2))
              (pass-calc2 (+ 1 num1) num2)))))

; aoc-day4-2 : num num => num
; takes the two input numbers and returns the solution to day 4, part 2

(define (aoc-day4-2 num1 num2)
  (pass-calc2 num1 num2))
