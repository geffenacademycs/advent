#lang racket
(require 2htdp/batch-io)
(require racket/trace)


;(regexp-match* #px"-?\\d{1,2}" (first (read-lines "input12-1.txt")))


(define (process-input file)
  (stringlist->numlist (append
   (regexp-match* #px"-?\\d{1,2}" (first (read-lines file)))
   (regexp-match* #px"-?\\d{1,2}" (second (read-lines file)))
   (regexp-match* #px"-?\\d{1,2}" (third (read-lines file)))
   (regexp-match* #px"-?\\d{1,2}" (fourth (read-lines file)))
   ))
)

(define (stringlist->numlist slst)
  (cond
    [(empty? slst) empty]
    [(cons? slst) (cons (string->number (first slst)) (stringlist->numlist (rest slst)))]
  )
)


(define (aoc-day12-1 file)
  (calc-energy (tick-pos (process-input file) (gen-list (list)) 1000) (tick-vel (process-input file) (gen-list (list)) 1000) 0)
)

(define (gravity lon vel pos pos1 pos2 pos3)
  (+
   (list-ref vel pos)
   (cond
     [(> (list-ref lon pos) (list-ref lon pos1)) -1]
     [(< (list-ref lon pos) (list-ref lon pos1)) 1]
     [else 0]
    )
      (cond
     [(> (list-ref lon pos) (list-ref lon pos2)) -1]
     [(< (list-ref lon pos) (list-ref lon pos2)) 1]
     [else 0]
    )
        (cond
     [(> (list-ref lon pos) (list-ref lon pos3)) -1]
     [(< (list-ref lon pos) (list-ref lon pos3)) 1]
     [else 0]
    )
  ) 
)

(define (all-gravity pos vel)
  (list
   (gravity pos vel 0 3 6 9) (gravity pos vel 1 4 7 10) (gravity pos vel 2 5 8 11)
   (gravity pos vel 3 6 9 0) (gravity pos vel 4 7 10 1) (gravity pos vel 5 8 11 2)
   (gravity pos vel 6 9 0 3) (gravity pos vel 7 1 10 4) (gravity pos vel 8 2 5 11)
   (gravity pos vel 9 3 6 0) (gravity pos vel 10 4 7 1) (gravity pos vel 11 5 8 2)
  )
)


(define (calc-pos pos vel)
  (list
   (+ (list-ref pos 0) (list-ref vel 0)) (+ (list-ref pos 1) (list-ref vel 1)) (+ (list-ref pos 2) (list-ref vel 2))
   (+ (list-ref pos 3) (list-ref vel 3)) (+ (list-ref pos 4) (list-ref vel 4)) (+ (list-ref pos 5) (list-ref vel 5))
   (+ (list-ref pos 6) (list-ref vel 6)) (+ (list-ref pos 7) (list-ref vel 7)) (+ (list-ref pos 8) (list-ref vel 8))
   (+ (list-ref pos 9) (list-ref vel 9)) (+ (list-ref pos 10) (list-ref vel 10)) (+ (list-ref pos 11) (list-ref vel 11))
   )
)


(define (tick-pos pos vel count)
  (if (<= count 0) pos (tick-pos (calc-pos pos (all-gravity pos vel)) (all-gravity pos vel) (- count 1)))
)

(define (tick-vel pos vel count)
  (if (<= count 0) vel (tick-vel (calc-pos pos (all-gravity pos vel)) (all-gravity pos vel) (- count 1)))
)


(define (calc-energy pos vel count)
  (+
   (* (+ (abs (list-ref pos 0)) (abs (list-ref pos 1)) (abs (list-ref pos 2))) (+ (abs (list-ref vel 0)) (abs (list-ref vel 1)) (abs (list-ref vel 2))))
   (* (+ (abs (list-ref pos 3)) (abs (list-ref pos 4)) (abs (list-ref pos 5))) (+ (abs (list-ref vel 3)) (abs (list-ref vel 4)) (abs (list-ref vel 5))))
   (* (+ (abs (list-ref pos 6)) (abs (list-ref pos 7)) (abs (list-ref pos 8))) (+ (abs (list-ref vel 6)) (abs (list-ref vel 7)) (abs (list-ref vel 8))))
   (* (+ (abs (list-ref pos 9)) (abs (list-ref pos 10)) (abs (list-ref pos 11))) (+ (abs (list-ref vel 9)) (abs (list-ref vel 10)) (abs (list-ref vel 11))))
  )
 )

(define (gen-list lon)
  (if (= (length lon) 12) lon (gen-list (append lon (list 0))))
)

(define (pot-energy pos count)
  (+ (abs (list-ref pos count)) (if (>= count (- (length pos) 1)) 0 (pot-energy pos (+ count 1))))
)

(define (ken-energy vel count)
  (+ (abs (list-ref vel count)) (if (>= count (- (length vel) 1)) 0 (ken-energy vel (+ count 1))))
)


(define (aoc-day12-2 file)
  (write-file "12.txt" (number->string (check (process-input file) (gen-list (list)) (list) 0)))
)


(define (store-data pos vel lst)
  (append lst (list (append pos vel)))
)

(define (check pos vel lst count)
  (cond
    [(find-in-list lst (append pos vel)) count]
    ;[(> count 3000) lst]
    [else (check (calc-pos pos (all-gravity pos vel)) (all-gravity pos vel) (store-data pos vel lst) (+ 1 count))]
  )
)
 
(define (find-in-list lst val)
  (cond
    [(empty? lst) #f]
    [(equal? (first lst) val) #t]
    [else (find-in-list (rest lst) val)]
  )
)


(aoc-day12-2 "input12-1.txt")