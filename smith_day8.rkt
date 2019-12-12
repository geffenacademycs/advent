#lang racket
(require 2htdp/batch-io)

;aoc-day8-1 : string -> num
;takes the image input and returns the number of 1 digits * the number of 2 digits

(define (aoc-day8-1 file)
  (* (layer-count (process-input file) (most-0 (process-input file) 0 150 149) (+ 149 (most-0 (process-input file) 0 150 149)) 1) (layer-count (process-input file) (most-0 (process-input file) 0 150 149) (+ 149 (most-0 (process-input file) 0 150 149)) 2))
)

(define (process-input file)
  (num->list (string->number (read-file file)))
)

(define (num->list num)
  (charlist->numlist (string->list (number->string num)))
)

(define (charlist->numlist clst)
  (cond
    [(empty? clst) empty]
    [(cons? clst) (cons (- (char->integer (first clst)) 48) (charlist->numlist (rest clst)))]
  )
)

(define (num-list lon pos stop num)
  (cond
            [(= pos stop) (if (= num (list-ref lon pos)) (list (list-ref lon pos)) empty)]
            [(cons? lon) (append (if (= num (list-ref lon pos)) (list (list-ref lon pos)) empty) (num-list lon (+ pos 1) stop num))]
            )
)

(define (most-0 lon pos1 pos2 layer)
  (cond
    [(= (- (length lon) 1)(+ pos2 layer)) (if (< (layer-count lon pos1 (+ pos1 layer) 0) (layer-count lon pos2 (+ pos2 layer) 0)) pos1 pos2)]
    [(< (layer-count lon pos1 (+ pos1 layer) 0) (layer-count lon pos2 (+ pos2 layer) 0)) (most-0 lon pos1 (+ pos2 (+ 1 layer)) layer)]
    [(>= (layer-count lon pos1 (+ pos1 layer) 0) (layer-count lon pos2 (+ pos2 layer) 0)) (most-0 lon pos2 (+ pos2 (+ 1 layer)) layer)]
    [else (error pos2)]
  )
)

 
(define (layer-count lon pos stop num)
  (length (num-list lon pos stop num))
)


(define (aoc-day8-2 file)
  (stack-layers (process-input file) 0 149 (gen-list (list)))
)


(define (stack-layers lon pos layer output)
  (cond
    [(= 0 (layer-count output 0 149 3)) output]
    [(= 0 (list-ref lon pos)) (stack-layers lon (+ (modulo pos 150) 1) 149 (list-set output (modulo pos 150) 0))]
    [(= 1 (list-ref lon pos)) (stack-layers lon (+ (modulo pos 150) 1) 149 (list-set output (modulo pos 150) 1))]
    [(= 2 (list-ref lon pos)) (stack-layers lon (+ pos (+ 1 layer)) 149 output)]
   )
)

(define (gen-list lon)
  (if (= (length lon) 150) lon (gen-list (append lon (list 3))))
)

(aoc-day8-2 "input8-2.txt")