;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname depue_day6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "while.rkt" "installed-teachpacks") (lib "until.rkt" "installed-teachpacks") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "while.rkt" "installed-teachpacks") (lib "until.rkt" "installed-teachpacks") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)

(define (direct lst) (length lst)) ; since the input contains all branches, the length of the list of branches is the number of direct orbits

(define (interpret lst output) (cond ; this is to interpret codes in the format "GBH)EKR" to (list (list num num num) (list num num num))
    [(empty? lst) output] ; i converted strings to list of characters and individual characters to numbers
    [else (interpret (rest lst) (cons (organize (intcodes (first lst))) output))]))

(define (intcodes strng) ; changes strings into a list of characters and then characters to integers.
  (map char->integer ; for example, "GBH)EKR" -> (list num num num num num num num)
          (string->list strng)))

(define (organize lst) ; this is the organize function 
  (list ; while this may seem vestigial, this helps a lot when it comes to organize-reverse and backtracking for me
   (list (first lst) (second lst) (third lst)) ; a better and rewritten solution would be less shitty but whatever
   (list (fifth lst) (sixth lst) (seventh lst)))) ; this still works

(define (organize-reverse lst)
  (list ; reverse of organize, where the second object comes first
   (list (fifth lst) (sixth lst) (seventh lst))
   (list (first lst) (second lst) (third lst))))

(define (newpath code lst counter total)
  (path (associate lst code empty) lst counter total))

(define (path codelist lst counter total) (cond
   [(empty? codelist) total]
   [(> (length codelist) 1) (+ total (path (list (first codelist)) lst counter 0)
                               (path (rest codelist) lst counter 0))]
   [else (newpath (second (first codelist)) lst (+ counter 1) (+ (- counter 1) total))]))

(define (associate lst code output) (cond
  [(false? (assoc code lst)) output]
  [else (associate (remove (assoc code lst) lst)
                   code
                   (cons (assoc code lst) output))]))

(define (interpret-reverse lst output) (cond
    [(empty? lst) output]
    [else (interpret-reverse (rest lst) (cons (organize-reverse (intcodes (first lst))) output))]))



(define (tracepath code lst counter pathlist)
  (pathtrace (associate lst code empty) lst counter pathlist))

(define (pathtrace codelist lst counter pathlist) (cond
   [(empty? codelist) (cons counter pathlist)]
   [else (tracepath (second (first codelist)) lst (+ counter 1) (cons codelist pathlist))]
  ))

(define (tree-distancex path1 path2 lst)
  (- (+ (first path1) (first path2)) 2 (* 2 (lowestcommonpath (length path1) (rest path1) (rest path2) empty lst))))

(define (lowestcommonpath lst1b lst1 lst2 currentlowest lst) (cond
      [(empty? lst1) lst1b]
      [(matches? (first (first (first lst1))) 
                 (first (first (first lst2)))) 
                        (first (tracepath currentlowest lst 0 empty))]
      [else (lowestcommonpath lst1b (rest lst1) (rest lst2) 
                              (if (empty? (first (first (first lst1)))) 
                                          currentlowest 
                                  (first (first (first lst1)))) 
                              lst)]))

(define (matches? lst1 lst2) (if
    (and (eq? (first lst1) (first lst2)) 
         (eq? (second lst1) (second lst2)) 
         (eq? (third lst1) (third lst2))) 
         false 
         true
  ))

(define (tree-distance name1 name2 file) ; starter function for tree-distancex
(tree-distancex (tracepath (intcodes name1)
                     (interpret-reverse (read-lines file) empty) 0 empty) (tracepath (intcodes name2)
                     (interpret-reverse (read-lines file) empty) 0 empty) (interpret-reverse (read-lines file) empty)))
