;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname depue_day6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "while.rkt" "installed-teachpacks") (lib "until.rkt" "installed-teachpacks") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "while.rkt" "installed-teachpacks") (lib "until.rkt" "installed-teachpacks") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)

; lst -> num
(define (direct lst) (length lst)) ; since the input contains all branches, the length of the list of branches is the number of direct orbits

; lst empty -> lst
(define (interpret lst output) (cond ; this is to interpret codes in the format "GBH)EKR" to (list (list num num num) (list num num num))
    [(empty? lst) output] ; i converted strings to list of characters and individual characters to numbers
    [else (interpret (rest lst) (cons (organize (intcodes (first lst))) output))]))

; string -> lst of num
(define (intcodes strng) ; changes strings into a list of characters and then characters to integers.
  (map char->integer ; for example, "GBH)EKR" -> (list num num num num num num num)
          (string->list strng)))

; lst -> lst
(define (organize lst) ; this is the organize function 
  (list ; while this may seem vestigial, this helps a lot when it comes to organize-reverse and backtracking for me
   (list (first lst) (second lst) (third lst)) ; a better and rewritten solution would be less shitty but whatever
   (list (fifth lst) (sixth lst) (seventh lst)))) ; this still works

; lst -> lst
(define (organize-reverse lst) ; this makes it easier to take the second item as the path, to trace back to COM
  (list ; reverse of organize, where the second object comes first
   (list (fifth lst) (sixth lst) (seventh lst))
   (list (first lst) (second lst) (third lst))))

; lst lst 0 0 -> num
(define (newpath code lst counter total) ; starter function for newpath
  (path (associate lst code empty) lst counter total)) ; when passed a code and a lst it finds all items with that begin with that code for use

; lst lst 0 0 -> num
(define (path codelist lst counter total) (cond
   [(empty? codelist) total] ; output total for this branch of codelist
   [(> (length codelist) 1) (+ total (path (list (first codelist)) lst counter 0) ; i probably should make this prettier but i have to many advents to work on rn
                               (path (rest codelist) lst counter 0))] ; this just splits the function if its a pair or more
   [else (newpath (second (first codelist)) lst (+ counter 1) (+ (- counter 1) total))])) ; if its a singular item and runs again for the next value ahead of previous code

; lst lst empty -> lst
(define (associate lst code output) (cond ; this is a function which uses associate and takes all codes that begin with the inputted code
  [(false? (assoc code lst)) output] ; if that code no longer exists then output
  [else (associate (remove (assoc code lst) lst) ; else just take the next matching input and add it to the output
                   code ; i probably could have found a more efficient way but whatever
                   (cons (assoc code lst) output))]))

; lst empty -> lst
(define (interpret-reverse lst output) (cond ; this is used in part 2 and is the same as interpret but it uses organize-reverse
    [(empty? lst) output]
    [else (interpret-reverse (rest lst) (cons (organize-reverse (intcodes (first lst))) output))]))

; this is for part 2 now

; lst lst num lst -> (list num lst)
(define (tracepath code lst counter pathlist) ; starter function for path trace kinda
  (pathtrace (associate lst code empty) lst counter pathlist)) ; when passed a code and a lst it finds all items with that begin with that code for use

; lst lst num lst -> (list num lst)
(define (pathtrace codelist lst counter pathlist) (cond
   [(empty? codelist) (cons counter pathlist)]
   [else (tracepath (second (first codelist)) lst (+ counter 1) (cons codelist pathlist))]
  ))
  
 ; the definition of a distance between two items in a tree is 
(define (tree-distancex path1 path2 lst) ; the path1->start + path2->start - 2 * lowest common ancestor
  (- (+ (first path1) (first path2)) 2 (* 2 (lowestcommonpath (length path1) (rest path1) (rest path2) empty lst))))
  ; subtracted two here bc my path functions included the items themselves in distance calculations

; num lst lst lst lst -> num
(define (lowestcommonpath lst1b lst1 lst2 currentlowest lst) (cond
      [(empty? lst1) lst1b] ; if lst1 is on the path to lst2, then the length of lst1 is the result
      [(matches? (first (first (first lst1)))  ; check if the addresses don't match
                 (first (first (first lst2)))) 
                        (first (tracepath currentlowest lst 0 empty))] ; if they don't match then output the distance between this lowest common ancestor and 0
      [else (lowestcommonpath lst1b (rest lst1) (rest lst2) ; if those addresses match then run again with the (rest) of lists
                              (if (empty? (first (first (first lst1)))) ; store the address of this current ancestor
                                          currentlowest  ; unless its empty then just store the previous
                                  (first (first (first lst1)))) ; if not then store it
                              lst)])) ; maintain

; lst lst -> bool
(define (matches? lst1 lst2) 
     (not (and (eq? (first lst1) (first lst2)) ; if the first item in the first list mathes the first item in the second list
         (eq? (second lst1) (second lst2)) ; and so on
         (eq? (third lst1) (third lst2))) ; and so on
         ; return false if they are matching 
       ; if they dont match then return true
  )) ; this is kinda reversed but intentionally to reduce the size of the lowest common path function

(define (aoc-6-2 name1 name2 file) ; starter function for tree-distancex
(tree-distancex (tracepath (intcodes name1) ; pretty self explanatory
                     (interpret-reverse (read-lines file) empty) 0 empty) (tracepath (intcodes name2)
                     (interpret-reverse (read-lines file) empty) 0 empty) (interpret-reverse (read-lines file) empty)))
