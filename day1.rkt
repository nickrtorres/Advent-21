#lang racket
(require rackunit)

(define ex '(199 200 208 210 200 207 240 269 260 263))
(define puzzle (map string->number (file->lines "input/day1.txt"))) 

;; Part 1

(define (solve in)
  (define (solve-tail in prev acc)
    (if (empty? in)
        acc
        (solve-tail (rest in)
                    (first in)
                    (if (> (first in) prev) (add1 acc) acc))))
  (solve-tail (rest in) (first in) 0))

(check-eq? 7 (solve ex))
(check-eq? 1711 (solve puzzle))

;; Part 2

(define (solve-two in)
  (define (make-sum new-num old-sum)
    (list new-num (first old-sum) (second old-sum)))
  (define (sum-gt? lhs rhs)
    (define (sum-value sum) (apply + sum))
    (> (sum-value lhs) (sum-value rhs)))
  (define (solve-tail in last-sum acc)
    (if (empty? in)
        acc
        (let ([new-sum (make-sum (first in) last-sum)])
          (solve-tail (rest in)
                      new-sum
                      (if (sum-gt? new-sum last-sum) (add1 acc) acc)))))
  (let ([first-sum (list (third in) (second in) (first in))]
        [rem (rest (rest (rest in)))])
    (solve-tail rem first-sum 0)))

(check-eq? 5 (solve-two ex))
(check-eq? 1743 (solve-two puzzle))
