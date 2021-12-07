#lang racket
(require rackunit)

(define ex '(16 1 2 0 4 2 7 1 2 14))
(define puzzle (map string->number
                    (string-split
                     (string-trim
                      (file->string "input/day7.txt") "\n") ",")))

(define (solve in part)
  (define (minimize-fuel-cost in)
    (define (added-cost n)
      (cond [(eq? part 'part-one) (identity n)]
            [(eq? part 'part-two) (/ (* n (+ n 1)) 2)]))
    (define (fuel-cost pos)
      (foldl + 0 (map (lambda (x) (added-cost (abs (- pos x)))) in)))
     (define range-size (abs (- (argmin min in) (argmax max in))))
    (define pos-range (build-list range-size add1))
    (argmin min (map fuel-cost pos-range)))
  (minimize-fuel-cost in))

(check-eq? (solve ex 'part-one) 37)
(check-eq? (solve puzzle 'part-one) 344297)
(check-eq? (solve ex 'part-two) 168)
(check-eq? (solve puzzle 'part-two) 97164301)
