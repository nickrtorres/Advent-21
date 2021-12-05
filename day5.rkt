#lang racket
(require rackunit)

(define ex #<<ex
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
ex
  )
(define puzzle (file->string "input/day5.txt"))

(define (cycle v n) (make-list n v))

(define (smart-range b e)
  (define step (if (> b e) -1 1))
  (inclusive-range b e step))

(define (make-line-segment in)
  (map string->number (regexp-match* #rx"[0-9]+" in)))

(check-equal? '(0 9 5 9) (make-line-segment "0,9 -> 5,9"))

(define (line-segment-x1 line-segment) (first line-segment))
(define (line-segment-y1 line-segment) (second line-segment))
(define (line-segment-x2 line-segment) (third line-segment))
(define (line-segment-y2 line-segment) (fourth line-segment))

(define (horizontal-line-segment? line-segment)
  (= (line-segment-x1 line-segment) (line-segment-x2 line-segment)))

(define (vertical-line-segment? line-segment)
  (= (line-segment-y1 line-segment) (line-segment-y2 line-segment)))

(define (diagonal-line-segment? line-segment)
  (define x1 (line-segment-x1 line-segment))
  (define x2 (line-segment-x2 line-segment))
  (define y1 (line-segment-y1 line-segment))
  (define y2 (line-segment-y2 line-segment))
  (define rise (abs (- x1 x2)))
  (define run (abs (- x1 x2)))
  (= 1 (/ rise run)))

(check-true (diagonal-line-segment? (make-line-segment "1,1 -> 3,3")))

(define (line-segment-span line-segment)
  (define x1 (line-segment-x1 line-segment))
  (define x2 (line-segment-x2 line-segment))
  (define y1 (line-segment-y1 line-segment))
  (define y2 (line-segment-y2 line-segment))
  (define (vertical-span line-segment)
    (define d (add1 (abs (- x2 x1))))
    (map cons (smart-range x1 x2) (cycle y1 d)))
  (define (horizontal-span line-segment)
    (define d (add1 (abs (- y2 y1))))
    (map cons (cycle x1 d) (smart-range y1 y2)))
  (define (diagonal-span line-segment)
    (map cons (smart-range x1 x2) (smart-range y1 y2)))
  (cond [(vertical-line-segment? line-segment) (vertical-span line-segment)]
        [(horizontal-line-segment? line-segment) (horizontal-span line-segment)]
        [(diagonal-line-segment? line-segment) (diagonal-span line-segment)]
        [else (error "TODO!")]))

(check-equal? '((2 . 2) (2 . 1)) (line-segment-span (make-line-segment "2,2 -> 2,1")))
(check-equal? '((2 . 2) (2 . 1)) (line-segment-span (make-line-segment "2,2 -> 2,1")))
(check-equal? '((0 . 9) (1 . 9) (2 . 9)) (line-segment-span (make-line-segment "0,9 -> 2,9")))
(check-equal? '((9 . 7) (8 . 8) (7 . 9)) (line-segment-span (make-line-segment "9,7 -> 7,9")))

(define (solve in part)
  (define (make-plot segments part)
    (define (candidate? line)
      (define (part-one-candidate? line)
        (or (vertical-line-segment? line)
            (horizontal-line-segment? line)))
      (define (part-two-candidate? line)
        (or (vertical-line-segment? line)
            (horizontal-line-segment? line)
            (diagonal-line-segment? line)))
      (cond [(equal? part 'part-one) (part-one-candidate? line)]
            [(equal? part 'part-two) (part-two-candidate? line)]
            [else (error "invalid part")]))
    (define candidates (filter candidate? segments))
    (foldl (lambda (segment table)
             (foldl (lambda (point acc)
                      (hash-update acc point add1 0)) table (line-segment-span segment)))
           (make-immutable-hash)
           candidates))
  (define (count-overlap table)
    (count (lambda (x) (>= (cdr x) 2)) (hash->list table)))
  (define lines (string-split in "\n"))
  (count-overlap (make-plot (map make-line-segment lines) part)))

(check-eq? 5 (solve ex 'part-one))
(check-eq? 12 (solve ex 'part-two))
(check-eq? 5197 (solve puzzle 'part-one))
(check-eq? 18605 (solve puzzle 'part-two))
