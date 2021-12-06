#lang racket
(require rackunit)

(define ex '(3 4 3 1 2))
(define puzzle (map string->number
                    (string-split (string-trim (file->string "input/day6.txt") "\n") ",")))

;; Brute force solution didn't cut it for part 2; got the hash table idea from
;; https://youtu.be/0seLl53EsEI
(define (solve in days)
  (define (make-fish-table in)
    (foldl (lambda (fish table) (hash-update table fish add1 0))
           (make-immutable-hash) in))
  (define (simulate-days table days)
    (define (simulate-day table)
      (define reset-fish 100)
      (define new-fish 200)
      (define (simulate fish-bucket table)
        (define (reset-table fish-bucket table)
          (hash-set
           (hash-update
            (hash-update table new-fish (lambda (x) (+ x (cdr fish-bucket))) 0)
            reset-fish (lambda (x) (+ x (cdr fish-bucket))) 0) 0 0))
        (define (update-table fish-bucket table)
          (hash-set
           (hash-update table (sub1 (car fish-bucket)) (lambda (x) (+ x (cdr fish-bucket))) 0)
           (car fish-bucket) 0))
        (cond [(= (car fish-bucket) 0) (reset-table fish-bucket table)]
              [else (update-table fish-bucket table)]))
      (define updated (foldl simulate (make-immutable-hash) (hash->list table)))
      (hash-remove
       (hash-remove
        (hash-update
         (hash-update updated 8 (lambda (x) (+ x (hash-ref updated new-fish 0))) 0)
         6 (lambda (x) (+ x (hash-ref updated reset-fish 0))) 0) reset-fish) new-fish))
    (define (loop table i end)
      (cond [(= i end) table]
            [else (loop (simulate-day table) (add1 i) end)]))
    (loop table 0 days))
  (define (count-fish table)
    (foldl + 0 (hash-values table)))
  (count-fish (simulate-days (make-fish-table in) days)))

(check-eq? (solve ex 18) 26)
(check-eq? (solve ex 80) 5934)
(check-eq? (solve puzzle 80) 365131)
(check-eq? (solve puzzle 256) 1650309278600)
