#lang racket
(require rackunit)

(define (make-pos tag x y) (list tag x y 0))

(define (pos-tag pos) (first pos))

(define (pos-x pos) (second pos))

(define (pos-y pos) (third pos))

(define (pos-aim pos) (fourth pos))

(define (pos-update-x pos x)
  (list (pos-tag pos) (+ (pos-x pos) x) (pos-y pos) (pos-aim pos))) 

(define (pos-update-y pos y)
  (list (pos-tag pos) (pos-x pos) (+ (pos-y pos) y) (pos-aim pos)))

(define (pos-update-aim pos aim)
  (list (pos-tag pos) (pos-x pos) (pos-y pos) (+ aim (pos-aim pos))))

(define (pos-mul pos) (* (pos-x pos) (pos-y pos)))

(define (down value)
  (lambda (pos)
    (cond [(eq? (pos-tag pos) 'part-one) (pos-update-y pos value)]
          [(eq? (pos-tag pos) 'part-two) (pos-update-aim pos value)]
          [else (error "bad tag")])))

(define (up value)
  (lambda (pos)
    (cond [(eq? (pos-tag pos) 'part-one) (pos-update-y pos (- 0 value))]
          [(eq? (pos-tag pos) 'part-two) (pos-update-aim pos (- 0 value))]
          [else (error "bad tag")])))

(define (forward value)
  (define (part-two-forward pos value)
    (let ([new-pos (pos-update-x pos value)])
      (pos-update-y new-pos (* (pos-aim new-pos) value))))
  (lambda (pos)
    (cond [(eq? (pos-tag pos) 'part-one) (pos-update-x pos value)]
          [(eq? (pos-tag pos) 'part-two) (part-two-forward pos value)]
          [else (error "bad tag")])))

(check-equal? (make-pos 'part-one 5 0) ((forward 5) (make-pos 'part-one 0 0)))

(define (solve tag in)
  (define (dispatch in)
    (define cmd (string-split in))
    (cond [(string=? (first cmd) "forward") (forward (string->number (second cmd)))]
          [(string=? (first cmd) "up") (up (string->number (second cmd)))]
          [(string=? (first cmd) "down") (down (string->number (second cmd)))]
          [else (error "bad cmd")]))
  (define (solve-tail in pos)
    (if (empty? in)
        pos
        (solve-tail (rest in) ((dispatch (first in)) pos))))
  (pos-mul (solve-tail in (make-pos tag 0 0))))

(define ex '("forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2"))
(define puzzle (file->lines "input/day2.txt"))

(check-eq? 150 (solve 'part-one ex))
(check-eq? 2070300 (solve 'part-one puzzle))

(check-eq? 900 (solve 'part-two ex))
(check-eq? 2078985210 (solve 'part-two puzzle))
