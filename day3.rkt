#lang racket
(require rackunit)

(define ex '("00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"))
(define puzzle (file->lines "input/day3.txt"))

(define (make-bin-report)
  (cons 0 0))

(define (bin-report-inc-hi bin-report)
  (cons (add1 (car bin-report)) (cdr bin-report)))

(define (bin-report-inc-lo bin-report)
  (cons (car bin-report) (add1 (cdr bin-report))))

(define (bin-report-value bits offset tag)
  (define (get-value bin-report)
    (define (gamma-value bin-report)
      (if (> (car bin-report) (cdr bin-report)) 1 0))
    (define (epsilon-value bin-report)
      (if (< (car bin-report) (cdr bin-report)) 1 0))
    (cond [(eq? tag 'gamma) (gamma-value bin-report)]
          [(eq? tag 'epsilon) (epsilon-value bin-report)]
          [else (error "bad tag")]))
  (define (count bit acc)
    (cond [(= bit 0) (bin-report-inc-lo acc)]
          [(= bit 1) (bin-report-inc-hi acc)]
          [else (error "invalid bin")]))

  (arithmetic-shift (get-value (foldl count (make-bin-report) bits)) offset))

(check-eq? 32 (bin-report-value '(1 0 1 0 1) 5 'gamma))

(define (char->number x)
  (- (char->integer x) 48))

(define (bin-value bins tag)
  (define (inner bins acc offset)
    (if (empty? (first bins))
        acc
        (inner (map rest bins)
               (bitwise-ior acc
                            (bin-report-value (map char->number (map first bins)) offset tag))
               (sub1 offset))))
  (inner (map string->list bins) 0 (sub1 (string-length (first bins)))))


(check-eq? 22 (bin-value ex 'gamma))

;; Part 1

(define (solve in)
  (define gamma (bin-value in 'gamma))
  (define epsilon (bin-value in 'epsilon))
  (* gamma epsilon))

(check-eq? 198 (solve ex))
(check-eq? 1131506 (solve puzzle))

;; Part 2

(define (solve-two in)
  (define (rating in mode)
    (define (make-tagged-bin in i) (cons in i))
    (define (tagged-bin-value tb) (car tb))
    (define (tagged-bin-index tb) (cdr tb))
    (define (tagged-bin-msb tb)
      (char->number (car (tagged-bin-value tb))))
    (define (tagged-bin-shiftl tb)
      (make-tagged-bin (cdr (tagged-bin-value tb)) (tagged-bin-index tb)))
    (define (tag-bins in acc i)
      (if (empty? in)
          (reverse acc)
          (tag-bins (rest in)
                    (cons (make-tagged-bin (string->list (first in)) i) acc)
                    (add1 i))))
    (define (cv-mode in)
      (define hi (count (lambda (x) (= (tagged-bin-msb x) 1)) in))
      (define lo (count (lambda (x) (= (tagged-bin-msb x) 0)) in))
      (cond [(eq? mode 'oxygen) (if (>= hi lo) 1 0)]
            [(eq? mode 'carbon-dioxide) (if (< hi lo) 1 0)]
            [else (error "bad mode")]))
    (define (filter-cv in cv)
      (filter (lambda (x) (= (tagged-bin-msb x) cv)) in))
    (define (rating-tail in)
      (if (= (length in) 1)
          (tagged-bin-index (first in))
          (let ([cv (cv-mode in)])
            (rating-tail (map tagged-bin-shiftl (filter-cv in cv))))))
    (define tagged-bins (tag-bins in '() 0))
    (define bin-index (rating-tail tagged-bins))
    (string->number (list-ref in bin-index) 2))
  (define oxygen (rating in 'oxygen))
  (define carbon-dioxide (rating in 'carbon-dioxide))
  (* oxygen carbon-dioxide))

(check-eq? 230 (solve-two ex))
(check-eq? 7863147 (solve-two puzzle))
