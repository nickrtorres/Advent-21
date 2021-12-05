#lang racket

(require rackunit)

(define ex #<<game-state-ex
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
game-state-ex
  )
(define puzzle (file->string "input/day4.txt"))

(define (range n) (build-list 5 values))
(define (cycle v) (make-list 5 v))

(define (square-row square) (first square))
(define (square-col square) (second square))
(define (square-value square) (third square))
(define (square-marked? square) (fourth square))
(define (board-mark-square-at r c board)
  (define (mark-square square)
    (list (square-row square) (square-col square) (square-value square) #t))
  (list-update board r (lambda (row) (list-update row c mark-square))))

(define (make-game-state in)
  (define (make-boards in)
    (define (make-board in)
      (define board-lines (string-split in "\n"))
      (define col-v-row
        (map (lambda (line row)
               (let ([elements (map list (map string->number (string-split line)) (cycle #f))])
                 (cons row (map cons (range 5) elements))))
             board-lines (range 5)))
      (define (make-board-tail in acc)
        (if (empty? in)
            (reverse acc)
            (make-board-tail (rest in)
                             (cons (map cons (cycle (first (first in))) (rest (first in))) acc))))
      (make-board-tail col-v-row '()))
    (map make-board in))
  (define in-split (string-split in "\n\n"))
  (define drawing (map string->number (string-split (first in-split) ",")))
  (define boards (make-boards (rest in-split)))
  (cons drawing boards))
(define (game-state-boards gs) (cdr gs))
(define (game-state-drawings gs) (car gs))

(define (maybe-winner? board)
  (define (any pred lst)
    (> (length (filter pred lst)) 0))
  (define (file-win? board)
    (any (lambda (x) (= x 5)) (map (lambda (row) (count square-marked? row)) board)))
  (define (board-at board row col)
    (list-ref (list-ref board row) col))
  (define (board-vfile board col)
    (define indicis (map cons (build-list 5 values) (make-list 5 col)))
    (map (lambda (p) (board-at board (car p) (cdr p))) indicis))
  (define (vfile-win? board)
    (define candidate-files
      (map (lambda (square) (square-col square))
           (filter square-marked? (first board))))
    (file-win? (map (lambda (col) (board-vfile board col)) candidate-files)))
  (if (or (file-win? board) (vfile-win? board))
      board
      false))

(define (mark-if-contains drawing board)
  (define maybe-found (filter (lambda (b) (not (false? b)))
                              (map (lambda (row)
                                     (findf (lambda (square)
                                              (= drawing (square-value square))) row)) board)))
  (if (not (empty? maybe-found))
      (let ([square (first maybe-found)])
        (board-mark-square-at (square-row square) (square-col square) board))
      board))

(define (play in part)
  (define initial-game-state (make-game-state in))
  (define (play-until-win game-state last-called)
    (define winners (filter maybe-winner? (game-state-boards game-state)))
    (if (not (empty? winners))
        (list game-state last-called (first winners))
        (let ([current-drawing (first (game-state-drawings game-state))])
          (play-until-win (cons (rest (game-state-drawings game-state))
                                (map (lambda (board) (mark-if-contains current-drawing board))
                                     (game-state-boards game-state))) current-drawing))))
  (define (play-until-end game-state last-result)
    (if (empty? (game-state-boards game-state))
        (rest last-result)
        (let* ([result (play-until-win game-state 0)]
               [winning-game-state (first result)]
               [winning-board (third result)]
               [new-boards (remove winning-board (game-state-boards winning-game-state) eq?)])
          (play-until-end (cons (game-state-drawings winning-game-state)
                                new-boards) result))))
  (cond [(eq? part 'part-one) (rest (play-until-win (make-game-state in) 0))]
        [(eq? part 'part-two) (play-until-end (make-game-state in) (cons 0 0))]
        [else (error "invalid part")]))

(define (board-sum-unmarked board)
  (foldl + 0 (map (lambda (row)
                    (foldl (lambda (square acc)
                             (if (square-marked? square) acc (+ (square-value square) acc))) 0 row)) board)))

(define (solve in part)
  (define winner (play in part))
  (* (board-sum-unmarked (second winner)) (first winner)))

(check-eq? 4512 (solve ex 'part-one))
(check-eq? 22680 (solve puzzle 'part-one))
(check-eq? 1924 (solve ex 'part-two))
(check-eq? 16168 (solve puzzle 'part-two))
