#lang racket/gui

(require bazaar/gui/board
         bazaar/matrix
         bazaar/mutation
         bazaar/cond-let)

#;(random-seed 1234)

(module+ test
  (require rackunit))

(define n-cells 22)

(define color-names '("red" "blue" "darkgreen" "yellow" "violet" "turquoise" "orange"))
(define n-colors (length color-names))
(define colors (build-list n-colors values))
(define n-plays 0)
(define mat #f)

(struct player (row col score)
  #:transparent
  #:mutable)
(define player1 (player 0 0 0))
(define player2 (player (- n-cells 1) (- n-cells 1) 0))

(define player1-col (player-col player1))
(define player1-row (player-row player1))
(define player2-col (player-col player2))
(define player2-row (player-row player2))

(define (player-color mat pl)
  (matrix-ref mat (player-col pl) (player-row pl)))

(define (forbidden-colors mat)
  (list (player-color mat player1)
        (player-color mat player2)))

(define (set-mat! m)
  (set! mat m))

(define (reset-game!)
  (set! n-plays 0)
  (set-player-score! player1 0)
  (set-player-score! player2 0)
  (matrix-map!
   mat
   (λ(row col v)
     (random n-colors))))

(define (mat-in-bounds? mat col row)
  (and (>= col 0)
       (>= row 0)
       (< col (matrix-ncols mat))
       (< row (matrix-nrows mat))))

;; Returns the number of cells painted with the same color as the one in (col row)
;; that can be reached for (col row).
(define (mat-score mat col row)
  (define from-color (matrix-ref mat col row))
  (define counted? (make-hash))
  (let loop ([col col] [row row])
    (define key (cons col row))
    (cond [(and (mat-in-bounds? mat col row)
                (= from-color (matrix-ref mat col row))
                (not (hash-ref counted? key #f)))
           (hash-set! counted? key #t)
           (+ 1
              (loop col (+ row 1))
              (loop col (- row 1))
              (loop (+ col 1) row)
              (loop (- col 1) row))]
          [else 0])))

(define (mat-flood! mat col row to-color)
  (define from-color (matrix-ref mat col row))
  (define n-tests 0)
  (when (not (= to-color from-color))
    (let loop ([col col] [row row])
      (set! n-tests (+ 1 n-tests))
      (when (and (mat-in-bounds? mat col row)
                 (= from-color (matrix-ref mat col row)))
        (matrix-set! mat col row to-color)
        (loop col (+ row 1))
        (loop col (- row 1))
        (loop (+ col 1) row)
        (loop (- col 1) row))))
  #;(displayln (list 'n-tests: n-tests))
  (not (= to-color from-color)))

;; Returns the number of cells that can be captured by flooding
(define (test-flood mat col row to-color)
  (define from-color (matrix-ref mat col row))
  (define counted? (make-hash))
  (if (= to-color from-color)
      0
      (let loop ([col col] [row row] [count? #f])
        (cond-let [(not (mat-in-bounds? mat col row)) 0]
                  #:let* ([c    (matrix-ref mat col row)]
                          [c=to (= c to-color)]
                          [key  (cons col row)])
                  [(and (or c=to (= c from-color))
                        (not (hash-ref counted? key #f)))
                   (hash-set! counted? key #t)
                   (+ (if c=to 1 0)
                      (loop col (+ row 1) c=to)
                      (loop col (- row 1) c=to)
                      (loop (+ col 1) row c=to)
                      (loop (- col 1) row c=to))]
                  [else 0]))))


;; Returns the final score after playing a sequence of moves (to-colors).
(define (test-flood* mat col row to-colors)
  (define from-color (matrix-ref mat col row))
  (define counted? (make-hash))
  (if (= (first to-colors) from-color)
      0
      (let loop ([col col] [row row]
                 [from-color from-color] [to-colors to-colors])
        (define key (cons col row))
        (cond-let [(or (not (mat-in-bounds? mat col row))
                       (hash-ref counted? key #f))
                   0]
                  #:let* ([c (matrix-ref mat col row)]
                          [to-color (if (empty? to-colors) -1 (first to-colors))]
                          [c=to (= c to-color)]
                          [next-from-color (if c=to to-color from-color)]
                          [next-to-colors  (if c=to (rest to-colors) to-colors)])
                  [(or c=to (= c from-color))
                   (hash-set! counted? key #t)
                   (+ 1
                      (loop col (+ row 1) next-from-color next-to-colors)
                      (loop col (- row 1) next-from-color next-to-colors)
                      (loop (+ col 1) row next-from-color next-to-colors)
                      (loop (- col 1) row next-from-color next-to-colors))]
                  [(not (empty? to-colors))
                   ; try again with the next color in line
                   (loop col row (first to-colors) (rest to-colors))]
                  [else 0]))))

;; From starting positions (the current frontier), returns the new frontier
;; of cells that have not yet been used.
(define (color-frontier mat frontier-dict used-dict color)
  (define changed 0)
  (for ([key (in-list (dict-keys frontier-dict))])
    (define col (car key))
    (define row (cdr key))
    (define c (matrix-ref mat col row))
    (when (= c color)
      (dict-set! used-dict key #t)
      (dict-remove! frontier-dict key)
      (++ changed)
      (for ([col2 (list (- col 1)  col        col        (+ col 1))]
            [row2 (list row        (- row 1)  (+ row 1)  row)])
        (define key2 (cons col2 row2))
        (when (and (mat-in-bounds? mat col2 row2)
                   (not (dict-ref used-dict key2 #f)))
          (dict-set! frontier-dict key2 #t)))))
  changed)

#;
(define (test-flood* mat col row to-colors)
  (define front (make-hash `(((,col . ,row) . #t))))
  (define used (make-hash))
    (for ([color (cons (matrix-ref mat col row) to-colors)])
      (let loop ()
        (when (> (color-frontier mat front used color) 0)
          (loop))))
  #;(values front used)
  (dict-count used))

#;(define mat2 #f)
;; Plays a sequence of floods and returns the final score
#;
(define (test-flood* mat col row to-colors)
  (if mat2
      (matrix-copy! mat2 mat)
      (set! mat2 (matrix-copy mat)))
  (for ([to-color to-colors])
    (mat-flood! mat2 col row to-color))
  (mat-score mat2 col row))


(define (play-color to-color depth)
  (when (not (memv to-color (forbidden-colors mat)))
    (mat-flood! mat player1-col player1-row to-color)
    ; Automatic opponent
    (mat-flood! mat player2-col player2-row
                (best-color mat player2-col player2-row depth
                            (forbidden-colors mat)))
    (++ n-plays)
    (update-scores! player1 player2)))

(define (auto-play)
  (play-color (argmax
               (λ(c)
                 (define n (test-flood mat 0 0 c))
                 (displayln (list 'color: (list-ref color-names c) 'test-flood: n))
                 n)
               colors)))

;; Returns the list of all sequences of elements of lst of the specified depth.
;; If with-rep? is false, it does not include sequences where two adjacent elements are `equal?`.
;; (This can make a huge difference when lst is small. For example if lst is of size 2,
;; the number of possible sequences is always 2, vs 2^depth if with-rep is #t.)
(define (all-seqs lst depth [with-rep? #t])
  (let loop ([ll '(())] [depth depth])
    (if (= depth 0)
        ll
        (loop (for*/list ([l ll]
                          [x lst]
                          #:when (or with-rep?
                                     (empty? l)
                                     (not (equal? x (first l)))))
                (cons x l))
              (- depth 1)))))

(module+ test
  (check-equal? (all-seqs '(a b c) 0) '(()))
  (check-equal? (all-seqs '(a b c) 1) '((a) (b) (c)))
  (check-equal? (all-seqs '(a b c) 2)
                '((a a) (b a) (c a) (a b) (b b) (c b) (a c) (b c) (c c)))
  (check-equal? (all-seqs '(a b c) 2 #f)
                '((b a) (c a) (a b) (c b) (a c) (b c))))

(define (best-color mat col row depth [forbid-colors '()])
  (first (argmax 
          (λ(l)(test-flood* mat col row l))
          (filter
           (λ(l)(not (memv (first l) forbid-colors)))
           (all-seqs colors depth #t)))))

(define (auto-play-depth depth)
  (play-color (best-color mat 0 0 depth)))

(define (update-scores! . players)
  (for ([pl players])
    (set-player-score! pl (mat-score mat (player-col pl) (player-row pl)))))

(define (game-over? mat . players)
  (= (* (matrix-ncols mat) (matrix-nrows mat))
     (apply + (map player-score players))))

;===========;
;=== GUI ===;
;===========;

(define (gui-reset-game!)
  (reset-game!)
  (refresh))

(define (gui-auto-play)
  (define depth (send slider-level get-value))
  #;(auto-play)
  #;(auto-play-depth depth)
  #;(refresh)
  (time
   (for ([i 36])
     (auto-play-depth depth)
     (refresh)
     (sleep/yield 0.1))))

(define (gui-play-color c)
  (play-color c (send slider-level get-value))
  (refresh)
  (when (game-over? mat player1 player2)
    (define d (- (player-score player1) (player-score player2)))
    (message-box "Game over"
                 (cond [(< d 0) "You lose"]
                       [(> d 0) "You win!"]
                       [else "Draw game"])
                 frame)))

(define (refresh)
  (define fc (forbidden-colors mat))
  (for ([cb color-buttons] [c (in-naturals)])
    (send cb enable (not (memv c fc))))
  (msg-n-plays-refresh)
  (send board draw)
  (send frame refresh))

(define (msg-n-plays-refresh)
  (send msg-n-plays set-label (format "Moves: ~a\tPlayer1: ~a\tPlayer2: ~a"
                                      n-plays (player-score player1) (player-score player2)))
  (send msg-n-plays refresh))

(define (on-char k-evt)
  (when (eq? (send k-evt get-key-code) 'release)
    (define chr (send k-evt get-key-release-code))
    (writeln (list 'char: chr))
    (case chr
      [(#\return)
       (for ([c n-colors])
         (displayln (list 'color: (list-ref color-names c)
                          'test-flood: (test-flood mat 0 0 c))))])))

(define (on-left-up xc yc)
  (define c (board-ref board xc yc))
  (displayln (list 'color: (list-ref color-names c)))
  (gui-play-color c))

(define (on-right-up xc yc)
  (define c (board-ref board xc yc))
  (displayln (list 'color: (list-ref color-names c)
                   'test-flood: (test-flood mat 0 0 c))))

(define frame (new frame% [label "Flood"]))
(define board (new board%
                   [parent frame]
                   [on-char on-char]
                   [on-left-up on-left-up]
                   [on-right-up on-right-up]
                   [num-cell-x n-cells]
                   [stretchable-width #f] [stretchable-height #f]
                   [cell-dx 20]))

(define msg-n-plays (new message% [parent frame] [label ""]
                         [auto-resize #t]))
(define color-button-panel (new horizontal-panel% [parent frame]
                                [stretchable-height #f]
                                [alignment '(center center)]))

(define color-vec
  (for/vector ([c color-names])
    (send board make-cell-pic c)))

(define color-buttons
  (for/list ([c-bmp color-vec] [c colors])
    (new button% [parent color-button-panel] [label c-bmp]
         [callback (λ(bt ev)(gui-play-color c))])))

(define button-panel (new horizontal-panel% [parent frame]
                          [stretchable-height #f]
                          [alignment '(center center)]))
(define button-init (new button% [parent button-panel] [label "Restart Game"]
                         [callback (λ(bt ev)(gui-reset-game!))]))
#;
(define button-auto (new button% [parent button-panel] [label "Auto play"]
                         [callback (λ(bt ev)(gui-auto-play))]))
(define max-level 5)
(define slider-level (new slider% [parent button-panel] [label "Level"]
                          [min-value 1] [max-value max-level] [init-value 1]
                          [min-width (* 40 max-level)]
                          [stretchable-width #f]))

(send board set-cell-pic 
      (λ(i j v)(vector-ref color-vec v)))

(set-mat! (board-get-matrix board))
(gui-reset-game!)

(send frame show #t)
