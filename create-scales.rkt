(require srfi/1)
(require rsound)

(define (decimal-to-cont-frac decimal timeout)
  ;(decimal-to-cont-frac (sqrt 2) 5) -> '(1 2 2 2 2 2)
  (let ((int-part (exact-floor decimal)))
    (if (or (eq? 0 timeout) (< (- decimal int-part) 0.00001))
        (cons int-part '())
        (append (cons int-part '())
                (decimal-to-cont-frac (/ 1 (- decimal int-part))
                                      (- timeout 1))))))

(define (sequence-to-fraction seq)
  ;(sequence-to-fraction '(1 2 2 2 2 2)) -> 99/70
  (if (eq? 1 (length seq))
      (car seq)
      (+ (car seq) (/ 1 (sequence-to-fraction (cdr seq))))))

(define fifth-approx (/ (log (/ 3 2)) (log 2)))
(define fifth-approx-fracs
  (let ((cont-frac-fifth (decimal-to-cont-frac fifth-approx 10)))
    (map (lambda (i) (sequence-to-fraction (take cont-frac-fifth i)))
         (iota (length cont-frac-fifth) 1))))
;'(0 1 1/2 3/5 7/12 24/41 31/53 179/306 389/665 9126/15601)

(define (play-pitches . pitches)
  (let ((tones (map (lambda (p) (make-tone p 0.5 (/ FRAME-RATE 4))) pitches)))
    (play (rs-append* tones))))

;(play-pitches 300 400 500)
(define (create-scale fifth subdivs num-notes)
  (let ((note-indices (sort (map (lambda (x) (modulo (* fifth x) subdivs))
                                 (iota num-notes))
                            <)))
    (lambda (start)
      (map (lambda (i) (* start (expt 2 (/ i subdivs))))
           note-indices))))

(define pentatonic-lydian (create-scale 7 12 5))
(define lydian (create-scale 7 12 7))
(define lydian-blues (create-scale 7 12 7))
(define chromatic (create-scale 7 12 12))

(define (ascend-and-descend scale)
  (append scale (cons (* 2 (car scale)) (reverse scale))))

(apply play-pitches (ascend-and-descend (pentatonic-lydian 400)))
(apply play-pitches (ascend-and-descend (lydian 400)))
(apply play-pitches (ascend-and-descend (lydian-blues 400)))
(apply play-pitches (ascend-and-descend (chromatic 400)))

; within 10 hz of (12 based) pentatonic lydian
(apply play-pitches (ascend-and-descend ((create-scale 3 5 5) 400)))
; within 5 hz of (12 based) pentatonic lydian
(apply play-pitches (ascend-and-descend ((create-scale 24 41 5) 400)))
(apply play-pitches (ascend-and-descend ((create-scale 24 41 7) 400)))
; ~ (12 based) chromatic scale
(apply play-pitches (ascend-and-descend ((create-scale 24 41 12) 400)))
; half chromatic scale
(define half-chromatic (create-scale 24 41 24))
(apply play-pitches (ascend-and-descend (half-chromatic 400)))

; could draw pretty interval symmetry pictures and turn into blog post
(define (create-random-melody scale len)
  ;(play (create-random-melody (lydian 400) 10))
  (let* ((scale+oct (append scale
                            (cons (* 2 (car scale)) '())))
         (rand-notes (map (lambda _ (list-ref scale (random (length scale))))
                          (iota len)))
         (rand-times (map (lambda _ (list-ref '(11025 22050) (random 2)))
                          (iota len))))
    (rs-append*
     (map (lambda (p t) (make-tone p 0.5 t)) rand-notes rand-times))))

(play (create-random-melody (lydian 400) 20))
(play (create-random-melody ((create-scale 24 41 7) 400) 20)) ; more accurate
(play (create-random-melody (half-chromatic 400) 20))
(play (create-random-melody ((create-scale 31 53 31) 400) 30)) ; even smaller step size
