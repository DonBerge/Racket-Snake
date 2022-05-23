;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname racket-snake) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; last-key: ultima tecla presionada
; snake: lista de posn
; fruits: posiciones de las frutas
(define-struct game [last-key snake fruits points])

(define FRAME-DELAY 0.07)

; FONDO DE LA ESCENA
(define ANCHO 1800)
(define ALTO 1000)
(define FONDO (empty-scene ANCHO ALTO))

; SERPIENTE
(define ANCHO-SERPIENTE 30)
(define ALTO-SERPIENTE 30)
(define COLOR-SERPIENTE "cyan")
(define BORDE-SIZE 2)
(define CUERPO (overlay (rectangle ANCHO-SERPIENTE ALTO-SERPIENTE "outline" (pen "black" BORDE-SIZE "solid" "round" "round")) (rectangle ANCHO-SERPIENTE ALTO-SERPIENTE "solid" COLOR-SERPIENTE)))
(define POS-INICIO (make-posn (/ ANCHO 2) (/ ALTO 2)))
(define DIFF-HITBOX 5)
(define HITBOX (crop 0 0 (- ANCHO-SERPIENTE DIFF-HITBOX) (- ALTO-SERPIENTE DIFF-HITBOX) CUERPO))

;ESPACIO DE MOVIMIENTO DE LA SERPIENTE
(define DELTA-X (- ANCHO-SERPIENTE (/ BORDE-SIZE 2)))
(define DELTA-Y (- ALTO-SERPIENTE (/ BORDE-SIZE 2)))


;OVERLAPING DE ENTORNOS
(define (entorno-x imagen coords) (make-posn (- (posn-x coords) (/ (image-width imagen) 2)) (+ (posn-x coords) (/ (image-width imagen) 2))))
(define (entorno-y imagen coords) (make-posn (- (posn-y coords) (/ (image-height imagen) 2)) (+ (posn-y coords) (/ (image-height imagen) 2))))

(define (overlap-entorno? a b) (and (< (posn-x a) (posn-y b)) (< (posn-x b) (posn-y a))))

(define (overlap-imagen? imagen1 coords1 imagen2 coords2) (and (overlap-entorno? (entorno-x imagen1 coords1) (entorno-x imagen2 coords2)) (overlap-entorno? (entorno-y imagen1 coords1) (entorno-y imagen2 coords2))) )

;FRUTAS
(define ANCHO-FRUTA 30)
(define ALTO-FRUTA 30)
(define COLOR-FRUTA "red")
(define BORDE-FRUTA 2)
(define CANT-FRUTA 10)
(define FRUTA (overlay (ellipse ANCHO-FRUTA ALTO-FRUTA "outline" (pen "black" BORDE-FRUTA "solid" "round" "round")) (ellipse ANCHO-FRUTA ALTO-FRUTA "solid" COLOR-FRUTA)))

(define (random-between a b) (+ (random (- b a)) a))

(define (generate-fruit-list n) (cond [(<= n 0) null]
                                      [else (list* (make-posn (random-between (/ ANCHO-FRUTA 2) (- ANCHO (/ ANCHO-FRUTA 2))) (random-between (/ ALTO-FRUTA 2) (- ALTO (/ ALTO-FRUTA 2)))) (generate-fruit-list (- n 1)))]
                                      )
  )

(define (add-fruit fruits) (append fruits (generate-fruit-list 1)))

;; SNAKE MOVIMIENTO
(define (last snake) (first (reverse snake)))

(define (map-movement k) (cond
                             [(key=? (string-downcase k) "w") (make-posn 0 -1)]
                             [(key=? (string-downcase k) "s") (make-posn 0 1)]
                             [(key=? (string-downcase k) "a") (make-posn -1 0)]
                             [(key=? (string-downcase k) "d") (make-posn 1 0)]
                             [else (make-posn 0 0)]
                            )
  )

(define (build-list n val) (if (<= n 0) null (append (list val) (build-list (- n 1) val))))

(define (move-body snake) (rest snake))

(define (move-head k snake) (make-posn (+ (* (posn-x (map-movement k)) DELTA-X) (posn-x (last snake))) (+ (* (posn-y (map-movement k)) DELTA-Y) (posn-y (last snake))) ))

(define (move-snake k snake) (append (move-body snake) (list (move-head k snake))))

; PANTALLA GAME OVER
(define game-over-screen (overlay (text "YOU DIED" (min (floor (/ ALTO 6)) 255) "red") (rectangle ANCHO (/ ALTO 4) "solid" "black")))

; INTERACTIVIDAD
(define (draw-snake snake) (place-images (build-list (length snake) CUERPO) snake FONDO))
(define (draw-fruits coords  fondo) (place-images (build-list (length coords) FRUTA) coords fondo))

(define (posn=? a b) (and (= (posn-x a) (posn-x b)) (= (posn-y a) (posn-y b))))

(define (oposite k) (cond [(key=? (string-downcase k) "w") "s"]
                          [(key=? (string-downcase k) "s") "w"]
                          [(key=? (string-downcase k) "a") "d"]
                          [(key=? (string-downcase k) "d") "a"]
                          [else k]
                          )
  )

(define (handle-keyboard s k) (cond [(key=? (string-downcase k) (oposite (game-last-key s))) s]
                                    [else (make-game k (game-snake s) (game-fruits s) (game-points s))]
                                    )
  )

(define (cabeza-toca-parte-de? snake-head fruits imagen) (cond [(empty? fruits) (make-posn -1 -1)]
                                              [else (if (overlap-imagen? CUERPO snake-head imagen (first fruits)) (first fruits) (cabeza-toca-parte-de? snake-head (rest fruits) imagen))]
                                              )
  )

(define (erase-val l val) (cond [(empty? l) null]
                                ;[(not (= (type (first l)) (type val))) l]
                                [(posn=? (first l) val) (rest l)]
                                [else (list* (first l) (erase-val (rest l) val))]
                                )
  )

(define (do-player-move s) (cond [(not (posn=? (make-posn -1 -1) (cabeza-toca-parte-de? (last (game-snake s)) (game-fruits s) FRUTA)))

                                  (make-game
                                   (game-last-key s)

                                   (move-snake (game-last-key s) (list* (first (game-snake s)) (game-snake s)))

                                   (add-fruit (erase-val (game-fruits s) (cabeza-toca-parte-de? (last (game-snake s)) (game-fruits s) FRUTA)))

                                   (+ (game-points s) 1)
                                  )
                                  
                                 ]
                                 
                                 [else
                                  (make-game
                                   (game-last-key s)

                                   (move-snake (game-last-key s) (game-snake s))

                                   (game-fruits s)

                                   (game-points s)
                                  )
                                  ]
  )
)

(define (snake-toco-borde? snake-head) (or
                                        (>= 0 (- (posn-x snake-head) (/ ANCHO-SERPIENTE 2)))
                                        (>= 0 (- (posn-y snake-head) (/ ALTO-SERPIENTE 2)))
                                        (< ALTO (+ (posn-y snake-head) (/ ALTO-SERPIENTE 2)))
                                        (< ANCHO (+ (posn-x snake-head) (/ ANCHO-SERPIENTE 2)))
                                        )
  )

(define (escena-juego s) (place-image (text (number->string (game-points s)) 20 "black") (/ ANCHO 2) 20 (draw-fruits (game-fruits s) (draw-snake (game-snake s)))))

(define (escena-juego-moriste s) (overlay game-over-screen (escena-juego s)))

(define (game-over? s) (or (snake-toco-borde? (last (game-snake s)))
                           (not (posn=? (make-posn -1 -1) (cabeza-toca-parte-de? (last (game-snake s)) (rest (reverse (game-snake s))) HITBOX)))
                           )
  )

(big-bang (make-game "d" (list POS-INICIO) (generate-fruit-list CANT-FRUTA) 0)
  [to-draw escena-juego]
  ;[display-mode 'fullscreen ANCHO]
  [on-key handle-keyboard]
  [on-tick do-player-move FRAME-DELAY]
  [stop-when game-over? escena-juego-moriste]
  [close-on-stop 1]
)