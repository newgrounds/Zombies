#lang class/1
(require 2htdp/image)
(require class/universe)

;; A Color is a String
;; A Char is one of:
;;  - Player
;;  - Live-Zombie
;;  - Dead-Zombie
(define-class char%
  (fields x y color)
  
  (define (draw)
    (circle 10 "solid" (field color)))
  
  (define (die? z)
    (<= (distance z) 10))
  
  (define (distance z)
    (+ (abs (- (field x)
               (send z x)))
       (abs (- (field y)
               (send z y))))))


(define-interface zombie<%>
  [x y color
     ;; Draw : -> Image
     ;; Returns an Image of the Zombie
     draw
     ;; die? : [Listof Zombie] -> Boolean
     ;; Determines if the Zombie has collided with the given Zombie
     die?])

;; A Live-Zombie is a (new live-zombie% Number Number Number Number String)
(define-class live-zombie%
  (super char%)
  (implements zombie<%>)
  (fields px py)
  
  ;; Give-Pos : Number Number -> Live-Zombie
  ;; gives the zombie the player position
  (define (give-pos px py)
    (new live-zombie%
         px py
         (field x) 
         (field y)
         (field color)))
  
  ;; Move : -> Zombie
  ;; Move the Zombie based on the player's position
  (define (move)
    (new live-zombie% 
         (field px) (field py)
         (+ (/ (round (- (field px) (field x))) 80) (field x))
         (+ (/ (round (- (field py) (field y))) 80) (field y))
         (field color))))


;; A Dead-Zombie is a (new dead-zombie% Number Number Color)
(define-class dead-zombie%
  (super char%)
  (implements zombie<%>))


(define-interface player<%>
  [x y mx my
     ;; Move : -> player
     ;; Move the player
     move
     ;; Draw : -> Scene
     ;; Places the player on an empty-scene
     draw
     ;; die? : [Listof Zombie] -> Boolean
     ;; Determines if the Player has collided with a Zombie
     die?])

;; A Player is a (new player% Number Number Number Number Color)
(define-class player%
  (super char%)
  (implements player<%>)
  (fields mx my)
  
  (define (move)
    (new player% 
         (field mx) (field my)
         (+ (/ (round (- (field mx) (field x))) 100) (field x))
         (+ (/ (round (- (field my) (field y))) 100) (field y))
         "green")))

;; List-Dead : [Listof Live-Zombies] -> [Listof Live-Zombies]
;; Creates a list of all the zombies that have juse died
;;   from a list of all the previously live zombies
(define (list-dead lolz loaz)
  (cond [(empty? lolz) empty]
        [(ormap (λ (z) (send (first lolz) die? z)) (rest loaz))
         (cons (first lolz) (list-dead (rest lolz) (rest loaz)))]
        [else (list-dead (rest lolz) (rest loaz))]))

;; List-Live : [Listof Live-Zombies] -> [Listof Live-Zombies]
;; Creates an updated list of live zombies without all those dead ones
(define (list-live lolz loaz)
  (cond [(empty? lolz) empty]
        [(ormap (λ (z) (send (first lolz) die? z)) (rest loaz))
         (list-live (rest lolz) (rest loaz))]
        [else (cons (first lolz) 
                    (list-live (rest lolz) (rest loaz)))]))

;; Make-Dead : [Listof Live-Zombies] -> [Listof Dead-Zombies]
;; Changes a list of live-zombies into dead-zombies
(define (make-dead lolz)
  (cond [(empty? lolz) empty]
        [else (map (λ (z) (new dead-zombie% 
                               (send z x)
                               (send z y)
                               "gray")) lolz)]))


;; A World is a (new world% [Listof Dead-Zombie] [Listof Live-Zombie] Player)
(define-class world%
  (fields dead-zombies live-zombies player)
  
  ;; App-Zombies : -> [Listof Zombies]
  ;; Appends the lists of dead-zombies and live-zombies
  (define (app-zombies)
    (append (field live-zombies)
            (field dead-zombies)))
  
  ;; on-mouse : Number Number String -> World
  ;; Handles mouse events
  (define (on-mouse mx my m)
    (cond [(string=? "move" m) (move-mouse mx my)]
          [(string=? "button-down" m) (transport mx my)]
          [else this]))
  
  ;; move-mouse : -> World
  ;; Moves the mouse position
  (define (move-mouse mx my)
    (new world% 
         (field dead-zombies)
         (field live-zombies)
         (new player%
              mx my
              (send (field player) x)
              (send (field player) y)
              (send (field player) color))))
  
  ;; transport : -> World
  ;; transports the player to a random position
  (define (transport mx my)
    (new world%
         (field dead-zombies)
         (field live-zombies)
         (new player%
              mx my
              (random 400)
              (random 400)
              (send (field player) color))))
  
  ;; Name : -> String
  ;; This is the name of the game!
  (define (name) "Zombie!")
  
  ;; to-draw : -> Scene
  ;; Draws everything into one Scene
  (define (to-draw)
    (foldr (λ (z scene) (place-image (send z draw)
                                     (send z x) 
                                     (send z y)
                                     scene))
           (empty-scene 400 400)
           (cons (field player) (app-zombies))))
  
  ;; on-tick : -> World
  ;; Update the World
  (define (on-tick)
    (new world% 
         (append (make-dead (list-dead (field live-zombies) (app-zombies)))
                 (field dead-zombies))
         (foldr (λ (z last) (cons (send (send z 
                                              give-pos
                                              (send (field player) x)
                                              (send (field player) y)) move) last))
                empty
                (list-live (field live-zombies) (app-zombies)))
         (send (field player) move)))
  
  ;; tick-rate : -> Number
  ;; Tells on-tick how often to tick
  (define (tick-rate)
    1/50)
  
  ;; stop-when : -> World
  ;; Produces the last World when the player collides with any Zombies
  (define (stop-when)
    (ormap (λ (z) (send (field player) die? z)) (app-zombies))))

;; Zombs : -> [Listof Zombie]
;; Creates a list of n zombies in random locations
(define (zombs n)
  (cond [(zero? n) empty]
        [else (cons (new live-zombie% 0 0 (random 400) (random 400) "red")
                    (zombs (sub1 n)))]))

(big-bang (new world% 
               (list) 
               (zombs (+ 5 (random 8))) 
               (new player% 0 0 0 0 "green")))