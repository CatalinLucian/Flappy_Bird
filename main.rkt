#lang racket/gui

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "abilities.rkt")
(require "constants.rkt")
;(require "random.rkt")

(provide next-state)
(provide next-state-bird)
(provide next-state-bird-onspace)
(provide change)

(provide get-pipes)
(provide get-pipe-x)
(provide next-state-pipes)
(provide add-more-pipes)
(provide clean-pipes)
(provide move-pipes)

(provide invalid-state?)
(provide check-ground-collision)
(provide check-pipe-collisions)

(provide draw-frame)

(provide get-initial-state)
(provide get-bird)
(provide get-bird-y)
(provide get-bird-v-y)

; pipe
(provide get-pipes)
(provide get-pipe-x)

; score25
(provide get-score)

(provide get-abilities)
(provide get-abilities-visible)
(provide get-abilities-active)
; variables
(provide get-variables)
(provide get-variables-gravity)
(provide get-variables-momentum)
(provide get-variables-scroll-speed)


;Initial state
; Primul pas pe care trebuie sa il facem este sa cream starea initiala a jocului.
; Aceasta va fi salvata in (get-initial-state), si trebuie sa incapsuleze toate informatiile
; necesare jocului, si anume: informatii despre pasare, despre pipes si despre powerups.
; Pe parcursul temei, in state, salvam coordonatele colturilor din stanga sus ale obiectelor.
; Aceasta va face mai usoara logica miscarii obiectelor.
; Toate coordonatele oferite in comentarii sau in fisierul constants.rkt, se refera la
; coltul din stanga sus ale obiectelor.

; Structurile jocului
(define-struct bird_struct (y v-y) #:transparent)
(define-struct pipe (gap-y x) #:transparent)
(define-struct variables (gravity momentum scroll-speed) #:transparent)
(define-struct state_struct (bird pipes variable scor) #:transparent)

; În starea jocului, trebuie să păstrăm informații despre pipes. Pe parcursul jocului,
; pipe-urile se vor schimba, unele vor fi șterse și vor fi adăugate altele.
; După ce definim structura pentru pipe și pentru mulțimea de pipes din stare,
; adăugam primul pipe în starea jocului. Acesta se va află inițial în afara ecranului.
; Celelalte pipe-uri vor fi adăugate ulterior, poziționându-le după acest prim pipe.
; Fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap.
; Colțul din stânga sus al gap-ului dintre componentele primului pipe se va afla inițial la:
;    y = (+ added-number (random random-threshold)), pentru a da un element de noroc jocului,
; și x = scene-width,
; pentru a-l forța să nu fie inițial pe ecran.
(define init-bird (bird_struct bird-initial-y 0))
(define first-pipe (pipe (+ added-number (random random-threshold)) scene-width))
(define init-variables (variables initial-gravity initial-momentum initial-scroll-speed))

; Vrem o modalitate de a păstra scorul jocului. După ce definim structura
; acestuia, adăugam scorul inițial, adică 0, în starea inițială a jocului.
(define (get-initial-state)
  (state_struct init-bird (list first-pipe) init-variables 0))


(define (get-gravity state)
  (variables-gravity (state_struct-variable state)))
(define (get-momentum state)
  (variables-momentum (state_struct-variable state)))
(define (get-scroll-speed state)
  (variables-scroll-speed (state_struct-variable state)))

; După aceasta, implementam un getter care extrage din structura
; pasărea, și un al doilea getter care extrage din structura pasăre
; y-ul curent pe care se află aceasta.
(define (get-bird state)
  (state_struct-bird state))
(define (get-bird-y bird)
  (bird_struct-y bird))

; Trebuie să implementăm logică gravitației. next-state-bird va primi drept
; parametri o structură de tip pasăre, și gravitația(un număr real). Aceasta va adauga
; pozitiei pe y a păsării viteza acesteia pe y, si va adaugă vitezei pe y a păsării,
; gravitația.
(define (next-state-bird bird gravity)
  (match-let ([(bird_struct y v-y) bird])
             (struct-copy bird_struct bird [y (+ y v-y)] [v-y (+ v-y gravity)])))

; După aceasta, implementam un getter care extrage din structura noastra
; viteza pe y a păsării.
(define (get-bird-v-y bird)
  (bird_struct-v-y bird))

; Dorim să existe un mod prin care să imprimăm păsării un impuls.
; Definim funcția next-state-bird-onspace care va primi drept parametri
; o structură de tip pasăre, momentum(un număr real), și va schimba viteza
; pe y a păsării cu -momentum.
(define (next-state-bird-onspace bird momentum)
  (match-let ([(bird_struct _ v-y) bird])
             (struct-copy bird_struct bird [v-y (- momentum)])))

; Change
; Change va fi responsabil de input-ul de la tastatură al jocului.
; Acesta va primi drept parametri o structură de tip stare, și tasta pe
; care am apăsat-o. Aceasta va imprimă păsării momentum-ul, apelând
; funcția next-state-bird-onspace. Pentru orice altă tasta, starea rămâne aceeași.
(define (change current-state pressed-key)
  (match-let ([(state_struct bird _ _ _) current-state])
             (cond [(key=? pressed-key " ") (struct-copy state_struct current-state [bird (next-state-bird-onspace bird (variables-momentum (state_struct-variable current-state)))])]
                   [else current-state])))

; După ce definim structurile pentru mulțimea de pipes și pentru un singur pipe,
; implementam getterul get-pipes, care va extrage din starea jocului mulțimea de pipes,
; sub formă de lista.
(define (get-pipes state)
  (state_struct-pipes state))

; Implementam get-pipe-x ce va extrage dintr-o singură structura de tip pipe, x-ul acesteia.
(define(get-pipe-x pipe)
  (pipe-x pipe))

; Trebuie să implementăm logica prin care se mișcă pipes.
; Funcția move-pipes va primi drept parametri mulțimea pipe-urilor din stare
; și scroll-speed(un număr real). Aceasta va scădea din x-ul fiecărui pipe
; scroll-speed-ul dat.
(define (move-pipes pipes scroll-speed)
  (map (λ (p) (match-let ([(pipe _ x) p])
                         (struct-copy pipe p [x (- x scroll-speed)])))
       pipes))

; Vom implementa logica prin care pipe-urile vor fi șterse din stare. În momentul
; în care colțul din dreapta sus al unui pipe nu se mai află pe ecran, acesta trebuie
; șters.
; Funcția va primi drept parametru mulțimea pipe-urilor din stare.

(define (clean-pipes pipes)
  (filter (λ (p) (if (<= (+ (pipe-x p) pipe-width) 0)
                     #f
                     #t))
          pipes))


; Vrem să avem un sursa continuă de pipe-uri.
; Implementam funcția add-more-pipes, care va primi drept parametru mulțimea pipe-urilor
; din stare și, dacă avem mai puțin de no-pipes pipe-uri, mai adăugăm una la mulțime,
; având x-ul egal cu pipe-width + pipe-gap + x-ul celui mai îndepărtat pipe, în raport
; cu pasărea.
(define (add-more-pipes pipes)
  (if (< (length pipes) no-pipes)
      (cons (pipe (+ added-number (random random-threshold)) (+ pipe-width pipe-gap (pipe-x (car pipes)))) pipes)
      pipes))


; Vrem ca toate funcțiile implementate anterior legate de pipes să fie apelate
; de către next-state-pipes.
; Aceasta va primi drept parametri mulțimea pipe-urilor și scroll-speed-ul,
; și va apela cele trei funcții implementate anterior, în această ordine:
(define (next-state-pipes pipes scroll-speed)
  (let* ((moving (move-pipes pipes scroll-speed))
         (cleaning (clean-pipes moving))
         (adding (add-more-pipes cleaning)))
        adding))

; Cream un getter ce va extrage scorul din starea jocului.
(define (get-score state)
  (state_struct-scor state))

; Vrem să creăm logica coliziunii cu pământul.
; Implementam check-ground-collision, care va primi drept parametru
; o structura de tip pasăre, și returnează true dacă aceasta are coliziune
; cu pământul.

(define (check-ground-collision bird)
 (if (> (+ (bird_struct-y bird) bird-height) ground-y)
     #t
     #f))

; invalid-state?
; invalid-state? îi va spune lui big-bang dacă starea curentă mai este valida,
; sau nu. Aceasta va fi validă atât timp cât nu avem coliziuni cu pământul
; sau cu pipes.
; Aceasta va primi ca parametru starea jocului.
; Vrem să integrăm verificarea coliziunii cu pământul în invalid-state?.
; Odată creată logică coliziunilor dintre pasăre și pipes, vrem să integrăm
; funcția nou implementată în invalid-state?.
(define (invalid-state? state)
  (if (or (check-ground-collision (get-bird state))
          (check-pipe-collisions (get-bird state) (get-pipes state)))
      #t
      #f))

; Odată ce am creat pasărea, pipe-urile, scor-ul și coliziunea cu pământul,
; următorul pas este verificarea coliziunii dintre pasăre și pipes.
; Implementam funcția check-pipe-collisions care va primi drept parametri
; o structură de tip pasăre, mulțimea de pipes din stare, și va returna
; true dacă există coliziuni, și false în caz contrar. Reiterând,
; fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap. Pot există
; coliziuni doar între pasăre și cele două părți. Dacă pasărea se află în
; chenarul lipsă, nu există coliziune.
;
(define (check-pipe-collisions bird pipes)
  (let iter ((l pipes))
            (if (null? l)
                #f
                (if (or (check-collision-rectangles (make-posn bird-x (bird_struct-y bird))
                                                    (make-posn (+ bird-x bird-width) (+ (bird_struct-y bird) bird-height))
                                                    (make-posn (pipe-x (car l)) 0)
                                                    (make-posn (+ (pipe-x (car l)) pipe-width) (pipe-gap-y (car l))))
                        (check-collision-rectangles (make-posn bird-x (bird_struct-y bird))
                                                    (make-posn (+ bird-x bird-width) (+ (bird_struct-y bird) bird-height))
                                                    (make-posn (pipe-x (car l)) (+ pipe-self-gap (pipe-gap-y (car l))))
                                                    (make-posn (+ (pipe-x (car l)) pipe-width) scene-height)))
                    #t
                    (iter (cdr l))))))

(define (check-collision-rectangles A1 A2 B1 B2)
  (match-let ([(posn AX1 AY1) A1]
              [(posn AX2 AY2) A2]
              [(posn BX1 BY1) B1]
              [(posn BX2 BY2) B2])
    (and (< AX1 BX2) (> AX2 BX1) (< AY1 BY2) (> AY2 BY1))))

;Next-state
; Next-state va fi apelat de big-bang la fiecare cadru, pentru a crea efectul de
; animație. Acesta va primi ca parametru o structură de tip stare, și va întoarce
; starea corespunzătoare următorului cadru.


; Vrem ca next-state să incrementeze scorul cu 0.1 la fiecare cadru.
(define (next-state state)
  (state_struct (next-state-bird (get-bird state) (get-gravity state))
                (next-state-pipes (get-pipes state) (get-scroll-speed state))
                (state_struct-variable state)
                (+ 0.1 (get-score state))))



; draw-frame
; draw-frame va fi apelat dupa fiecare apel la next-state, pentru a afisa cadrul curent.
; Fiecare cadru va fi desenat in urmatorul mod:
; bird peste ground, peste scor, peste pipes, peste empty-scene.

; Variabile folosite in aceasta functie:
; bird -> bird-width si bird-height
; ground -> ground-y si ground-height, acesta va acoperi intreaga latime a ecranului
; scor -> text-x si text-y
; pipes -> pipe-width si pipe-height
(define bird-image (rectangle bird-width bird-height  "solid" "yellow"))
(define ground-image (rectangle scene-width ground-height "solid" "brown"))
(define initial-scene (empty-scene scene-width scene-height))

(define text-family (list "Gill Sans" 'swiss 'normal 'bold #f))
(define (score-to-image x)
(if SHOW_SCORE
	(apply text/font (~v (round x)) 24 "indigo" text-family)
	empty-image))

(define (draw-frame state)
  (place-image ground-image (quotient scene-width 2) (+ ground-y (quotient ground-height 2))
               (place-image bird-image (+ bird-x (quotient bird-width 2)) (+ (get-bird-y (get-bird state)) (quotient bird-height 2))
                            (place-pipes (get-pipes state) initial-scene))))

; Folosind `place-image/place-images` va poziționa pipe-urile pe scenă.
(define (place-pipes pipes scene)
  (place-images
   (foldr (λ (p acc) (cons (rectangle pipe-width (pipe-gap-y p) "solid" "green")
                           (cons (rectangle pipe-width (- scene-height (- (pipe-gap-y p) pipe-self-gap)) "solid" "green") acc))) '() pipes)
   (foldr (λ (p acc) (cons (make-posn (+ (pipe-x p) (quotient pipe-width 2)) (quotient (pipe-gap-y p) 2))
                           (cons (make-posn
                                  (+ (pipe-x p) (quotient pipe-width 2))
                                  (+ (+ (pipe-gap-y p) pipe-self-gap) (quotient (- scene-height (- (pipe-gap-y p) pipe-self-gap)) 2))) acc))) '() pipes)
   scene))


(define fast-ability 'your-code-here)


(define slow-ability 'your-code-here)

(define ABILITIES (list slow-ability fast-ability))


(define get-variables 'your-code-here)
(define get-variables-gravity 'your-code-here)
(define get-variables-momentum 'your-code-here)
(define get-variables-scroll-speed 'your-code-here)


(define (get-abilities x) null)

(define (get-abilities-visible x) null)

(define (get-abilities-active x) null)

(define (clean-abilities abilities)
	'your-code-here)

(define (move-abilities abilities scroll-speed)
	'your-code-here)

(define (time-counter abilities)
	'your-code-here)

(define (next-abilities-visible visible scroll-speed)
	'your-code-here)

(define (next-abilities abilities bird scroll-speed)
	'your-code-here)

(define (next-variables variables abilities)
  'your-code-here)


(define (place-visible-abilities abilities scene)
	'your-code-here)

(define (place-active-abilities abilities scene)
	'your-code-here)

(module+ main
	(big-bang (get-initial-state)
	 [on-tick next-state (/ 1.0 fps)]
	 [to-draw draw-frame]
	 [on-key change]
	 [stop-when invalid-state?]
	 [close-on-stop #t]
	 [record? #f]))
