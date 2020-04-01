#lang racket/gui
;Ignorați următoarele linii de cod. Conțin import-uri și export-uri necesare checker-ului.

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "random.rkt")
(require "abilities.rkt")
(require "constants.rkt")
;---------------------------------------checker_exports------------------------------------------------
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

;---------------------------------------checker_exports------------------------------------------------
; Checker-ul contine un numar de teste, fiecare cu numele sau. In acest fisier veti gasi comentarii
; care incep cu TODO %nume_test, unde trebuie sa modificati sau sa implementati o functie, pentru
; a trece testul %nume_test.
;
;Initial state
; Primul pas pe care trebuie sa il facem este sa cream starea initiala a jocului.
; Aceasta va fi salvata in (get-initial-state), si trebuie sa incapsuleze toate informatiile
; necesare jocului, si anume: informatii despre pasare, despre pipes si despre powerups.
; Recomandam ca in pasare, sa retineti, printre altele, informatii despre y-ul curent
; si viteza pe y
; Pe parcursul temei, in state, salvati coordonatele colturilor din stanga sus ale obiectelor.
; Aceasta va face mai usoara atat logica miscarii obiectelor, cat si testarea cerintelor.
; Toate coordonatele oferite in comentarii sau in fisierul constants.rkt, se refera la
; coltul din stanga sus ale obiectelor!

;TODO 1
; După ce definiți structurile lui (get-initial-state) și a păsării, introduceți în prima
; pe cea din urmă. Colțul din stânga sus a păsării se va află inițial la:
;    y = bird-inițial-y
; și x = bird-x.
; (get-initial-state) va fi o funcție care va returna starea inițială a jocului.
(define-struct State (bird pipes score variables abilities) #:transparent)
(define-struct Bird (x y acc) #:transparent)
(define-struct Pipe (x y) #:transparent)
(define-struct Variables (gravity momentum scroll) #:transparent)
(define init-variables (Variables initial-gravity initial-momentum initial-scroll-speed))
(define-struct Abilities (visible active))

;TODO 8
; În starea jocului, trebuie să păstrăm informații despre pipes. Pe parcursul jocului,
; pipe-urile se vor schimba, unele vor fi șterse și vor fi adăugate altele.
; După ce definiți structura pentru pipe și pentru mulțimea de pipes din stare,
; adăugați primul pipe în starea jocului. Acesta se va află inițial în afară ecranului.
; Celelalte pipe-uri vor fi adăugate ulterior, poziționându-le după acest prim pipe.
; Atenție! Fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap.
; Colțul din stânga sus al gap-ului dintre componentele primului pipe se va afla inițial la:
;    y = (+ added-number (random random-threshold)), pentru a da un element de noroc jocului,
; și x = scene-width,
; pentru a-l forța să nu fie inițial pe ecran.
; Atenție! Recomandăm să păstrați în stare colțul din stânga sus al chenarului lipsa
; dintre cele 2 pipe-uri!

;TODO 16
; Vrem o modalitate de a păstra scorul jocului. După ce definiți structura
; acestuia, adăugați scorul inițial, adică 0, în starea inițială a jocului.
; Atenție get-initial-state trebuie sa fie o funcție
; și trebuie apelată în restul codului.
(define (get-initial-state)
  (State (Bird bird-x bird-initial-y 0)
         (list (Pipe scene-width (+ added-number (random random-threshold))))
         0
         init-variables
         (Abilities (fill-abilities '() DISPLAYED_ABILITIES ABILITIES) '())
         ))

;TODO 2
; După aceasta, implementați un getter care extrage din structura voastră
; pasărea, și un al doilea getter care extrage din structura pasăre
; y-ul curent pe care se află această.
(define (get-bird state)
  (State-bird state))
(define (get-bird-y bird)
  (Bird-y bird))
(define (get-bird-x bird)
  (Bird-x bird))
(define (get-momentum variables)
  (Variables-momentum variables))
(define (get-gravity variables)
  (Variables-gravity variables))
(define (get-scroll variables)
  (Variables-scroll variables))

;MYCHECK:
;(get-bird-y (get-bird (get-initial-state)))

;TODO 3
; Trebuie să implementăm logică gravitației. next-state-bird va primi drept
; parametri o structură de tip pasăre, și gravitația(un număr real). Aceasta va adaugă
; pozitiei pe y a păsării viteza acesteia pe y, si va adaugă vitezei pe y a păsării,
; gravitația.
(define (next-state-bird bird gravity)
  (struct-copy Bird bird [y (+ (Bird-y bird) (Bird-acc bird))] [acc (+ (Bird-acc bird) gravity)]))

;TODO 4
; După aceasta, implementati un getter care extrage din structura voastră
; viteza pe y a păsării.
(define (get-bird-v-y bird)
  (Bird-acc bird))

;TODO 6
; Dorim să existe un mod prin care să imprimăm păsării un impuls.
; Definiți funcția next-state-bird-onspace care va primi drept parametri
; o structură de tip pasăre, momentum(un număr real), și va schimba viteza
; pe y a păsării cu -momentum.
(define (next-state-bird-onspace bird momentum)
  (struct-copy Bird bird [acc (- 0 momentum)]))

; Change
; Change va fi responsabil de input-ul de la tastatură al jocului.
;TODO 7
; Acesta va primi drept parametri o structură de tip stare, și tasta pe
; care am apăsat-o. Aceasta va imprimă păsării momentum-ul, apelând
; funcția next-state-bird-onspace. Pentru orice altă tasta, starea rămâne aceeași.
(define (change current-state pressed-key)
  (if (key=? pressed-key " ")
      (State (next-state-bird-onspace (get-bird current-state) (get-variables-momentum current-state))
             (get-pipes current-state)
             (get-score current-state)
             (get-variables current-state)
             (get-abilities current-state))
      current-state))

;TODO 9
; După ce ați definit structurile pentru mulțimea de pipes și pentru un singur pipe,
; implementați getterul get-pipes, care va extrage din starea jocului mulțimea de pipes,
; sub formă de lista.
(define (get-pipes state)
  (State-pipes state))

;TODO 10
; Implementați get-pipe-x ce va extrage dintr-o singură structura de tip pipe, x-ul acesteia.
(define(get-pipe-x pipe)
  (Pipe-x pipe))
(define(get-pipe-y pipe)
  (Pipe-y pipe))

;TODO 11
; Trebuie să implementăm logica prin care se mișcă pipes.
; Funcția move-pipes va primi drept parametri mulțimea pipe-urilor din stare
; și scroll-speed(un număr real). Aceasta va scădea din x-ul fiecărui pipe
; scroll-speed-ul dat.
(define (move-pipes pipes scroll-speed)
  (map (lambda (pipe) (struct-copy Pipe pipe [x (- (get-pipe-x pipe) scroll-speed)])) pipes))

;TODO 12
; Vom implementa logica prin care pipe-urile vor fi șterse din stare. În momentul
; în care colțul din DREAPTA sus al unui pipe nu se mai află pe ecran, acesta trebuie
; șters.
; Funcția va primi drept parametru mulțimea pipe-urilor din stare.
;
; Hint: cunoaștem lățimea unui pipe, pipe-width
(define (clean-pipes pipes)
  (filter (lambda (pipe) (> (+ (get-pipe-x pipe) pipe-width) (- 0 pipe-width))) pipes))

;TODO 13
; Vrem să avem un sursa continuă de pipe-uri.
; Implementati funcția add-more-pipes, care va primi drept parametru mulțimea pipe-urilor
; din stare și, dacă avem mai puțin de no-pipes pipe-uri, mai adăugăm una la mulțime,
; având x-ul egal cu pipe-width + pipe-gap + x-ul celui mai îndepărtat pipe, în raport
; cu pasărea.
(define (add-more-pipes pipes)
  (if (>= (length pipes) no-pipes)
      pipes
      (cons (Pipe (+ (get-pipe-x (car pipes)) pipe-width pipe-gap) (+ added-number (random random-threshold))) pipes)))

;TODO 14
; Vrem ca toate funcțiile implementate anterior legate de pipes să fie apelate
; de către next-state-pipes.
; Aceasta va primi drept parametri mulțimea pipe-urilor și scroll-speed-ul,
; și va apela cele trei funcții implementate anterior, în această ordine:
; move-pipes, urmat de clean-pipes, urmat de add-more pipes.
(define (next-state-pipes pipes scroll-speed)
  (add-more-pipes (clean-pipes (move-pipes pipes scroll-speed))))

;TODO 17
; Creați un getter ce va extrage scorul din starea jocului.
(define (get-score state)
  (State-score state))

;TODO 19
; Vrem să creăm logica coliziunii cu pământul.
; Implementati check-ground-collision, care va primi drept parametru
; o structura de tip pasăre, și returnează true dacă aceasta are coliziune
; cu pământul.
;
; Hint: știm înălțimea păsării, bird-height, și y-ul pământului, ground-y.
; Coliziunea ar presupune ca un colț inferior al păsării să aibă y-ul
; mai mare sau egal cu cel al pământului.
(define (check-ground-collision bird)
  (if (>= (+ (get-bird-y bird) bird-height) ground-y)
      #t
      #f))

; invalid-state?
; invalid-state? îi va spune lui big-bang dacă starea curentă mai este valida,
; sau nu. Aceasta va fi validă atât timp cât nu avem coliziuni cu pământul
; sau cu pipes.
; Aceasta va primi ca parametru starea jocului.

;TODO 20
; Vrem să integrăm verificarea coliziunii cu pământul în invalid-state?.

;TODO 22
; Odată creată logică coliziunilor dintre pasăre și pipes, vrem să integrăm
; funcția nou implementată în invalid-state?.
(define (invalid-state? state)
  (cond [(check-ground-collision (get-bird state)) #t]
        [(check-pipe-collisions (get-bird state) (get-pipes state)) #t]
        [else #f]))

;TODO 21
; Odată ce am creat pasărea, pipe-urile, scor-ul și coliziunea cu pământul,
; următorul pas este verificarea coliziunii dintre pasăre și pipes.
; Implementati funcția check-pipe-collisions care va primi drept parametri
; o structură de tip pasăre, mulțimea de pipes din stare, și va returna
; true dacă există coliziuni, și false în caz contrar. Reiterând,
; fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap. Pot există
; coliziuni doar între pasăre și cele două părți. Dacă pasărea se află în
; chenarul lipsă, nu există coliziune.
;
; Hint: Vă puteți folosi de check-collision-rectangle, care va primi drept parametri
; colțul din stânga sus și cel din dreapta jos ale celor două dreptunghiuri
; pe care vrem să verificăm coliziunea.
(define (check-pipe-collisions bird pipes)
  (cond [(> (length (filter (λ (pipe) (check-collision-rectangles
                                      (make-posn (get-bird-x bird) (get-bird-y bird))
                                      (make-posn (+ bird-width (get-bird-x bird)) (+ bird-height (get-bird-y bird)))
                                      (make-posn (get-pipe-x pipe) 0)
                                      (make-posn (+ (get-pipe-x pipe) pipe-width) (get-pipe-y pipe)))) pipes)) 0) #t]
        [(> (length (filter (λ (pipe) (check-collision-rectangles
                                      (make-posn (get-bird-x bird) (get-bird-y bird))
                                      (make-posn (+ bird-width (get-bird-x bird)) (+ bird-height (get-bird-y bird)))
                                      (make-posn (get-pipe-x pipe) (+ pipe-self-gap (get-pipe-y pipe)))
                                      (make-posn (+ (get-pipe-x pipe) pipe-width) pipe-height))) pipes)) 0) #t]
        [else #f]))

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

;TODO 5
; Trebuie să integrăm funcția implementată anterior, și anume next-state-bird,
; în next-state.

;TODO 15
; Vrem să implementăm logică legată de mișcarea, ștergerea și adăugarea pipe-urilor
; în next-state. Acesta va apela next-state-pipes pe pipe-urile din starea curentă.

;TODO 18
; Vrem ca next-state să incrementeze scorul cu 0.1 la fiecare cadru.
(define (next-state state)
  (match-let ([(State bird pipes score variables abilities) state])
    (struct-copy State state [bird (next-state-bird bird (get-gravity variables))]
                             [pipes (next-state-pipes pipes (get-scroll variables))]
                             [score (+ score 0.1)]
                             [variables (next-variables init-variables (get-abilities state))]
                             [abilities (next-abilities (get-abilities state) (get-bird state) (get-scroll (get-variables state)))]
                             )))

; draw-frame
; draw-frame va fi apelat de big-bang dupa fiecare apel la next-state, pentru a afisa cadrul curent.
;TODO 23
; Fiecare cadru va fi desenat in urmatorul mod:
; bird peste ground, peste scor, peste pipes, peste empty-scene.
;
; Hint: score-to-image primeste un numar real si intoarce scor-ul sub forma de imagine;
; Scor-ul îl puteți plasa direct la coordonatele date, fără a mai face translatiile menționate mai jos.
; Noi tinem minte coltul din stanga sus al imaginii, insa, la suprapunerea unei imagini A peste o alta imagine,
; coordonatele unde plasam imaginea A reprezinta centrul acesteia. Trebuie facute translatiile de la coltul din stanga
; sus la centrul imaginilor.
; Variabile folosite in aceasta functie:
; bird -> bird-width si bird-height
; ground -> ground-y si ground-height, acesta va acoperi intreaga latime a ecranului
; scor -> text-x si text-y
; pipes -> pipe-width si pipe-height
(define pipe-image (rectangle pipe-width pipe-height "solid" "green"))
(define bird-image (rectangle bird-width bird-height  "solid" "yellow"))
(define ground-image (rectangle scene-width ground-height "solid" "brown"))
(define initial-scene (rectangle scene-width scene-height "solid" "white"))

(define text-family (list "Gill Sans" 'swiss 'normal 'bold #f))
(define (score-to-image x)
(if SHOW_SCORE
	(apply text/font (~v (round x)) 24 "indigo" text-family)
	empty-image))

(define (draw-frame state)
        (place-active-abilities (get-abilities-active (get-abilities state))
               (place-image bird-image (+ (get-bird-x (get-bird state)) (quotient bird-width 2)) (+ (get-bird-y (get-bird state)) (quotient bird-height 2))
                            (place-image ground-image (quotient scene-width 2) (+ ground-y (quotient ground-height 2))
                                         (place-image (score-to-image (get-score state)) text-x text-y
                                                      (place-pipes (get-pipes state)
                                                                   (place-visible-abilities (get-abilities-visible (get-abilities state))
                                                                   initial-scene)))))))

; Folosind `place-image/place-images` va poziționa pipe-urile pe scenă.

(define (place-pipes pipes scene)
  (foldr (λ (pipe scene) (place-image pipe-image (+ (get-pipe-x pipe) (quotient pipe-width 2)) (- (get-pipe-y pipe) (quotient pipe-height 2))
                              (place-image pipe-image (+ (get-pipe-x pipe) (quotient pipe-width 2)) (+ (get-pipe-y pipe) pipe-self-gap (quotient pipe-height 2)) scene))) scene pipes))
                                                                                                     
; Bonus
; Completați abilities.rkt mai întâi, aceste funcții căt, apoi legați
; această funcționalitate la jocul inițial.


; Abilitatea care va încetini timpul va dura 10 de secunde, va avea imaginea (hourglass "mediumseagreen")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = max(5, scroll-speed - 1)
(define slow-ability (Ability
                      (hourglass "mediumseagreen")
                      10
                      null
                      (λ (variables) (struct-copy Variables variables [scroll (max 5 (get-scroll variables))]))))

; Abilitatea care va accelera timpul va dura 30 de secunde, va avea imaginea (hourglass "tomato")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = scroll-speed + 1
(define fast-ability (Ability
                      (hourglass "tomato")
                      30
                      null
                      (λ (variables) (struct-copy Variables variables [scroll (+ (get-scroll variables) 1)]))))

; lista cu toate abilităţile posibile în joc
(define ABILITIES (list fast-ability slow-ability))


(define get-variables (lambda (state) (State-variables state)))
(define get-variables-gravity (lambda (state) (Variables-gravity (get-variables state))))
(define get-variables-momentum (lambda (state) (Variables-momentum (get-variables state))))
(define get-variables-scroll-speed 'your-code-here)

; Întoarce abilităţile din stare, cu o reprezentare
; intermediară care trebuie să conțină două liste:
;  - lista abilităţilor vizibile (încarcate în scenă dar nu neaparat vizibile pe ecran).
;  - lista abilităţilor activate (cu care pasărea a avut o coloziune).
(define (get-abilities x) (State-abilities x))

; Întoarce abilităţile vizibile din reprezentarea intermediară.
(define get-abilities-visible (λ (abilities) (Abilities-visible abilities)))

; Întoarce abilităţile active din reprezentarea intermediară.
(define get-abilities-active (λ (abilities) (Abilities-active abilities)))

(define get-ability-pos (λ (ability) (Ability-pos ability)))
(define get-ability-time (λ (ability) (Ability-time ability))) 

; Șterge din reprezentarea abilităţilor vizibile pe cele care nu mai sunt vizibile.
; echivalent cu clean-pipes.
(define (clean-abilities abilities)
  (filter (lambda (ability) (> (+ (posn-x (Ability-pos ability)) (quotient (image-width (get-ability-image ability)) 2)) 0)) abilities))


; Muta abilităţile vizibile spre stanga.
; echivalent cu move-pipes.
(define (move-abilities abilities scroll-speed)
  (map (lambda (ability) (struct-copy Ability ability [pos (make-posn (- (posn-x (get-ability-pos ability)) scroll-speed)
                                                                      (posn-y (get-ability-pos ability)))])) abilities))


; Scurge timpul pentru abilităţile activate și le sterge pe cele care au expirat.
; Puteți să va folosiți de variabila globală fps.
(define (time-counter abilities)
  (filter (λ (ability) (> (get-ability-time ability) 0))
    (map (λ (ability) (struct-copy Ability ability [time (- (get-ability-time ability) 0.04)])) abilities)))
;AM MODIFICAT SA I DEA EL ALEA ACTIVE DOAR
; Generează următoarele abilitați vizibile.
; *Atentie* La orice moment pe scena trebuie să fie exact DISPLAYED_ABILITIES
; abilităţi vizibile
; Folosiți funcția fill-abilities din abilities.rkt cât si cele scrise mai sus:
; move-abilities, clean-abilities, time-counter, etc..
(define (next-abilities-visible visible scroll-speed)
  (fill-abilities (clean-abilities (move-abilities visible scroll-speed)) DISPLAYED_ABILITIES ABILITIES))

; Generează structura intermediară cu abilități.
; Observați ca nu există next-abilities-active aceastea sunt acele abilităti
; întoarse next-abilities-visible care au o coliziune cu pasărea.
; Puteti folosi `filer`/`filter-not` ca sa verificați ce abilităti au și abilitați
; nu au coliziuni cu pasărea sau puteti folosi `partition`
(define (check-ability-collision bird)
  (λ (ability) (check-collision-rectangles
                (make-posn (get-bird-x bird) (get-bird-y bird))
                (make-posn (+ bird-width (get-bird-x bird)) (+ bird-height (get-bird-y bird)))
                (make-posn (- (posn-x (get-ability-pos ability)) (quotient (image-width (get-ability-image ability)) 2))
                           (- (posn-y (get-ability-pos ability)) (quotient (image-height (get-ability-image ability)) 2)))
                (make-posn (+ (posn-x (get-ability-pos ability)) (quotient (image-width (get-ability-image ability)) 2))
                           (+ (posn-y (get-ability-pos ability)) (quotient (image-height (get-ability-image ability)) 2))))))
                 
(define (next-abilities abilities bird scroll-speed)
      (struct-copy Abilities abilities
                   [visible (filter-not (check-ability-collision bird) (next-abilities-visible (get-abilities-visible abilities) scroll-speed))]
                   [active (append (time-counter (get-abilities-active abilities)) (filter (check-ability-collision bird) (get-abilities-visible abilities)))]
                   ))

; Dând-use variabilele actuale și abilitațile calculați care vor
; variabile finale folosite în joc
; Folositi compose-abilities
; Atenție când apelați `next-variables` în next-state dați ca paremetru
; initial-variables și nu variabilele aflate deja în stare
; In felul acesta atunci când
(define (next-variables variables abilities)
  ((compose-abilities (get-abilities-active abilities)) variables))

; Folosind `place-image/place-images` va poziționa abilităţile vizibile la ability pos.
(define (place-visible-abilities abilities scene)
  (foldr (λ (ability scene) (place-image (get-ability-image ability)
                                        (posn-x (get-ability-pos ability))
                                        (posn-y (get-ability-pos ability))
                                        scene)) scene abilities))

; Folosind `place-image/place-images` va poziționa abilităţile active
; în partea de sus a ecranului lângă scor.
; Imaginiile vor scalate cu un factor de 0.75 și așezate plecând
; de la abilitIES-posn (constantă globală) cu spații de 50 de px.
; Imaginea cu indexul i va fi așezată la (abilitIES-posn.x - 50*i, abilitIES-posn.y)

(define (interval a b step)
  (let iter ((b b) (result '()))             
    (if (> a b)                              
        result
        (iter (- b step) (cons b result)))))

(define (place-active-abilities abilities scene)
  (let* ((counter (interval 0 (- (length abilities) 1) 1)))
   (foldr (λ (count ability scene) (place-image (scale 0.75 (get-ability-image ability))
                                        (- (posn-x abilities-posn) (* 50 count))
                                        (posn-y abilities-posn)
                                        scene)) scene counter abilities)))


(module+ main
	(big-bang (get-initial-state)
	 [on-tick next-state (/ 1.0 fps)]
	 [to-draw draw-frame]
	 [on-key change]
	 [stop-when invalid-state?]
	 [close-on-stop #t]
	 [record? #f]))
