; 323CA Dumitrascu Filip-Teodor
#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  (lcp '() w1 w2))

(define (lcp acc w1 w2)
  (cond
    ; nu mai sunt elemente in ambele liste sau nu mai au prefix comun
    [(or (null? w1) (null? w2) (not (equal? (car w1) (car w2))))
     (list (reverse acc) w1 w2)]
    ; altfel construiesc prefixul comun
    [else (lcp (cons (car w1) acc) (cdr w1) (cdr w2))]))


(define (longest-common-prefix-of-collection words)
  (define (lcpl words acc)
    (cond
      [(collection-empty? words) acc]
      ; calcul de prefix comun intre prefixul de pana acum si cuvantul curent
      [else (lcpl (collection-rest words) (collection-first (lcp '() acc (collection-first words))))])) 

  ; plecam cu primu cuvant ca fiind cel mai lung prefix comun
  ; pentru cazul cand lista contine doar un cuvant
  (lcpl words (collection-first words)))  


(define (st-has-pattern? st pattern)
  (let*
       ; eticheta curenta
      ([label (get-branch-label (first-branch st))]
       ; ce prefix au in comun eticheta cu patternul
       [common (car (lcp '() label pattern))]
       ; ce a ramas din pattern nepotrivit
       [new-pattern (caddr (lcp '() label pattern))])

    (cond
      ; patternul nu este in niciun branch
      [(st-empty? st) #f]

      ; patternul nu este in branchul curent
      [(null? common) (st-has-pattern? (other-branches st) pattern)]

      ; patternul este in eticheta curenta
      [(equal? common pattern) #t]

      ; patternul se poate termina in subarbore
      [else (st-has-pattern? (get-branch-subtree (first-branch st)) new-pattern)])))


(define (get-suffixes text)
  (cond
    [(null? text) '()]
    ; se formeaza colectia cu sufixe dupa aplicatia recursiva
    [else (collection-cons text (get-suffixes (cdr text)))]))


(define (get-ch-words words ch)
  ; dupa eliminarea cuv vide se elimina si cele care nu incep cu ch 
  (stream-filter (λ (x) (equal? ch (car x)))
          (stream-filter (λ (x) (not (null? x))) words)))

(define (ast-func suffixes)
  ; se construieste perechea cu eticheta si sufixele modificate
  (cons (list (car (collection-first suffixes)))
        (collection-map (λ (x) (cdr x)) suffixes)))


(define (cst-func suffixes)
  (let ([common (longest-common-prefix-of-collection suffixes)])
    ; se construieste perechea cu eticheta si sufixele modificate
    (cons common
          (collection-map (λ (x) (cadr (longest-common-prefix x common))) suffixes))))


(define (suffixes->st labeling-func suffixes alphabet)
  ; modific charurile din colectia alfabet in liste cu sufixe
  ; care incep cu charul modificat
  (define (group-by-initial suffixes alphabet)     
    (collection-map (λ (x) (get-ch-words suffixes x)) alphabet))

  ; se formeaza perechea (eticheta, noi sufixe) in fiecare lista cu sufixe
  ; listele vide sunt eliminate (sufixul nu a inceput cu caracterul din alfabet)
  (define (label-and-subtree labeling-func grouped-suffixes)     
    (collection-map (λ (x) (labeling-func x))                                        
       (collection-filter (λ (x) (not (collection-empty? x))) grouped-suffixes)))

  ; se pastreaza eticheta si se continua pe fiecare branch
  ; pana la formarea subtreeurilor
  (collection-map (λ (x)
    (cons (car x)                                                                  
       (suffixes->st labeling-func (cdr x) alphabet)))                            
    (label-and-subtree labeling-func (group-by-initial suffixes alphabet))))


(define text->st
  (λ (text)
    (λ (labeling-func)
      (let ([suffixes (get-suffixes (append text '(#\$)))]
            [alphabet (list->collection (sort (remove-duplicates (append text '(#\$))) char<?))])
        (suffixes->st labeling-func suffixes alphabet)))))


(define text->ast
  (λ (text)
    ((text->st text) ast-func)))


(define text->cst
  (λ (text)
    ((text->st text) cst-func)))


(define (substring? text pattern)
  (st-has-pattern? (text->ast text) pattern))


(define (repeated-substring-of-given-length text len)
  (define (dfs st acc len)
    (let ([label (get-branch-label (first-branch st))]
          [subtree (get-branch-subtree (first-branch st))])
      (cond
        ; daca s-a terminat subarborele returnam subsirul
        [(st-empty? st) acc]

        ; daca labelu curent e frunza
        [(collection-empty? subtree)
         ; continuam pe alte branchuri ale nodului intern precedent
         (dfs (other-branches st) acc len)]

        ; altfel labelu curent este nod intern si construim subsirul
        [else
         (let ([len-acc (length acc)]
               [len-label (length label)])
           (cond
             ; daca deja are lungimea dorita ne intoarcem din aplicatia recursiva
             [(= len-acc len) acc]

             ; arborele fiind compact, daca doar o parte din label ajunge pentru
             ; formarea subsirului construim cu aceasta
             [(>= (+ len-acc len-label) len)
              (dfs subtree (append acc (take label (- len len-acc))) len)]

             ; altfel lungimea labelului nu ajunge pentru ca subsirul sa aiba
             ; lungimea dorita asa ca continuam recursiv pe subarbore, iar daca
             ; nu gasim subsirul in subarbore, pe alte branchuri ale nodului intern
             [else
              (let ([search (dfs subtree (append acc label) len)])
                (if (= (length search) len)
                       search
                       (dfs (other-branches st) acc len)))]))])))


  ; astfel dfs imi returneaza subsirul cu lungimea maxima
  ; mai mica sau egala cu len, in final verificandu-se
  ; daca are lungimea len sau alta lungime
  (let ((acc (dfs (text->cst text) '() len)))
    (if (= (length acc) len)
        acc
        #f)))
