; 323CA Dumitrascu Filip-Teodor
#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
(define (longest-common-prefix w1 w2)
  (lcp '() w1 w2))

(define (lcp acc w1 w2)
  (cond
    ; nu mai sunt elemente in ambele liste sau nu mai au prefix comun
    [(or (null? w1) (null? w2) (not (equal? (car w1) (car w2))))
     (list (reverse acc) w1 w2)]
    ; altfel construiesc prefixul comun
    [else (lcp (cons (car w1) acc) (cdr w1) (cdr w2))]))

; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.

(define (longest-common-prefix-of-list words)
  (define (lcpl words acc)
    (cond
      [(null? words) acc]
      ; calcul de prefix comun intre prefixul de pana acum si cuvantul curent
      [else (lcpl (cdr words) (car (lcp '() acc (car words))))])) 
  
  ; plecam cu primu cuvant ca fiind cel mai lung prefix comun
  ; pentru cazul cand lista contine doar un cuvant
  (lcpl words (car words)))



;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.

(define (match-pattern-with-label st pattern)
  (let*
       ; eticheta curenta
      ([label (get-branch-label (first-branch st))]
       ; ce prefix au in comun eticheta cu patternul
       [common (car (lcp '() label pattern))]
       ; ce a ramas din pattern nepotrivit
       [new-pattern (caddr (lcp '() label pattern))])

    (cond
      ; nu s-a potrivit in nicio eticheta
      [(st-empty? st) (list #f '())]

      ; nu se potriveste in eticheta curenta, continuam cautarea
      [(null? common) (match-pattern-with-label (other-branches st) pattern)]

      ; in intregime in eticheta
      [(equal? common pattern) #t]
      
      ; partial in eticheta, restul de pattern poate fi in subarbore
      [(equal? common label) (list label new-pattern (get-branch-subtree (first-branch st)))]

      ; partial in eticheta iar restul nu mai are potrivire 
      [else (list #f common)])))
 


; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
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
