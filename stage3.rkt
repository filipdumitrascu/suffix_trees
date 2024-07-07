; 323CA Dumitrascu Filip-Teodor
#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
    (st-has-pattern? (text->st text) pattern))


; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).
(define (sort-substrings acc)
  (define (len-desc list1 list2)
    ; sorteaza descrescator stringuri dupa lungime
    ; si in caz de egalitate dupa ordinea initiala
    (let ([len1 (length list1)]
          [len2 (length list2)])
      (cond
        [(> len1 len2) #t]       ; #t le interschimba
        [(< len1 len2) #f]       ; #f nu le interschimba
        [else #f])))
  (sort acc len-desc))

(define (longest-common-substring text1 text2)
  ; in functie de returnul lui match-pattern-with-label
  ; se construieste cea mai lunga potrivire cu sufixele din primul text
  (define (match-making st suffix)
    (let [(result (match-pattern-with-label st suffix))]
      (cond
        ; daca exista eticheta cu tot sufixu punem sufixu
        [(equal? result #t) suffix]

        ; daca nu exista eticheta cu tot sufixu punem cat s-a potrivit
        [(equal? (car result) #f) (cadr result)]

        ; altfel eticheta nu ajunge pt sufix si continuam in subarbore
        [else
         (let ([label (car result)]
               [rest-suffix (cadr result)]
               [subtree (caddr result)])
           (append label (match-making subtree rest-suffix)))])))
    

  (let iter ([suffixes2 (get-suffixes text2)] [acc '()])
    (if (null? suffixes2)
        ; daca avem in acc toate potrivirile o alegem pe prima cea mai lunga
        (car (sort-substrings (reverse acc)))

        ; altfel continuam construirea acc ului
        (iter (cdr suffixes2) (cons (match-making (text->cst text1) (car suffixes2)) acc)))))
          

; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.
(define (repeated-substring-of-given-length text len)
  (define (dfs st acc len)
    (let ([label (get-branch-label (first-branch st))]
          [subtree (get-branch-subtree (first-branch st))])
      (cond
        ; daca s-a terminat subarborele returnam subsirul
        [(st-empty? st) acc]

        ; daca labelu curent e frunza
        [(null? subtree)
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
