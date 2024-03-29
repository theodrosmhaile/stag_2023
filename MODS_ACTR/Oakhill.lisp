;;; MISC

(clear-all)

(define-model MODS_model

(sgp
 :esc t ;; formerly :era enable rational analysis
 :ga 1.00
 :bll 0.5
 :ol t
 ;; :sl 0.5 production strength learning parameter is deprecated in v5+
 :ans 0.14 ;; formerly :an, value for activation noise
 ;; :pm t partial matching enable; deprecated
 :mp 3.3
 :rt 0.75
 :dat .180
 ;; :dmt t deprecated
 ;; :mt t deprecated
 :v t
 )

;;; CHUNK TYPES

;; various flag symbols

(Chunk-Type symbol)

;; characters with associated external form

(Chunk-Type character external)

;; positions represent serial ordering, with associated similarities
;; chained both ways to allow attractors at both ends.

(Chunk-Type position previous next)

;; trial index, with next link (circular)

(Chunk-Type trial next)

;; memories store items and their trial and string positions
;; and whether it has been recalled

(Chunk-Type memory item trial position recalled)

;; goals to articulate hold the trial index, the result of visual encoding,
;; an index flag to determine whether it is the first or last of the span,
;; a status flag to indicate whether the articulation has been done,
;; the position of the current span, and the next position to rehearse

(Chunk-Type articulate trial vision flag status position rehearse)

;; goals to recall the span of memories hold the item recalled,
;; its position, a status flag to indicate whether the item has been
;; recalled, and the index of the last one to recall

(Chunk-Type recall trial item position status limit)


;;; CHUNKS

(Add-DM
 ; useful symbols
 ;(last isa symbol) ----- TMH mod
 ;(not isa symbol) ----- TMH mod
 ;(done isa symbol) ----- TMH mod
 ; the 10 digits plus blank
 (n0 isa character external "0")
 (n1 isa character external "1")
 (n2 isa character external "2")
 (n3 isa character external "3")
 (n4 isa character external "4")
 (n5 isa character external "5")
 (n6 isa character external "6")
 (n7 isa character external "7")
 (n8 isa character external "8")
 (n9 isa character external "9")
 (blank isa character external "blank")
 (a isa character external "a")
 (b isa character external "b")
 (c isa character external "c")
 (d isa character external "d")
 (e isa character external "e")
 (f isa character external "f")
 (g isa character external "g")
 (h isa character external "h")
 (i isa character external "i")
 (j isa character external "j")

 ; only 6 positions needed
 (first isa position previous first next second)
 (second isa position previous first next third)
 (third isa position previous second next fourth)
 (fourth isa position previous third next fifth)
 (fifth isa position previous fourth next sixth)
 (sixth isa position previous fifth next dummy)
 ; initial goals to articulate and recall
 (problem isa articulate)

  (solution isa recall)
  )

 

;; with optimized learning and d = 0.5, 5 references and a creation time
;; 100 seconds removed yields an initial base level of 0.0
(set-all-base-levels 100.0 -1000.0)

;; linearly decreasing similarities between positions

(set-similarities
 (first second 0.5)
 (first third 0.25)
 (first fourth 0.125)
 (first fifth 0.0625)
 (first sixth 0.03125)
 (second third 0.5)
 (second fourth 0.25)
 (second fifth 0.125)
 (second sixth 0.0625)
 (third fourth 0.5)
 (third fifth 0.25)
 (third sixth 0.125)
 (fourth fifth 0.5)
 (fourth sixth 0.25)
 (fifth sixth 0.5)
 ;(done not -1.0)   --------TMH mod
)

;;; PRODUCTIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;; Production: read-aloud                                               ;;                                                               ;;
;; Does: read the digit aloud by retrieving its external form           ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p read-aloud
   =goal>
      isa articulate
      vision =char
      status nil
;; =char>
;; isa character 
;; external =string      

    =visual>
      ;;isa character
      external =string
==>
  

   !output! ("Saying ~A" =string)
   =goal>
      status done
      )

; the reading + articulating latency is set at 200 milliseconds
;; (parameters read-aloud :dat .200)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;; Production: create-memory                                            ;;                                                               ;;
;; Does: if the character is the last, then create a memory for it      ;;
;;       stamped with the current trial and string position, which is   ;;
;;       incremented                                                    ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p create-memory
   =goal>
      isa articulate
      trial =trial
      vision =char
      flag last
      status done
      position =serial_position

   =serial_position>
     isa position
      next =next
==>
 ;  !output! ("Memorizing ~A in position ~A incremented to ~A"
   ;          =char =position =next)
;   =memory>
 ;     isa memory
  ;    item =char
    ;  trial =trial
    ;  position =serial_position
    ;  recalled not
   =goal>
      vision nil
      ;position =next
      rehearse first)

;;(parameters create-memory :dat 0.100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;; Production: rehearse-memory                                          ;;                                                               ;;
;; Does: when there is nothing else to do, rehearse memories backwards  ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p rehearse-memory
   =goal>
      isa articulate
      trial =trial
      flag last
      status done
      rehearse =position
   =memory>
      isa memory
      trial =trial
      item =char
      position =position
      recalled not
   =position>
      isa position
      next =next
==>
   !output! ("Rehearsing ~A in ~A" =char =position)
   !eval! (incf *rehearsals*)
   =goal>
      rehearse =next)

;;(parameters rehearse-memory :dat 0.150)

;; RECALL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Production: parse-screen
;; Does: simulates the subject reading the recall prompt
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p parse-screen
   =goal>
      isa recall
      trial =trial
      item nil
      position nil
==>
   !output! ("Encoding recall instructions")
   =goal>
      position first
      )

;;(parameters parse-screen :effort 1.565)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Production: recall-span
;; Does: recall a span element based on its position disable the
;;       recall of the same element twice in the same span
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p recall-span
   =goal>
      isa recall
      trial =trial
      item nil
      position =position
   =memory>
      isa memory
      item =item
      trial =trial
      position =position
      recalled not
==>
   !output! ("Recalling memory position ~A" =position)
   !eval! (push-last =item *answers*)
   =memory>
      recalled done
   =goal>
      item =item)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Production: no-recall
;; Does: when no recall, just retrieve blank to allow further retrievals
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p no-recall
   =goal>
      isa recall
      item nil
      position =position
   =blank>
      isa character
      external "blank"
==>
   !output! ("Skip memory index ~A" =position)
   !eval! (push-last =blank *answers*)
   =goal>
      item =blank
      )

; downgrade the value of this
;;(parameters no-recall :r 0.5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Production: read-item
;; Does: says a recalled item aloud
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p read-item
   =goal>
      isa recall
      item =char
      status nil
   =char>
      isa character
      external =string
==>
   !output! ("Saying ~A" =string)
   =goal>
      status done
      )

; the articulating latency is set at 150 milliseconds
;;(parameters read-item :effort .150)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Production: next-item
;; Does: increments the position to be recalled
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p next-item
   =goal>
      isa recall
      position =position
      status done
    - limit =position
   =position>
      isa position
      next =next
==>
   !output! ("Moving to index ~A" =next)
   =goal>
      item nil
      position =next
      status nil
      )

) ;; wns define-model
