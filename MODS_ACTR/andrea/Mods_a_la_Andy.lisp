;;------------------------------------
;; MODS a la Andy
;;------------------------------------
;;
;; This is a re-implementation of the Daily et al. MODS ACT-R model.

;;------ CHANGE-LOG
;;------- 08/27/2023 -TMH
;;  - Added a simple function to transform numbers to their word form. 
;;  - Replaced the 6 response productions with a single production that incrementally
;;    responds to each position. 
;;  - Updated check-memory production to hold information about which position to respond to next.
;;        -  check memory also required a new way of binding the word form of the number since this
;;            information is no longer provided through visual buffer, but is needed for Sji computation. 
;;  - changes did not occur here but, rehearsals do not run during the response period. 

(clear-all)

;;(setf *numbers* '((1 . one) (2 . two) (3 . three) (4 . four) (5 . five) (6 . six)))

(defun return-word (x)
  (setq y
        (case x
          (1 'one)
          (2 'two)
          (3 'three)
          (4 'four)
          (5 'five)
          (6 'six)
          (otherwise nil))))


(define-model MODS_ACTR


(sgp
 :esc t
 :ga 1.00
 :bll 0.5
 :ol t
 :ans 0.14
 :mp nil
 :dat .180
 :v nil
 :declarative-finst-span 0.5
 ;:act t
 :rt -0.81 ;-0.81 ;-10 ;-10
 ;:mas 2
 :visual-activation 1
 )

;;---------------------------------------------
;;----------------Chunk types------------------
(chunk-type goal
            articulate
            read
            respond
            rehearsing
            )

(chunk-type stimulus
            item
            type
            position
            pos-word
            kind)

(chunk-type wmobj
            kind
            item
            position
            pos-word)


;(add-dm (womobj1 isa wmobj kind wm digit 4 position 1))

;;-------------------------------------------
;; This production just reads out all characters except for blank spaces, changes
;; goal for 'read' activity to yes and preserves items in visual buffer.

(p read-aloud
    =goal>
    ;isa articulate
    read no
    respond nil ;;tmh 08/24  was respond no

    =visual>
    item =ThisItem
    - type space

    ?vocal>
    preparation free
    processor free
    execution free
    state free

    ==>
   +vocal>
   cmd subvocalize
   string =ThisItem

    *goal>
    read yes


    =visual>
    )



;;-------------------------------------------
;; If a character has been read aloud, but a response is not required, this
;; production resets the goal to read the next character.


(p read-next
    =goal>
    read yes
    respond nil ;;tmh 08/24  was respond no


    ?visual>
    - buffer empty

    =visual>
    type letter

    ==>

 *goal>
   read no

    )

;;-------------------------------------------
;;  encode working memory: this production creates new imaginal chunks that are of
;; type WM.

(p encode-wm
  =goal>
     read yes
  =visual>
     type digit
     item =D
     position =P
     pos-word =PW
==>
  !bind! =NEXT (1+ =P)
  ;!bind! =POS-WORD (cdr (assoc =P *numbers*))
    +imaginal>
      isa wmobj
      kind wm
      item =D
      position =P
      pos-word =PW
      next =NEXT

    *goal>
    read yes
    clear_wm yes
 !output! (ndigits =P)
)

;;-------------------------------------------
;; imaginal chunks get written to declarative memory when imaginal buffer is cleared by
;; the free-wm production.

(p free-wm

    =goal>
      clear_wm yes

    =imaginal>
      kind wm
      item =X
      position =N

    ==>

    -imaginal>

    *goal>

    clear_wm no
    ndigits =N
    rehearsing 1
)




;;-------------------------------------------
;; This production fires only when a blank space is displayed, signaling time for a response.
;; It sets the goal to respond and also notes that the space has been read. It also preserves
;; the contents of the visual buffer.

(p parse-screen

    =visual>
    type space
    position =current_span_size

    =goal>
    - respond yes ;; tmh 08/24 was respond no
      resp_position nil



    ==>
  !bind! =testPosition (1+ =current_span_size)
    *goal>
    read yes
    respond no
    resp_position 1
    last_Position  =testPosition

    !output! (limit set to =testPosition using =current_span_size)

    =visual>
    )




;;-------------------------------------------
;; Checks and retrieves memory for stimuli in the current position.
;; Retains contents of visual buffer.

(p check-memory

   =visual>
     type space
    ; position =ThisPosition
     ;pos-word =ThisPosition

   ?visual>
     state free

   ;?imaginal>
    ; state free


   =goal>
     read yes
   - respond yes
   resp_position =SP1
   - last_Position =SP1
   last_Position =LP
   ?retrieval>
     state free
    ;buffer full


  ==>

  !bind! =ThisPosition (return-word =SP1)

  !output! (function modified =SP1 to =ThisPosition)
  !output! (retrieving =SP1 before last item =LP)

   +retrieval>
   isa stimulus
   pos-word  =ThisPosition
   kind wm
   - item nil

   =visual>

   *goal>
     read yes
   respond yes
   resp_position =SP1
   pos-word  =ThisPosition

)



(p make-response

  ?retrieval>
   - state error

   =goal>
   read yes
   respond yes
   resp_position =SP1
   pos-word  =ThisPosition

   =visual>
   type space

   ?manual>
    preparation free
    processor free
    execution free

   =retrieval>
     kind wm
     item =response_key
     position =SP1
     pos-word =ThisPosition

  ==>

+manual>
    cmd press-key
    key =response_key

!bind! =next_resp_position (1+ =SP1)

    *goal>
    read yes
    respond no
   resp_position =next_resp_position
   ; pos-word  =ThisPosition

   =visual>
   type space


  )



;;-------------------------------------------
;; If retrieval fails, make a blank response, in this case just 'x'.
;; It resets the goal to read the next stimulus
;; note: for some reason, sometimes visual buffer isn't cleared and explicitly clearing
;; visual buffer breaks the model....perhaps because of interface failures.

(p respond-blank

    ?retrieval>
    state error

    =goal>
     read yes
     respond yes
     resp_position =SP1


    =visual>
   type space

    ?manual>
    preparation free
    processor free
    execution free

    ==>
!bind! =next_resp_position (1+ =SP1)
    +manual>
    cmd press-key
    key x


  *goal>
    read yes
    respond no
    resp_position =next_resp_position

   =visual>
   type space

       )




)
