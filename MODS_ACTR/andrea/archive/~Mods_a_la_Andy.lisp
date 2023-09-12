;;------------------------------------
;; MODS a la Andy
;;------------------------------------
;;
;; This is a re-implementation of the Daily et al. MODS ACT-R model. 



(clear-all)

;;(setf *numbers* '((1 . one) (2 . two) (3 . three) (4 . four) (5 . five) (6 . six)))

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
 :visual-activation 4
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
            position)


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

    =goal>
    - respond yes ;; tmh 08/24 was respond no

    =visual>
    type space

    ==>
    
    *goal>
    read yes
    respond yes

    =visual>
    )

;;-------------------------------------------
;; Checks and retrieves memory for stimuli in the current position. 
;; Retains contents of visual buffer. 

(p check-memory
   =visual>
     type space
    ; position =ThisPosition
     pos-word =ThisPosition
  
   ?visual>
     state free

   ;?imaginal>
    ; state free


   =goal>
     read yes
   respond yes

   ?retrieval>
     state free
    ;buffer full


  ==>

   +retrieval>
   isa stimulus
   pos-word =ThisPosition
   kind wm
   - item nil

   =visual>
   
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

    =visual>
   type space

    ?manual>
    preparation free
    processor free
    execution free
   
    ==>

    +manual>
    cmd press-key
    key x
 

  *goal>
    read no
    respond no
       
   ;-visual>
       )

;;-------------------------------------------
;; If a retrieval is successful, retrieved item is bound to a variable and 
;; executed via press-key command. There is a production for each serial position. 
;; There will be a total of 6 make-response production

(p make-response-s1
   
  ?retrieval>
   - state error

   =goal>
   read yes
   respond yes

   =visual>
   type space
   position 1
   pos-word one 

   =retrieval>
     kind wm
     item =response_key
     position 1
     pos-word one 
   
   ?manual>
    preparation free
    processor free
    execution free

   ==>

   +manual>
    cmd press-key
    key =response_key

    *goal>
    read no
    respond no

    )


(p make-response-s2
   
   =goal>
   read yes
   respond yes

   =visual>
   type space
   position 2
   pos-word two 

   =retrieval>
   kind wm
   item =response_key
   position 2
   pos-word two
  
   ?manual>
    preparation free
    processor free
    execution free

   ==>

   +manual>
    cmd press-key
    key =response_key

    *goal>
    read no
    respond no

    )

(p make-response-s3
   
   =goal>
   read yes
   respond yes

   =visual>
   type space
   position 3
   pos-word three

   =retrieval>
   kind wm
   item =response_key
   position 3
   pos-word three
   
   ?manual>
    preparation free
    processor free
    execution free

   ==>

   +manual>
    cmd press-key
    key =response_key

    *goal>
    read no
    respond no

    )
    
    (p make-response-s4
   
   =goal>
   read yes
   respond yes

   =visual>
   type space
   position 4
   pos-word four 

   =retrieval>
   kind wm
   item =response_key
   position 4
   pos-word four
   
    ?manual>
    preparation free
    processor free
    execution free

   ==>

   +manual>
    cmd press-key
    key =response_key

    *goal>
    read no
    respond no

    )
    
(p make-response-s5
   
   =goal>
   read yes
   respond yes

   =visual>
   type space
   position 5
   pos-word five

   =retrieval>
   kind wm
   item =response_key
   position 5
   pos-word five ;tmh 08/23
   
   ?manual>
    preparation free
    processor free
    execution free

   ==>

   +manual>
    cmd press-key
    key =response_key

    *goal>
    read no
    respond no 

    )
    
    (p make-response-s6
   
   =goal>
   read yes
   respond yes

   =visual>
   type space
   position 6
   pos-word six


   =retrieval>
   kind wm
   item =response_key
   position 6
   pos-word six

   ?manual>
    preparation free
    processor free
    execution free

   ==>

   +manual>
    cmd press-key
    key =response_key

    *goal>
    read no
    respond no 

    )

)







