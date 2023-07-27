;;------------------------------------
;; MODS a la Teddy
;;------------------------------------
;;
;; This is a re-implementation of the Daily et al. MODS ACT-R model. 



(clear-all)

(define-model MODS_ACTR


(sgp
 :esc t 
 :ga 1.00
 :bll 0.5
 :ol t
 :ans 0.14 
 :mp 3.3
 :rt 0.75
 :dat .180
 :v t
 :declarative-finst-span 0.5
 ;:act t 
 :rt -10 
 )

;;---------------------------------------------
;;----------------Chunk types------------------
(chunk-type goal
            articulate
            read
            respond
            rehearse) ;

(chunk-type stimulus
            item 
            type
            position)

(chunk-type wmobj 
            kind 
            digit
            position)


(add-dm (womobj1 isa wmobj kind wm digit 4 position 1))

;;-------------------------------------------
;; This production just reads out all characters except for blank spaces, changes
;; goal for 'read' activity to yes and preserves items in visual buffer. 

(p read-aloud 
    =goal>
    ;isa articulate
    read no
    respond no

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
    rehearse no

    =visual>
    )



;;-------------------------------------------
;; If a character has been read aloud, but a response is not required, this 
;; production resets the goal to read the next character. 


(p read-next
    =goal>
    read yes
    respond no
    rehearse no

    ?visual>
    - buffer empty

    =visual>
    type letter
    
    ==>

 *goal>
   rehearse yes
 
    )


(p encode-wm 
  =goal>
     ...
  =visual>
     type digit
     letter =D
     position =P
 ...
==>
  !bind! =NEXT (incf =P)
  *goal>
     ndigits =P
     ...
  +imaginal>
     isa wmobj
     kind wm
     digit =D
     position =P
     next =NEXT
)

(p free-wm
...
  =imaginal>
    kind wm
    digit =X
==>
  ...
  -imaginal
)

;;-------------------------------------------
;;------- Block of rehearsal productions
(p start-rehearse
  ...
  =goal>
     ndgits =N
     rehearsing =R
   - rehearsing =N
==>
  +retrieval>
     isa wmobj
     kind wm
     position =R)

(p continue-rehearse
   =goal>
   ...
   =retrieval>
      kind wm
      position =current
      next =next
==>
   =goal>
      rehearsing =next
)

(p reset-rehearse
   =goal>
      ndigits =N
      rehearsing =N
   ==>
      rehearsing 1
)



#|(p rehearse
  =goal>
     rehearse yes

  ?retrieval>
     state free
  ?visual>
     buffer empty
     state free
  
  ?manual>
     preparation free
     execution free
==>
  +retrieval>
    type  digit
:recently-retrieved nil 

!output! ( !!!!!!!!!!!RETRIEVAL!!!!!!!!!!!)
    
   *goal>
    read no
    rehearse no
 )|#



;;-------------------------------------------
;;If a digit is encountered, its identity (what number), and serial position,  
;;is passed on to the imaginal buffer to create a memory trace. 

#|(p create-memory
   
    =goal>
    read yes
    respond no
   
   =visual>
    type digit
    item =ThisItem
    position =ThisPosition

   ?imaginal>
    state free

    ==>

    +imaginal>
    isa stimulus
    item =ThisItem
    position =ThisPosition
    type digit

    +retrieval>
    isa stimulus
    item =ThisItem
    position =ThisPosition
    type digit

    *goal>
    read no
    ) |#

;;-------------------------------------------
;; This production fires only when a blank space is displayed, signaling time for a response. 
;; It sets the goal to respond and also notes that the space has been read. It also preserves 
;; the contents of the visual buffer. 

(p parse-screen

    =goal>
    read no
    respond no

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
     position =ThisPosition
     
  
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
   position =ThisPosition
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


   =retrieval>
   item =response_key
   position 1

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


   =retrieval>
   item =response_key
   position 2

  
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


   =retrieval>
   item =response_key
   position 3


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








