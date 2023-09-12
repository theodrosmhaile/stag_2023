#!/usr/bin/env python
# coding: utf-8


import random as rand
import numpy as np
import os
import sys
import string
import actr
import pandas as pd


import itertools


debug = False
show_output = False

#Load model

curr_dir = os.path.dirname(os.path.realpath(__file__))

#actr.load_act_r_model(os.path.join(curr_dir, "Mods_a_la_Andy.lisp"))
actr.load_act_r_model(os.path.join(curr_dir, "Mods_a_la_Andy.lisp"))#tmh 08/24
actr.load_act_r_code(os.path.join(curr_dir, "Mods_a_la_Andy_Rehearse.lisp"))

#actr.record_history('buffer-trace')
## Daisy chained python functions to present stimuli, get response and  present feedback

def present_stim():
    global chunks
    global stims
    global i
    global show_output

    
    if(show_output):
        		print('Presented item: ', stims[i], 'position: ', in_Position[i] )
    
    if i <= nTrials-1:

        chunks = actr.define_chunks(['isa', 'stimulus',
         'item', stims[i], 
         'type', of_Type[i], 
         'position', in_Position[i], 
          'pos-word', Pos_word[i],                           
         'kind', 'visual'])

        actr.set_buffer_chunk('visual', chunks[0])
        
        if (stims[i].__contains__('space')):
            
            if show_output:
                print('encountered space')
                actr.schedule_event_relative(0, 'present_stim')
        			

        else:
        	actr.schedule_event_relative(0.9, 'present_stim')
       

    i = i + 1
  #  print('from present stim: i= ', i)
    

def get_response(model, key ):
    global current_response
    global i
    global strategy_used
    global cr

    if show_output:
        print("Get Response ran")
    #actr.schedule_event_relative(0, 'present_stim')
    #print(cr)
    current_response[cr] = key
    if show_output:
        print(current_response)
    cr = cr + 1

    if cr == span:
        last_stim = actr.define_chunks([ 'isa', 'stimulus',
            'item', 'stop',
            'type', 'stop']
            )
        actr.set_buffer_chunk('visual', last_stim[0])


    return current_response
#increase index for next stimulus
    
    
   # print('from get resp: i= ', i)
    

# This function builds ACT-R representations of the python functions

def model_loop():

    global win
    global accuracy
    global nTrials
    global strategy_used
    global cr
    global span
   # accuracy = np.repeat(0, nTrials).tolist()
    cr = 0
    #initial goal dm
    actr.define_chunks(['articulate','isa', 'goal', 'read','no', 'respond', 'nil']) #tmh 08/24
 

    actr.goal_focus('articulate')

    #open window for interaction
    win = actr.open_exp_window("test", visible = False)
    actr.install_device(win)
    actr.schedule_event_relative(0, 'present_stim' )

    #waits for a key press?

    actr.run(40)

actr.add_command('present_stim', present_stim, 'presents stimulus')
#actr.add_command('present_feedback', present_feedback, 'presents feedback')
actr.add_command('get_response', get_response, 'gets response')
actr.monitor_command("output-key", 'get_response')


   
if debug:
    ### Stimuli  -  short test stimuli for a 3-span string, with 2 intervening letters
    i = 0
   

    stims =       ["'4'", "'C'", "'A'", "'1'","'H'", "'T'", "'7'","'E'","'R'","'2'","'D'","'M'","'3'","'L'","'O'","'8'", 'space']#, 'space2', 'space3', 'space4', 'space5', 'space6']
    of_Type =     ['digit', 'letter', 'letter', 'digit','letter', 'letter', 'digit','letter', 'letter','digit','letter', 'letter', 'digit','letter', 'letter', 'digit',  'space']#, 'space','space', 'space', 'space','space' ]
    in_Position = ['1', 'nil', 'nil','2', 'nil', 'nil', '3' ,'nil', 'nil','4','nil', 'nil','5','nil', 'nil','6',1]#, 2, 3,4,5,6]
    Pos_word =    ['one', 'nil', 'nil', 'two', 'nil', 'nil', 'three','nil', 'nil', 'four','nil', 'nil','five','nil', 'nil','six','six']#,'five', 'four', 'three','two', 'one' ]
    current_response  = np.repeat(-1, 6).tolist() 
    nTrials = len(stims)
    span = 6


def simulation(nSims, expt, mas):

    global nTrials
    global resps 
    global resps_test
    global i
    global current_response
    global cr
    global span
    global corr_responses
    global stims
    global of_Type
    global in_Position
    global Pos_word
   
    
    span = expt['span_size']
    
#----- set up experiment - initialize variables, these don't change for a set of sims
    
    resps = pd.DataFrame({0:np.repeat('NaN',6)}) #6 is the max number of span items/responses
    resps_test =[]
    type_temp     = np.repeat('digit', span)
    position_temp = ['1','2', '3', '4', '5', '6']
    letters='A', 'B', 'C'
    space_temp = ['space1', 'space2','space3','space4','space5','space6'] 
    pos_word_temp = ['one', 'two', 'three', 'four', 'five', 'six']
    
    for s in range(0, nSims):
#----- set up experiment - initialize variables, these change for a set of sims        
        this_type     = ()
        this_stim     = ()
        this_position = ()
        this_pos_word = ()
        
        win = None
        i = 0

#----- set up experiment - generate stimuli sequence with variable distractor length
    
        for ss in range(0, span):
       
            this_stim =  np.append(this_stim, expt['corr_responses'][ss])
            this_type = np.append(this_type, type_temp[ss])
            this_position = np.append(this_position,  position_temp[ss])
            this_pos_word = np.append(this_pos_word,  pos_word_temp[ss])
            if ss < span-1:
                
                this_len = rand.sample([2,3], 1)[0]
                this_stim     = np.append(this_stim, letters[0:this_len])
                this_type     = np.append(this_type, np.repeat('letter', this_len))
                this_position = np.append(this_position, np.repeat('nil', this_len))
                this_pos_word = np.append(this_pos_word,  np.repeat('nil', this_len))
                
        this_stim =     np.append(this_stim,'space')
        this_type =     np.append(this_type, 'space')
        this_position = np.append(this_position,span)
        this_pos_word = np.append(this_pos_word, 'space')
#----- set up experiment - setup stimuli and properties for presntation to model

        nTrials = len(this_stim)

        span_size = span
        corr_responses = expt['corr_responses']
        stims =          np.array(this_stim).tolist()
        of_Type =        np.array(this_type).tolist()
        in_Position =    np.array(this_position).tolist()
        Pos_word =        np.array(this_pos_word).tolist()
        current_response  = np.repeat(-1, span).tolist()  
        
#----- run experiment 

        actr.set_parameter_value(":mas", mas)
    
        model_loop()
        if show_output==False:
            actr.reset()
        
#----- compute accuracy 
    
        
       # resps.loc[0:(span-1), s] = np.array(current_response) == np.array(corr_responses)
        resps_test.append([np.array(current_response) == np.array(corr_responses)])
    return resps_test 







