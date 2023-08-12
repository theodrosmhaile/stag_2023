#!/usr/bin/env python
# coding: utf-8


import random as rnd
import numpy as np
import os
import sys
import string
import actr
import pandas as pd
import seaborn as sns
from matplotlib import pyplot
import itertools


show_output = True

#Load model

curr_dir = os.path.dirname(os.path.realpath(__file__))
#actr.load_act_r_model(os.path.join(curr_dir, "spacing-effect.lisp"))
actr.load_act_r_model(os.path.join(curr_dir, "Mods_a_la_Teddy.lisp"))
actr.record_history('buffer-trace')
## Daisy chained python functions to present stimuli, get response and  present feedback

def present_stim():
    global chunks
    global stims
    global i
    global show_output

    
    if(show_output):
        		print('Presented item: ', stims[i], 'position: ', in_Position[i] )
    if i < nTrials:


        chunks = actr.define_chunks(['isa', 'stimulus',
         'item', stims[i], 
         'type', of_Type[i], 
         'position', in_Position[i], 
         'kind', 'visual'])

        actr.set_buffer_chunk('visual', chunks[0])

        if (stims[i].__contains__('space')):
        	print('encountered space')
        	#actr.schedule_event_relative(1, 'get_response')
        			

        else:
        	actr.schedule_event_relative(0.9, 'present_stim')
        	

    i = i + 1
  #  print('from present stim: i= ', i)
    

def get_response(model, key ):
    global current_response
    global i
    global strategy_used

    print("Get Response ran")
    actr.schedule_event_relative(0, 'present_stim')
    current_response[i] = key
    print(current_response)
    return current_response
#increase index for next stimulus
    i = i + 1
   # print('from get resp: i= ', i)
    

# This function builds ACT-R representations of the python functions

def model_loop():

    global win
    global accuracy
    global nTrials
    global strategy_used

   # accuracy = np.repeat(0, nTrials).tolist()

    #initial goal dm
    actr.define_chunks(['articulate','isa', 'goal', 'read','no', 'respond', 'no'])
 

    actr.goal_focus('articulate')

    #open window for interaction
    win = actr.open_exp_window("test", visible = False)
    actr.install_device(win)
    actr.schedule_event_relative(0, 'present_stim' )

    #waits for a key press?

    actr.run(100)

actr.add_command('present_stim', present_stim, 'presents stimulus')
#actr.add_command('present_feedback', present_feedback, 'presents feedback')
actr.add_command('get_response', get_response, 'gets response')
actr.monitor_command("output-key", 'get_response')


### Stimuli  -  short test stimuli for a 3-span string, with 2 intervening letters
nTrials = 30

stims =       ["'4'", "'C'", "'A'", "'1'","'H'", "'T'", "'7'","'space1'", 'space2', 'space3']
of_Type =     ['digit', 'letter', 'letter', 'digit','letter', 'letter', 'digit',  'space', 'space','space' ]
in_Position = ['1', 'nil', 'nil','2', 'nil', 'nil', '3' ,1, 2, 3]

### span 5 test
#stims =       ["'4'", "'C'", "'A'", "'1'","'H'", "'T'", "'7'","'V'", "'M'","'L'","'2'","'W'","'T'","'6'", "'space1'", 'space2', 'space3', 'space4', 'space5']
#of_Type =     ['digit', 'letter', 'letter', 'digit','letter', 'letter', 'digit','letter', 'letter','letter','digit', 'letter', 'letter', 'digit',  'space', 'space','space', 'space', 'space']
#in_Position = ['1', 'nil', 'nil','2', 'nil', 'nil', '3', 'nil', 'nil','nil', '4','nil', 'nil','5',1, 2, 3, 4, 5]




i = 0

current_response  = np.repeat('na', 5).tolist() 
