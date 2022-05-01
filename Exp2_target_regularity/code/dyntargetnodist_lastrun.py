#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2020.2.10),
    on Mon Jun 28 15:53:41 2021
If you publish work using this script the most relevant publication is:

    Peirce J, Gray JR, Simpson S, MacAskill M, Höchenberger R, Sogo H, Kastman E, Lindeløv JK. (2019) 
        PsychoPy2: Experiments in behavior made easy Behav Res 51: 195. 
        https://doi.org/10.3758/s13428-018-01193-y

"""

from __future__ import absolute_import, division

from psychopy import locale_setup
from psychopy import prefs
from psychopy import sound, gui, visual, core, data, event, logging, clock
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)

import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle
import os  # handy system and path functions
import sys  # to get file system encoding

from psychopy.hardware import keyboard



# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)

# Store info about the experiment session
psychopyVersion = '2020.2.10'
expName = 'dyntargetnodist'  # from the Builder filename that created this script
expInfo = {'participant': '', 'group': '', 'screen height (cm)': ''}
dlg = gui.DlgFromDict(dictionary=expInfo, sortKeys=False, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
expInfo['psychopyVersion'] = psychopyVersion

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + u'data/%s_%s_%s' % (expInfo['participant'], expName, expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath='/Users/haileyyu/Desktop/新文件夹/dyntargetnodist_lastrun.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.EXP)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

endExpNow = False  # flag for 'escape' or other condition => quit the exp
frameTolerance = 0.001  # how close to onset before 'same' frame

# Start Code - component code to be run after the window creation

# Setup the Window
win = visual.Window(
    size=[1440, 900], fullscr=True, screen=0, 
    winType='pyglet', allowGUI=False, allowStencil=False,
    monitor='testMonitor', color=[0,0,0], colorSpace='rgb',
    blendMode='avg', useFBO=True, 
    units='height')
# store frame rate of monitor if we can measure it
expInfo['frameRate'] = win.getActualFrameRate()
if expInfo['frameRate'] != None:
    frameDur = 1.0 / round(expInfo['frameRate'])
else:
    frameDur = 1.0 / 60.0  # could not measure, so guess

# create a default keyboard (e.g. to check for escape)
defaultKeyboard = keyboard.Keyboard()

# Initialize components for Routine "instructions"
instructionsClock = core.Clock()
infoText = ('Visual Search\n'
    'In this experiment, you will see a display with many letters.\n'
    'One of the letters will be a "T", this is the target.\n'
    'The task is to find the target letter and determine whether the base of the "T" is pointing to the left or right.\n'
    'If it is pointing to the left please press the "f" key (with your left hand); \n'
    'If it is pointing to the right please press the  "j" key (with your right hand).\n'
    'Please try to respond as quickly and accurately as possible.\n'
    '\n\nWhen you are ready, press space to continue')

ScreenHeight = int(expInfo["screen height (cm)"])

instructions_key = keyboard.Keyboard()
instructions_text = visual.TextStim(win=win, name='instructions_text',
    text='default text',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);

# Initialize components for Routine "fixation"
fixationClock = core.Clock()
fixcross = visual.ShapeStim(
    win=win, name='fixcross', vertices='cross',units='height', 
    size=[1.0, 1.0],
    ori=0, pos=(0, 0),
    lineWidth=1, lineColor=[1,1,1], lineColorSpace='rgb',
    fillColor=[1,1,1], fillColorSpace='rgb',
    opacity=1, depth=0.0, interpolate=True)

# Initialize components for Routine "search"
searchClock = core.Clock()
# Coordinates of circle 
theta=[0, 45, 90, 135, 180, 225, 270, 315]
Cx=[4*cos(-t*pi/180) for t in theta]
Cy=[4*sin(-t*pi/180) for t in theta]

# Letter stimuli
dist_letters = ['F', 'L']
item_names = ['dist' + str(i) for i in [0, 1, 2, 3, 4, 5, 6, 7]]
items = [visual.TextStim(win, units='deg',  pos=(Cx[i], Cy[i]), name = item_names[i]) for i in [0, 1, 2, 3, 4, 5, 6, 7]]

# Target
tar_letters = 'T'
target = visual.TextStim(win, units='deg',  name = 'Target')

key_resp = keyboard.Keyboard()
fixcross_search = visual.ShapeStim(
    win=win, name='fixcross_search', vertices='cross',units='height', 
    size=(0.025, 0.025),
    ori=0, pos=(0, 0),
    lineWidth=1, lineColor=[1,1,1], lineColorSpace='rgb',
    fillColor=[1,1,1], fillColorSpace='rgb',
    opacity=1, depth=-2.0, interpolate=True)

# Initialize components for Routine "feedback"
feedbackClock = core.Clock()
feedback_text = visual.TextStim(win=win, name='feedback_text',
    text='default text',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "block_break"
block_breakClock = core.Clock()
block_trials=84
num_block=10
N_trials=block_trials*num_block
breaktext = visual.TextStim(win, text="Break", units='height', height=0.04)
break_key = keyboard.Keyboard()

# Initialize components for Routine "post_q1"
post_q1Clock = core.Clock()
text = visual.TextStim(win=win, name='text',
    text='Did you notice any pattern to how \nthe position of the target changed\nfrom trial to trial, or do you \nthink the changes were\ncompletely random?\n\n1) The target position changed following a regular pattern from trial to trial. \n2) The target position changed randomly from trial to trial.',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
key_pq1 = keyboard.Keyboard()

# Initialize components for Routine "post_q2"
post_q2Clock = core.Clock()
text_2 = visual.TextStim(win=win, name='text_2',
    text='Which of the following changes of target position\nfrom one trial to the next do you think occurred \nthe most often? \n\n1) Target moved to opposite end of circle\n2) Target moved one step clockwise\n3) Target moved one step counterclockwise\n4) Target moved two steps clockwise\n5) Target moved two steps counterclockwise\n6) Target moved three steps clockwise\n7) Target moved three steps counterclockwise',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
key_pq2 = keyboard.Keyboard()

# Initialize components for Routine "finish"
finishClock = core.Clock()
finish_text = visual.TextStim(win=win, name='finish_text',
    text='The experiment is finished.\nThank you very much!\n',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
finish_key = keyboard.Keyboard()

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.CountdownTimer()  # to track time remaining of each (non-slip) routine 

# ------Prepare to start Routine "instructions"-------
continueRoutine = True
# update component parameters for each repeat
instructions_key.keys = []
instructions_key.rt = []
_instructions_key_allKeys = []
instructions_text.setText(infoText)
# keep track of which components have finished
instructionsComponents = [instructions_key, instructions_text]
for thisComponent in instructionsComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
instructionsClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "instructions"-------
while continueRoutine:
    # get current time
    t = instructionsClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=instructionsClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *instructions_key* updates
    waitOnFlip = False
    if instructions_key.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        instructions_key.frameNStart = frameN  # exact frame index
        instructions_key.tStart = t  # local t and not account for scr refresh
        instructions_key.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(instructions_key, 'tStartRefresh')  # time at next scr refresh
        instructions_key.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(instructions_key.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(instructions_key.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if instructions_key.status == STARTED and not waitOnFlip:
        theseKeys = instructions_key.getKeys(keyList=['f', 'j', 'space'], waitRelease=False)
        _instructions_key_allKeys.extend(theseKeys)
        if len(_instructions_key_allKeys):
            instructions_key.keys = _instructions_key_allKeys[-1].name  # just the last key pressed
            instructions_key.rt = _instructions_key_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # *instructions_text* updates
    if instructions_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        instructions_text.frameNStart = frameN  # exact frame index
        instructions_text.tStart = t  # local t and not account for scr refresh
        instructions_text.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(instructions_text, 'tStartRefresh')  # time at next scr refresh
        instructions_text.setAutoDraw(True)
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in instructionsComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "instructions"-------
for thisComponent in instructionsComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if instructions_key.keys in ['', [], None]:  # No response was made
    instructions_key.keys = None
thisExp.addData('instructions_key.keys',instructions_key.keys)
if instructions_key.keys != None:  # we had a response
    thisExp.addData('instructions_key.rt', instructions_key.rt)
thisExp.addData('instructions_key.started', instructions_key.tStartRefresh)
thisExp.addData('instructions_key.stopped', instructions_key.tStopRefresh)
thisExp.nextEntry()
thisExp.addData('instructions_text.started', instructions_text.tStartRefresh)
thisExp.addData('instructions_text.stopped', instructions_text.tStopRefresh)
# the Routine "instructions" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
trials = data.TrialHandler(nReps=1, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions("sequences/seq_"+expInfo['group']+".csv"),
    seed=None, name='trials')
thisExp.addLoop(trials)  # add the loop to the experiment
thisTrial = trials.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
if thisTrial != None:
    for paramName in thisTrial:
        exec('{} = thisTrial[paramName]'.format(paramName))

for thisTrial in trials:
    currentLoop = trials
    # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
    if thisTrial != None:
        for paramName in thisTrial:
            exec('{} = thisTrial[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "fixation"-------
    continueRoutine = True
    routineTimer.add(1.000000)
    # update component parameters for each repeat
    fixcross.setSize((0.5/ScreenHeight, 0.5/ScreenHeight))
    # keep track of which components have finished
    fixationComponents = [fixcross]
    for thisComponent in fixationComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    fixationClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "fixation"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = fixationClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=fixationClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *fixcross* updates
        if fixcross.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fixcross.frameNStart = frameN  # exact frame index
            fixcross.tStart = t  # local t and not account for scr refresh
            fixcross.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fixcross, 'tStartRefresh')  # time at next scr refresh
            fixcross.setAutoDraw(True)
        if fixcross.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > fixcross.tStartRefresh + 1.0-frameTolerance:
                # keep track of stop time/frame for later
                fixcross.tStop = t  # not accounting for scr refresh
                fixcross.frameNStop = frameN  # exact frame index
                win.timeOnFlip(fixcross, 'tStopRefresh')  # time at next scr refresh
                fixcross.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in fixationComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "fixation"-------
    for thisComponent in fixationComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials.addData('fixcross.started', fixcross.tStartRefresh)
    trials.addData('fixcross.stopped', fixcross.tStopRefresh)
    
    # ------Prepare to start Routine "search"-------
    continueRoutine = True
    # update component parameters for each repeat
    # Reset active nonsingletons
    Distractors=[1, 1, 1, 1, 1, 1, 1, 1]
    DistLetters=np.random.randint(0,2,8)
    
    # Target coordinates
    TarCoords=[4*cos(-theta[tar_pos]*pi/180), 4*sin(-theta[tar_pos]*pi/180)]
    
    # Remove distractor from target position
    Distractors[tar_pos]=0
    
    # Define correct answer
    if (tar_ori==1):
        corr_ans='f'
    else:
        corr_ans='j'
    
    # Distractors
    for i in range(0, len(theta)):
        if Distractors[i]==1:
            items[i].setText(dist_letters[DistLetters[i]])
            items[i].setOri(90+180*np.random.randint(0,2))
            items[i].setAutoDraw(True)
        else:
            items[i].setAutoDraw(False)
    
    # Target 
    target.setText(tar_letters)
    if tar_ori==1:
        target.setOri(90)
    else:
        target.setOri(270)
    target.setPos(TarCoords)
    target.setAutoDraw(True)
    key_resp.keys = []
    key_resp.rt = []
    _key_resp_allKeys = []
    # keep track of which components have finished
    searchComponents = [key_resp, fixcross_search]
    for thisComponent in searchComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    searchClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "search"-------
    while continueRoutine:
        # get current time
        t = searchClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=searchClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *key_resp* updates
        if key_resp.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp.frameNStart = frameN  # exact frame index
            key_resp.tStart = t  # local t and not account for scr refresh
            key_resp.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp, 'tStartRefresh')  # time at next scr refresh
            key_resp.status = STARTED
            # keyboard checking is just starting
            key_resp.clock.reset()  # now t=0
            key_resp.clearEvents(eventType='keyboard')
        if key_resp.status == STARTED:
            theseKeys = key_resp.getKeys(keyList=['f', 'j', 'q'], waitRelease=False)
            _key_resp_allKeys.extend(theseKeys)
            if len(_key_resp_allKeys):
                key_resp.keys = _key_resp_allKeys[-1].name  # just the last key pressed
                key_resp.rt = _key_resp_allKeys[-1].rt
                # was this correct?
                if (key_resp.keys == str(corr_ans)) or (key_resp.keys == corr_ans):
                    key_resp.corr = 1
                else:
                    key_resp.corr = 0
                # a response ends the routine
                continueRoutine = False
        
        # *fixcross_search* updates
        if fixcross_search.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fixcross_search.frameNStart = frameN  # exact frame index
            fixcross_search.tStart = t  # local t and not account for scr refresh
            fixcross_search.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fixcross_search, 'tStartRefresh')  # time at next scr refresh
            fixcross_search.setAutoDraw(True)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in searchComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "search"-------
    for thisComponent in searchComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # Middle circle
    for i in range(0, len(theta)):
       items[i].setAutoDraw(False)
    
    # Target 
    target.setAutoDraw(False)
    
    # check responses
    if key_resp.keys in ['', [], None]:  # No response was made
        key_resp.keys = None
        # was no response the correct answer?!
        if str(corr_ans).lower() == 'none':
           key_resp.corr = 1;  # correct non-response
        else:
           key_resp.corr = 0;  # failed to respond (incorrectly)
    # store data for trials (TrialHandler)
    trials.addData('key_resp.keys',key_resp.keys)
    trials.addData('key_resp.corr', key_resp.corr)
    if key_resp.keys != None:  # we had a response
        trials.addData('key_resp.rt', key_resp.rt)
    trials.addData('key_resp.started', key_resp.tStart)
    trials.addData('key_resp.stopped', key_resp.tStop)
    trials.addData('fixcross_search.started', fixcross_search.tStartRefresh)
    trials.addData('fixcross_search.stopped', fixcross_search.tStopRefresh)
    # the Routine "search" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "feedback"-------
    continueRoutine = True
    routineTimer.add(0.500000)
    # update component parameters for each repeat
    if key_resp.corr:
        feedback_msg = "Correct"
        feedback_col = "darkgreen"
    else:
        feedback_msg = "Incorrect"
        feedback_col = "darkred"
    
    if 'q' in key_resp.keys:
        trials.finished=True
    feedback_text.setColor(feedback_col, colorSpace='rgb')
    feedback_text.setText(feedback_msg)
    # keep track of which components have finished
    feedbackComponents = [feedback_text]
    for thisComponent in feedbackComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    feedbackClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "feedback"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = feedbackClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=feedbackClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *feedback_text* updates
        if feedback_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            feedback_text.frameNStart = frameN  # exact frame index
            feedback_text.tStart = t  # local t and not account for scr refresh
            feedback_text.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(feedback_text, 'tStartRefresh')  # time at next scr refresh
            feedback_text.setAutoDraw(True)
        if feedback_text.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > feedback_text.tStartRefresh + 0.5-frameTolerance:
                # keep track of stop time/frame for later
                feedback_text.tStop = t  # not accounting for scr refresh
                feedback_text.frameNStop = frameN  # exact frame index
                win.timeOnFlip(feedback_text, 'tStopRefresh')  # time at next scr refresh
                feedback_text.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in feedbackComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "feedback"-------
    for thisComponent in feedbackComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials.addData('feedback_text.started', feedback_text.tStartRefresh)
    trials.addData('feedback_text.stopped', feedback_text.tStopRefresh)
    
    # ------Prepare to start Routine "block_break"-------
    continueRoutine = True
    # update component parameters for each repeat
    if ((trials.thisN+1) % block_trials == 0) and (trials.thisN+1 < N_trials):
        this_block=int((trials.thisN+1)/block_trials)
        breaktext.setText('Block ' + str(this_block) + ' finished\n' + str(num_block-this_block) + ' blocks left\n' + 'Press a key to continue')
        breaktext.setAutoDraw(True)
        break_end=False
    else:
        break_end=True
    
    break_key.keys = []
    break_key.rt = []
    _break_key_allKeys = []
    # keep track of which components have finished
    block_breakComponents = [break_key]
    for thisComponent in block_breakComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    block_breakClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "block_break"-------
    while continueRoutine:
        # get current time
        t = block_breakClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=block_breakClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *break_key* updates
        waitOnFlip = False
        if break_key.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            break_key.frameNStart = frameN  # exact frame index
            break_key.tStart = t  # local t and not account for scr refresh
            break_key.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(break_key, 'tStartRefresh')  # time at next scr refresh
            break_key.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(break_key.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(break_key.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if break_key.status == STARTED:
            if bool(break_end):
                # keep track of stop time/frame for later
                break_key.tStop = t  # not accounting for scr refresh
                break_key.frameNStop = frameN  # exact frame index
                win.timeOnFlip(break_key, 'tStopRefresh')  # time at next scr refresh
                break_key.status = FINISHED
        if break_key.status == STARTED and not waitOnFlip:
            theseKeys = break_key.getKeys(keyList=['f', 'j', 'space'], waitRelease=False)
            _break_key_allKeys.extend(theseKeys)
            if len(_break_key_allKeys):
                break_key.keys = _break_key_allKeys[-1].name  # just the last key pressed
                break_key.rt = _break_key_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in block_breakComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "block_break"-------
    for thisComponent in block_breakComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    breaktext.setAutoDraw(False)
    # check responses
    if break_key.keys in ['', [], None]:  # No response was made
        break_key.keys = None
    trials.addData('break_key.keys',break_key.keys)
    if break_key.keys != None:  # we had a response
        trials.addData('break_key.rt', break_key.rt)
    trials.addData('break_key.started', break_key.tStartRefresh)
    trials.addData('break_key.stopped', break_key.tStopRefresh)
    # the Routine "block_break" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'trials'


# ------Prepare to start Routine "post_q1"-------
continueRoutine = True
# update component parameters for each repeat
key_pq1.keys = []
key_pq1.rt = []
_key_pq1_allKeys = []
# keep track of which components have finished
post_q1Components = [text, key_pq1]
for thisComponent in post_q1Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
post_q1Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "post_q1"-------
while continueRoutine:
    # get current time
    t = post_q1Clock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=post_q1Clock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text* updates
    if text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text.frameNStart = frameN  # exact frame index
        text.tStart = t  # local t and not account for scr refresh
        text.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text, 'tStartRefresh')  # time at next scr refresh
        text.setAutoDraw(True)
    
    # *key_pq1* updates
    waitOnFlip = False
    if key_pq1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_pq1.frameNStart = frameN  # exact frame index
        key_pq1.tStart = t  # local t and not account for scr refresh
        key_pq1.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_pq1, 'tStartRefresh')  # time at next scr refresh
        key_pq1.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_pq1.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_pq1.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_pq1.status == STARTED and not waitOnFlip:
        theseKeys = key_pq1.getKeys(keyList=['1', '2'], waitRelease=False)
        _key_pq1_allKeys.extend(theseKeys)
        if len(_key_pq1_allKeys):
            key_pq1.keys = _key_pq1_allKeys[-1].name  # just the last key pressed
            key_pq1.rt = _key_pq1_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in post_q1Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "post_q1"-------
for thisComponent in post_q1Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('text.started', text.tStartRefresh)
thisExp.addData('text.stopped', text.tStopRefresh)
# check responses
if key_pq1.keys in ['', [], None]:  # No response was made
    key_pq1.keys = None
thisExp.addData('key_pq1.keys',key_pq1.keys)
if key_pq1.keys != None:  # we had a response
    thisExp.addData('key_pq1.rt', key_pq1.rt)
thisExp.addData('key_pq1.started', key_pq1.tStartRefresh)
thisExp.addData('key_pq1.stopped', key_pq1.tStopRefresh)
thisExp.nextEntry()
# the Routine "post_q1" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# ------Prepare to start Routine "post_q2"-------
continueRoutine = True
# update component parameters for each repeat
key_pq2.keys = []
key_pq2.rt = []
_key_pq2_allKeys = []
# keep track of which components have finished
post_q2Components = [text_2, key_pq2]
for thisComponent in post_q2Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
post_q2Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "post_q2"-------
while continueRoutine:
    # get current time
    t = post_q2Clock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=post_q2Clock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_2* updates
    if text_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text_2.frameNStart = frameN  # exact frame index
        text_2.tStart = t  # local t and not account for scr refresh
        text_2.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_2, 'tStartRefresh')  # time at next scr refresh
        text_2.setAutoDraw(True)
    
    # *key_pq2* updates
    waitOnFlip = False
    if key_pq2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_pq2.frameNStart = frameN  # exact frame index
        key_pq2.tStart = t  # local t and not account for scr refresh
        key_pq2.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_pq2, 'tStartRefresh')  # time at next scr refresh
        key_pq2.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_pq2.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_pq2.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_pq2.status == STARTED and not waitOnFlip:
        theseKeys = key_pq2.getKeys(keyList=['1', '2', '3', '4', '5', '6', '7'], waitRelease=False)
        _key_pq2_allKeys.extend(theseKeys)
        if len(_key_pq2_allKeys):
            key_pq2.keys = _key_pq2_allKeys[-1].name  # just the last key pressed
            key_pq2.rt = _key_pq2_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in post_q2Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "post_q2"-------
for thisComponent in post_q2Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('text_2.started', text_2.tStartRefresh)
thisExp.addData('text_2.stopped', text_2.tStopRefresh)
# check responses
if key_pq2.keys in ['', [], None]:  # No response was made
    key_pq2.keys = None
thisExp.addData('key_pq2.keys',key_pq2.keys)
if key_pq2.keys != None:  # we had a response
    thisExp.addData('key_pq2.rt', key_pq2.rt)
thisExp.addData('key_pq2.started', key_pq2.tStartRefresh)
thisExp.addData('key_pq2.stopped', key_pq2.tStopRefresh)
thisExp.nextEntry()
# the Routine "post_q2" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# ------Prepare to start Routine "finish"-------
continueRoutine = True
# update component parameters for each repeat
finish_key.keys = []
finish_key.rt = []
_finish_key_allKeys = []
# keep track of which components have finished
finishComponents = [finish_text, finish_key]
for thisComponent in finishComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
finishClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "finish"-------
while continueRoutine:
    # get current time
    t = finishClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=finishClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *finish_text* updates
    if finish_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        finish_text.frameNStart = frameN  # exact frame index
        finish_text.tStart = t  # local t and not account for scr refresh
        finish_text.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(finish_text, 'tStartRefresh')  # time at next scr refresh
        finish_text.setAutoDraw(True)
    
    # *finish_key* updates
    waitOnFlip = False
    if finish_key.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        finish_key.frameNStart = frameN  # exact frame index
        finish_key.tStart = t  # local t and not account for scr refresh
        finish_key.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(finish_key, 'tStartRefresh')  # time at next scr refresh
        finish_key.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(finish_key.clock.reset)  # t=0 on next screen flip
    if finish_key.status == STARTED and not waitOnFlip:
        theseKeys = finish_key.getKeys(keyList=['f', 'j', 'space', 'q'], waitRelease=False)
        _finish_key_allKeys.extend(theseKeys)
        if len(_finish_key_allKeys):
            finish_key.keys = _finish_key_allKeys[-1].name  # just the last key pressed
            finish_key.rt = _finish_key_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in finishComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "finish"-------
for thisComponent in finishComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('finish_text.started', finish_text.tStartRefresh)
thisExp.addData('finish_text.stopped', finish_text.tStopRefresh)
# check responses
if finish_key.keys in ['', [], None]:  # No response was made
    finish_key.keys = None
thisExp.addData('finish_key.keys',finish_key.keys)
if finish_key.keys != None:  # we had a response
    thisExp.addData('finish_key.rt', finish_key.rt)
thisExp.addData('finish_key.started', finish_key.tStartRefresh)
thisExp.addData('finish_key.stopped', finish_key.tStopRefresh)
thisExp.nextEntry()
# the Routine "finish" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# Flip one final time so any remaining win.callOnFlip() 
# and win.timeOnFlip() tasks get executed before quitting
win.flip()

# these shouldn't be strictly necessary (should auto-save)
thisExp.saveAsWideText(filename+'.csv', delim='auto')
thisExp.saveAsPickle(filename)
logging.flush()
# make sure everything is closed down
thisExp.abort()  # or data files will save again on exit
win.close()
core.quit()
