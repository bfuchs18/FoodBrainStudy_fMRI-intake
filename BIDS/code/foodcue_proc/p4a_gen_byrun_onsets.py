#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This script was created to generate onset files that censor runs with bad motion based on specified threshold
This script will reference task-foodcue_byrun-censorsummary_fd-XX.tsv, generated by p2_create_censor_files.py

Copyright (C) 2023 Bari Fuchs

     This program is free software: you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation, either version 3 of the License, or
     (at your option) any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program.  If not, see <https://www.gnu.org/licenses/>.
     
This script is not guaranteed to work for new data or under new directory 
configurations, however, it should work if no changes are made to directories
or raw data configurations.

@author: baf44
"""

#set up packages    
import pandas as pd
import os
from pathlib import Path
import re

#########################################################
####                                                 ####
####                  Subfunctions                   ####
####                                                 ####
#########################################################

def _gen_new_onset_file(sub, onsetfile_dat, censor_summary_allPar, p_thresh_block, p_thresh_food ,p_thresh_run):
    """Function to generate onset files that censor runs with excessive motion based on specified threshold
    Inputs:
        sub
        onsetfile_dat
        censor_summary_allPar
        p_thresh_block: threshold if censoring based on cue blocks (i.e., not fixation)
        p_thresh_block: threshold if censoring based on food blocks
        p_thresh_run: thresold if censoring based on all TRs in a run
    Outputs:
    """
    
    # Loop through rows in onset file (row i corresponds to run i+1)
    for i in range(len(onsetfile_dat)):

        # get run number
        runnum = i + 1

        # get % of TRs censored in cue blocks, food blocks, and across the run
        row = censor_summary_allPar[(censor_summary_allPar['sub'] == sub) & (censor_summary_allPar['run'] == int(runnum))] #select row based on sub and runnum
        run_p_censor_blocks = float(row['p_censor_blocks']) # % of TRs censored across all blocks
        run_p_censor_food = float(row['p_censor_food']) # % of TRs censored across food blocks
        run_p_censor = float(row['p_censor']) # % of TRs censored across entire run (includes fixation)

        # if censoring based on blocks
        if (p_thresh_block is not False) and (p_thresh_run is False):

            # if % of TRs censored across all blocks in a run is > threshold
            if run_p_censor_blocks > p_thresh_block:

                # replace column zero, row i with *
                pd.options.mode.chained_assignment = None  # disable SettingWithCopyWarning
                onsetfile_dat[0].iloc[i] = '*' ## gives SettingWithCopyWarning

        # if censoring based on food blocks
        if (p_thresh_food is not False) and (p_thresh_run is False):

            # if % of TRs censored across food blocks in a run is > threshold
            if run_p_censor_food > p_thresh_food:

                # replace column zero, row i with *
                pd.options.mode.chained_assignment = None  # disable SettingWithCopyWarning
                onsetfile_dat[0].iloc[i] = '*' ## gives SettingWithCopyWarning

        # if censoring based on total run only
        if (p_thresh_block is False) and (p_thresh_run is not False):

            # if % of TRs censored in run (row i) is > threshold
            if run_p_censor > p_thresh_run:

                # replace column zero, row i with *
                pd.options.mode.chained_assignment = None  # disable SettingWithCopyWarning
                onsetfile_dat[0].iloc[i] = '*' ## gives SettingWithCopyWarning

        # if censoring based on total run and blocks
        if (p_thresh_block is not False) and (p_thresh_run is not False):

            # if % of TRs censored in across blocks or in a run (row i) is > threshold
            if run_p_censor > p_thresh_run or run_p_censor_blocks > p_thresh_block:

                # replace column zero, row i with *
                pd.options.mode.chained_assignment = None  # disable SettingWithCopyWarning
                onsetfile_dat[0].iloc[i] = '*' ## gives SettingWithCopyWarning

        # if censoring based on total run and food blocks
        if (p_thresh_food is not False) and (p_thresh_run is not False):

            # if % of TRs censored in across blocks or in a run (row i) is > threshold
            if run_p_censor > p_thresh_run or run_p_censor_food > p_thresh_food:

                # replace column zero, row i with *
                pd.options.mode.chained_assignment = None  # disable SettingWithCopyWarning
                onsetfile_dat[0].iloc[i] = '*' ## gives SettingWithCopyWarning


    return(onsetfile_dat)


##############################################################################
####                                                                      ####
####                             Main Function                            ####
####                                                                      ####
##############################################################################

def gen_byrun_onsets(par_id, censorsum_file, p_thresh_run = False, p_thresh_block = False, p_thresh_food = False, preproc_path = False):
    """Function to generate onset files that censor runs with excessive motion based on specified threshold
    Inputs:
        par_id (int): participant ID 
        censorsumfile (string): name of censor summary file (e.g., task-foodcue_byrun-censorsummary_fd-1.0.tsv)
            This file is generated by 
        p_thresh_run (int): threshold for censoring runs based on percent of TRs censored across the whole run
        p_thresh_block (int): threshold for censoring runs based on percent of TRs censored in cue blocks (food and office blocks)
        p_thresh_food (int): threshold for censoring runs based on percent of TRs censored in food blocks
        Path (str) - path to direcory that contains foodcue_onsetfiles/ and fmriprep/ directories.
    Outputs:
        onsetfile_dat: 1 onset dataframe per condition, exported as a csv. Onsets for runs with motion that exceeds
            p_thresh_run or p_thresh_block will replaced with '*'
    """

    # set base_directory
    if preproc_path is False:

        # get script location
        script_path = Path(__file__).parent.resolve()

        # change directory to base directory (BIDSdat) and get path
        os.chdir(script_path)
        os.chdir('../..')
        base_directory = Path(os.getcwd())

        #set specific paths
        bids_onset_path = Path(base_directory).joinpath('derivatives/preprocessed/foodcue_onsetfiles')
        bids_origonset_path = Path(base_directory).joinpath('derivatives/preprocessed/foodcue_onsetfiles/orig')
        bids_fmriprep_path = Path(base_directory).joinpath('derivatives/preprocessed/fmriprep')


    elif isinstance(preproc_path, str):
        # make input string a path
        preprocessed_directory = Path(preproc_path)

        #set specific paths
        bids_onset_path = Path(preprocessed_directory).joinpath('foodcue_onsetfiles')
        bids_origonset_path = Path(preprocessed_directory).joinpath('foodcue_onsetfiles/orig')
        bids_fmriprep_path = Path(preprocessed_directory).joinpath('fmriprep')

    else: 
        print("preproc_path must be string")
        raise Exception()


    # set sub with leading zeross
    sub = str(par_id).zfill(3)

    # Import censor summary database
    censor_summary_path = Path(bids_fmriprep_path).joinpath( str(censorsum_file))

    # extract criteria used to censor TRs based on censor summary database name
    substring = censorsum_file.split("summary_",1)[1]
    TR_cen_critera = substring.split(".tsv",1)[0]

    if censor_summary_path.is_file(): # if database exists

        # import database
        censor_summary_allPar = pd.read_csv(str(censor_summary_path), sep='\t')

        # converting 'sub' to string with leading zeros
        censor_summary_allPar['sub'] = censor_summary_allPar['sub'].apply(lambda x: '{:03d}'.format(x))

        # check that subject ID is in censor_summary_allPar
        if (sub not in set(censor_summary_allPar['sub'])):
            print("sub_" + sub + " has no data in task_byrun-foodcue_censorsummary file")
            raise Exception()
    else:
        print("task_byrun-foodcue_censorsummary does not exist")
        raise Exception()

    #########################################
    #### Generate new onset timing files ####
    #########################################

    # initialize dictionary
    onset_dict = {}

    # Exit if no thresholds are specified
    if p_thresh_run is False and p_thresh_block is False and p_thresh_food is False:
        print("No thresholds specified. Must specify p_thresh_run and/or p_thresh_block/p_thresh_food to run")
        raise Exception()
    
    # Exit if specified p_thresh_block and p_thresh_food
    elif (p_thresh_block is not False) and (p_thresh_food is not False):
        print("Specified p_thresh_block and p_thresh_food. Pick one")
        raise Exception()
    
    # else, continue with function
    else:

        # get original onset files -- sub needs to be padded with leading zeros
        orig_onsetfiles = list(Path(bids_origonset_path).rglob('sub-' + str(sub).zfill(3) + '*AFNIonsets.txt'))

        # Loop through onset files (there is 1 onset file per condition)
        for onsetfile in orig_onsetfiles:

            #get filename
            filename = str(onsetfile).rsplit('/',1)[-1]

            #get conditon
            cond = re.split('_|-',filename)[2]

            #load original file
            onsetfile_dat = pd.read_csv(str(onsetfile), sep = '\t', encoding = 'utf-8-sig', engine='python', header=None)
            
            # generate new onset file -- this updates onsetfile_dat
            _gen_new_onset_file(sub, onsetfile_dat, censor_summary_allPar, p_thresh_block, p_thresh_food, p_thresh_run)

            # for subject 49, exclude run 1 due to triggering issues between scanner and eprime experiment 
            if sub == '049':
                onsetfile_dat[0].iloc[0] = '*'

            # add to dictionary
            onset_dict[cond] = onsetfile_dat

            #######################################
            #### Output new onset timing files ####
            #######################################
            
            # set path to onset directory

            # if thresholding based on blocks only
            if (p_thresh_block is not False) and (p_thresh_run is False):
                new_onset_path = Path(bids_onset_path).joinpath(str(TR_cen_critera) + "_b" + str(p_thresh_block))

            # if thresholding based on food blocks only
            elif (p_thresh_food is not False) and (p_thresh_run is False):
                new_onset_path = Path(bids_onset_path).joinpath(str(TR_cen_critera) + "_f" + str(p_thresh_food))

            # if thresholding based on runs only
            elif (p_thresh_run is not False) and (p_thresh_block is False):
                new_onset_path = Path(bids_onset_path).joinpath(str(TR_cen_critera) + "_r" + str(p_thresh_run))

            # if thresholding based on block and run
            elif (p_thresh_run is not False) and (p_thresh_block is not False):  
                new_onset_path = Path(bids_onset_path).joinpath(str(TR_cen_critera) + "_r" + str(p_thresh_run) + '_b' + str(p_thresh_block))

            # if thresholding based on food block and run
            elif (p_thresh_run is not False) and (p_thresh_food is not False):  
                new_onset_path = Path(bids_onset_path).joinpath(str(TR_cen_critera) + "_r" + str(p_thresh_run) + '_f' + str(p_thresh_food))

            # Check whether the onset directory exists or not
            isExist = os.path.exists(new_onset_path)
            if not isExist:
                # make new path
                os.makedirs(new_onset_path)
            
            # write file
            onsetfile_dat.to_csv(str(Path(new_onset_path).joinpath(filename)), sep = '\t', encoding='ascii', index = False, header=False)

    #return onset dictionary for integration testing
    return onset_dict