#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
%% De Ridder, Grassi, Morzenti copyright 2025
"""

'''
This code produces the data for Monte-Carlo Estimate
'''

import subprocess
import sys

def install_if_missing(pkg_name, import_as=None):
    try:
        __import__(import_as or pkg_name)
    except ImportError:
        subprocess.check_call([sys.executable, "-m", "pip", "install", pkg_name])

install_if_missing("numpy")
install_if_missing("pandas")
install_if_missing("pyreadstat")


#&& #### Some Import #####
import numpy as np
import pandas as pd
import warnings
warnings.filterwarnings('ignore')

import random #the random

#Get in the right directory
import os
#os.chdir('/home/basilou/Dropbox/Recherche/De Ridder Grassi Morzenti/replication/simulation/simulate')


script_dir = os.path.dirname(os.path.abspath(sys.argv[0]))
# Change working directory to script location
os.chdir(script_dir)



seed = 444
np.random.seed(seed)
random.seed(seed)

##

#%% 
############################
# Some options for this file
############################


#saving the montecarlo results in dta
save_results=True


#adding measurement errors in output
#add measurement errors ?
Err_output=True
#measurement errors factor
err_y = 1/0.905 # divides the variance of the error of sales (if negative applies no error)
R2_y = 0.905 


#Save the data at intermediate step
save_data_intermediate=False



#%% 
############################
# Set-up the Monte Carlo Repetition
############################

#number of repetitions
Nreps = 200               

#%% 
############################
# Loop over some parameters (NB: matlab file must have been created)
############################


#List of end_filename
#end_filename_list =['gamma_sigma1d1_epsi10_alpha0d4_gamma0d7_eta1d1']

end_filename_list = ['baseline_sigma1d1_epsi10_alpha0d4_gamma0d8_eta1d1',
                     'gamma_sigma1d1_epsi10_alpha0d4_gamma0d6_eta1d1',
                     'gamma_sigma1d1_epsi10_alpha0d4_gamma0d7_eta1d1',
                      'gamma_sigma1d1_epsi10_alpha0d4_gamma0d8_eta1d1',
                      'gamma_sigma1d1_epsi10_alpha0d4_gamma0d9_eta1d1',
                      'gamma_sigma1d1_epsi10_alpha0d4_gamma1_eta1d1',
                      'eta_sigma1d1_epsi10_alpha0d4_gamma0d8_eta0d95',
                      'eta_sigma1d1_epsi10_alpha0d4_gamma0d8_eta1',
                      'eta_sigma1d1_epsi10_alpha0d4_gamma0d8_eta1d05',
                      'eta_sigma1d1_epsi10_alpha0d4_gamma0d8_eta1d1',
                      'eta_sigma1d1_epsi10_alpha0d4_gamma0d8_eta1d15',
                      'eta_sigma1d1_epsi10_alpha0d4_gamma0d8_eta1d2',
                      'sigma_sigma1d1_epsi10_alpha0d4_gamma0d8_eta1d1',
                      'sigma_sigma1d6_epsi10_alpha0d4_gamma0d8_eta1d1',
                      'sigma_sigma2d1_epsi10_alpha0d4_gamma0d8_eta1d1',
                      'sigma_sigma2d6_epsi10_alpha0d4_gamma0d8_eta1d1',
                      'sigma_sigma3d1_epsi10_alpha0d4_gamma0d8_eta1d1',
                      'alpha_sigma1d1_epsi10_alpha0d4_gamma0d8_eta1d1',
                      'alpha_sigma1d1_epsi10_alpha0d5_gamma0d8_eta1d1',
                      'alpha_sigma1d1_epsi10_alpha0d6_gamma0d8_eta1d1',
                      'alpha_sigma1d1_epsi10_alpha0d7_gamma0d8_eta1d1',
                      'alpha_sigma1d1_epsi10_alpha0d8_gamma0d8_eta1d1',
                      'alpha_sigma1d1_epsi10_alpha0d9_gamma0d8_eta1d1'
                      ]


#some function to change number into text for the file name
def coding(var):
    str_var= str(var).replace('.','d')
    return str_var

#The actual loop over parameters
for end_filename in end_filename_list:
    
    seed = 444
    np.random.seed(seed)
    random.seed(seed)

    
    print('-----------------------------------------------------------------------')
    print('Processing:  ' + end_filename)
    
    
    
    
    
    #%% 
    ############################
    # Import firm-level data
    ############################
    
    ## import data
    dta_deep=pd.read_csv('export/firm_panel_'+ end_filename +'.csv')
    #dta_deep=pd.read_csv('export_test/firm_panel_'+ end_filename +'.csv')
    
    
    
    dta_deep.columns= dta_deep.columns.str.lower()
    dta_deep = dta_deep.rename(columns={'k': 'market_id'})
    
    #restricting on the data as test
    #dta_deep = dta_deep[dta_deep['market_id'] <10]
    #dta_deep = dta_deep[dta_deep['market_id'] >99 ]
    
    # keep relevant variables
    dta_deep = dta_deep[['t','uniqueid','markupfirm','saleslevelfirm','pricefirm','salessharefirm','outputfirm','empfirm','matfirm','w','market_id','prodfirm']]
    
    
    #rename some variables
    dta_deep = dta_deep.rename(columns={'t': 'date'})
    
    dta_deep = dta_deep.rename(columns={'uniqueid': 'firmid'})
    
    
    dta_deep = dta_deep.rename(columns={'markupfirm': 'mu_true'})
    
    dta_deep = dta_deep.rename(columns={'empfirm': 'var_input'})
    dta_deep = dta_deep.rename(columns={'matfirm': 'fixed_input'})
    
    dta_deep = dta_deep.rename(columns={'w': 'var_price'})
    
    
    
    # take log of some variables
    ## output and revenue
    dta_deep[['q']] = np.log(dta_deep[['outputfirm']] )
    dta_deep[['r']] = np.log(dta_deep[['saleslevelfirm']] )
    
    ## price
    dta_deep[['p']] = np.log(dta_deep[['pricefirm']] )
    
    ##inputs
    dta_deep[['v']] = np.log(dta_deep[['var_input']] )
    dta_deep[['k']] = np.log(dta_deep[['fixed_input']] )
    

    
    ##Take log of true markup
    dta_deep[['l_mu_true']] = np.log(dta_deep[['mu_true']] )
    
    
    #define some useful variables
    ## variable input bill
    dta_deep['var_bill']= dta_deep['var_price'] * dta_deep['var_input']
    ##ratio of variable input to sales
    dta_deep['var_share_sale'] =  dta_deep['var_bill'] / dta_deep['saleslevelfirm']
    
    
    
    #%% 
    ############################
    # Import Price Index data
    ############################
    
    
    # import price data
    dta_deep_deflator=pd.read_csv('export/price_defl_'+ end_filename +'.csv')
    #dta_deep_deflator=pd.read_csv('export_test/price_defl_'+ end_filename +'.csv')
    
    
    dta_deep_deflator.columns= dta_deep_deflator.columns.str.lower()
    
    #rename time
    dta_deep_deflator = dta_deep_deflator.rename(columns={'t': 'date'})
    
    #merge with main dataset
    dta_deep = pd.merge(dta_deep, dta_deep_deflator, on='date')
    
    #Compute deflated revenue
    dta_deep['saleslevelfirm_defl']= dta_deep['saleslevelfirm'] / dta_deep['price_defl']
    
    ##take log
    dta_deep[['r_defl']] = np.log(dta_deep[['saleslevelfirm_defl']] )
    
    
    
    
    #%%
    ############################
    # Add measurement errors
    ############################
    
    
    #add measurement errors
    if Err_output:
        
        #for output
        dta_deep['eps_q'] = np.random.normal(0, dta_deep['q'].std()*(1/R2_y -1)**(1/2), size=dta_deep.shape[0])
        dta_deep['q_obs'] = dta_deep['q'] + dta_deep['eps_q']
        
        #for revenue
        dta_deep['eps_r'] = np.random.normal(0, dta_deep['r'].std()*(1/R2_y -1)**(1/2), size=dta_deep.shape[0])
        dta_deep['r_obs'] = dta_deep['r'] + dta_deep['eps_r']
        
        #for deflated revenue
        dta_deep['eps_r_defl'] = np.random.normal(0, dta_deep['r_defl'].std()*(1/R2_y -1)**(1/2), size=dta_deep.shape[0])
        dta_deep['r_defl_obs'] = dta_deep['r_defl'] + dta_deep['eps_r_defl']
        
    else:
        dta_deep['q_obs'] = dta_deep['q']
        dta_deep['r_obs'] = dta_deep['r']
        dta_deep['r_defl_obs'] = dta_deep['r_defl']
    
    
    
    
    #%%
    ############################
    # Save the data in stata for a test
    ############################
    
    if save_data_intermediate:
        dta_deep.to_stata('output_montecarlo/processed_data'+ end_filename+ '.dta')
    
    
    #%% 
    ############################
    # Set-up the Monte Carlo Repetition
    ############################
    
    
    #number of distinct markets
    N= len(dta_deep.market_id.unique()) #total number of markets
    
    #Number of market per draw
    N_iter= int(np.floor(N/Nreps))
    
    if N_iter<1:
        print('NOT ENOUGH MARKETS IN YOUR SAMPLE FOR THE NUMBER OF REPETITIONS CHOOSEN')
    else: print('There are '+ str(N_iter)+' Market in each MonteCarlo simulations')
    
    
    #initial the database to save the firm-level data
    dta_mc_reps_resample = pd.DataFrame()
    

    
    
    #%% 
    ############################
    # Run the Monte Carlo Repetition
    ############################
    
    #Initialized the list of market index to draw from
    mktids = list(dta_deep.market_id.unique())
    
    #Iterate over repetitions
    for rr in range(0, Nreps):
        print('\n Repetition ' + str(rr+1) + '/' + str(Nreps) + '---------- ')
    
        #extract random sample from mktids     
        bootmkt = random.sample(mktids, N_iter) 
        
        #Select the sample for this repetition
        dta_mc_resample = pd.DataFrame(bootmkt, columns = ['market_id']) # init the sample    
        dta_mc_resample = dta_mc_resample.merge(dta_deep, how='left', on='market_id') # merge with original dataset
    
    
        #remove the sampled market from the list of market for the next iteration    
        mktids = [i for i in mktids if i not in bootmkt] 
        
        
        #Save the data for each reps
        dta_mc_resample['reps']=rr
        
                                        
    
        ############################
        ###Saving the data used in each reps
        ############################
        
        dta_mc_reps_resample=pd.concat([dta_mc_reps_resample, dta_mc_resample])
    
    
    
    
    print('\nEnd of Processing for:  ' + end_filename + '---------------------------------')
                            
              
                        
                        
    #%%
    ############################
    #Save results
    ############################
    if save_results==True:
       
        
        dta_mc_reps_resample.to_stata('output_montecarlo/results_dataReps_ForStata_'+ end_filename+ '_saved_reps.dta')
        
       
        
    print('\n-------------------------------------------------------------')
    print('\n')
