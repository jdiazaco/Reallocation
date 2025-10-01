%% housekeeping
clear all
close all

addpath('lib')


% Start logging
diary('run_multi_file_log.txt');

%%
%% %Quantitative model (alpha=0.4, eta=1.1, gamma=0.8) with sigma_z= 0.2 and rho_z= 0.6 (Baseline)
try
    main_baseline
catch
    disp('--------------------------------------------------------------------------------------------------------')
    disp('main_baseline did not work')
    disp('--------------------------------------------------------------------------------------------------------')
end


%% %Quantitative model (alpha=0.4, eta=1.1, gamma=0.8) with sigma_z= 0.2 and rho_z= 0.6 (gamma=[0.6, 0.7, 0.8, 0.9, 1])
%main_gamma

try
    main_gamma
catch
    disp('--------------------------------------------------------------------------------------------------------')
    disp('main_gamma did not work')
    disp('--------------------------------------------------------------------------------------------------------')
end


%% %Quantitative model (alpha=0.4, eta=1.1, gamma=0.8) with sigma_z= 0.2 and rho_z= 0.6 (eta= [0.95, 1, 1.05, 1.1, 1.15, 1.2])
%main_eta

try
    main_eta
catch
    disp('--------------------------------------------------------------------------------------------------------')
    disp('main_eta did not work')
    disp('--------------------------------------------------------------------------------------------------------')
end


%% %Quantitative model (alpha=0.4, eta=1.1, gamma=0.8) with sigma_z= 0.2 and rho_z= 0.6 (sigma = [1.1, 1.6 , 2.1, 2.6, 3.1])
%main_sigma

try
    main_sigma
catch
    disp('--------------------------------------------------------------------------------------------------------')
    disp('main_sigma did not work')
    disp('--------------------------------------------------------------------------------------------------------')
end


%%
%% %Quantitative model (alpha=0.4, eta=1.1, gamma=0.8) with sigma_z= 0.2 and rho_z= 0.6 (alpha = [0.4, 0.5, 0.6, 0.7, 0.8, 0.9])
%main_alpha

try
    main_alpha
catch
    disp('--------------------------------------------------------------------------------------------------------')
    disp('main_alpha did not work')
    disp('--------------------------------------------------------------------------------------------------------')
end



diary off;
