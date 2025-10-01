%% De Ridder, Grassi, Morzenti copyright 2025
%%%%%%%%%simulate a panel of firms from the AB2008 model


%% housekeeping
clear all
close all

addpath('lib')
% rng(444) % set the seed
rng(666) % set the seed

%set number of cores to physical system cores
%numCores = feature('numcores')-2;

%% CHOOSE CHARACTERISTICS


%%%% Optimizer
%Use of parallel computing
paral = 0; %if you want fsolve to parallelise the gradient


solver = 'fsolve';

%choice of initial values
initial_value = "CD"; %use Cobb-Douglas result as initial values for each period

disp(['Optimizer para: ' num2str(paral) '| Initial values ' initial_value])

%%%% Sample

% repetitions
repeat = 1; % number of repetitions (if =1 then no repetitions and save in default folder)

%panel length
T=40;

% total number of firms
N_tot = 1600;

%%%%% preferences

%elasticities
epsilonk = 10;

sigma=1.1;


%%%% Productivity process
sigma_z = 0.2;
rho_z=0.6;

%%%% Fixed Factor process
sigma_m = 0.66; rho_m=0.66; % from calibration


%%% correlation
sigma_zm = 0;

%%%Aggregate Shocks
sigma_wage = sqrt(.06);
sigma_pm = sqrt(.06);
sigma_PsigmaY = sqrt(.19);

%%%% market Structure
Comp = 2; %Cournot (Bertrand=1, Monop=0)


%%%% Export
Exporting = 1; % (0 for no export of data)
% Exporting = 0; % (0 for no export of data)

filename = 'firm_panel_sigma'; % calibration based on data

%%%% choose whether you want visual output
verbose = 1;

%Translog or CES
TL = 1; %exact translog

% same_firms = 1; % repeat the same firms in every specification (1 = repeat same firms)
same_firms = 0;

for rr = 1:repeat

counter = 0; % counter for sectors


% loop over specifications
for Nk = [8]; for sigma_z = [0.2]; for rho_z=[0.6]; for sigma = [1.1, 1.6 , 2.1, 2.6, 3.1]; for eta = [1.1]; for gamma = [0.8]; for alpha = [0.4]; for N_tot = [320000]; for sigma_wage=[sqrt(.06)]; for sigma_PsigmaY=[sqrt(0.19)]  % huge sample, also for Monte Carlo

% set the seed (for each iteration)                                        
rng(666) 

%to get the same volatility of inputs prices
sigma_pm=sigma_wage;                           
                 
K = floor(N_tot / Nk);                        

if TL==1
    disp('--------------------------------------')            
    disp(['Running Case ','TL_eta',strrep(num2str(eta),'.','d'),'_gamma',strrep(num2str(gamma),'.','d'),'_alpha',strrep(num2str(alpha),'.','d')])            
    disp('--------------------------------------')            
else
    disp('--------------------------------------')            
    disp(['Running Case ','_eta',strrep(num2str(eta),'.','d'),'_gamma',strrep(num2str(gamma),'.','d'),'_alpha',strrep(num2str(alpha),'.','d')])            
    disp('--------------------------------------')            
end


%% Simulate Input price

counter = counter + 1;

if counter == 1 % generate processes only once, to have always the same process in every specification

    %wage
    log_wage = simulate( arima('Constant',0.5,'AR',{0.87 },'Variance',sigma_wage^2) , T) ;
    wage = exp(log_wage);

    %material price
    log_pm = simulate( arima('Constant',0.5,'AR',{0.87 },'Variance',sigma_pm^2) , T) ;
    pm = exp(log_pm);

    %aggregate demand 
    log_PsigmaY = simulate( arima('Constant',0.5,'AR',{0.78 },'Variance',sigma_PsigmaY^2) , T) ;
    PsigmaY=exp(log_PsigmaY);

else
    % do nothing
end

%% Simulate firm's fundamentals
%%% Simulate productivity of firms
log_firm_pdty_all = zeros(Nk,T,K);
log_firm_fixed_all = zeros(Nk,T,K);
for k = 1:K
    if counter == 1 | same_firms == 0
    
        if sigma_zm==0
            inov_pdty = sigma_z.*randn(Nk,T);
            inov_fixed = sigma_m.*randn(Nk,T);
        elseif sigma_zm>0
            R = mvnrnd([0 0],[sigma_z sigma_zm ; sigma_zm sigma_m],Nk*T);
            inov_pdty = reshape(R(:,1),[Nk T]);
            inov_fixed = reshape(R(:,2),[Nk T]);
            if Graphs==1; scatter(inov_fixed(:,10),inov_pdty(:,10)); end
        end
    
        firm_pdty_LT = 1;
        firm_pdty_init = firm_pdty_LT*exp( sigma_z.*randn(Nk,1) );
    
    
        log_firm_pdty = zeros(Nk,T);
    
        for t=1:T
            if t==1
                log_firm_pdty(:,t) = rho_z*log(firm_pdty_init) + (1-rho_z)*log(firm_pdty_LT) + inov_pdty(:,t);
            else
                log_firm_pdty(:,t) = rho_z*log_firm_pdty(:,t-1) + (1-rho_z)*log(firm_pdty_LT) + inov_pdty(:,t);
            end
        end
    
        %%% Simulate fixed factor of firms
    
        firm_fixed_LT = 0.03;
        firm_fixed_init = firm_fixed_LT*exp( sigma_m.*randn(Nk,1) );
    
    
        log_firm_fixed = zeros(Nk,T);
    
        for t=1:T
            if t==1
                log_firm_fixed(:,t) = rho_m*log(firm_fixed_init) + (1-rho_m)*log(firm_fixed_LT) + inov_fixed(:,t);
            else
                log_firm_fixed(:,t) = rho_m*log_firm_fixed(:,t-1) + (1-rho_m)*log(firm_fixed_LT) + inov_fixed(:,t);
            end
        end
    
        % store processes for next specification
        log_firm_pdty_all(:,:,k) = log_firm_pdty;
        log_firm_fixed_all(:,:,k) = log_firm_fixed;
    
    
    else
    
        % recover stored processes
        log_firm_pdty = squeeze(log_firm_pdty_all(:,:,k));
        log_firm_fixed = squeeze(log_firm_fixed_all(:,:,k));
    
    end
    
    % var(log_firm_fixed_all(:))
end

%% Simulate sectors

%init the firm-level id
firmId=1;

varname={'t','k','UniqueId', 'markupFirm', 'salesLevelFirm','salesShareFirm','priceFirm','outputFirm','empFirm','Pk', 'Yk', 'muk','w','matFirm','pm','prodFirm','elasticity_l','elasticity_m','conv_issues','fun_value'};

Panel=zeros(Nk*T,length(varname),K); % Need to pre-allocate entire Panel to be compatible with parallelization

%If no existing parallel pool create parallel session
%if isempty(gcp('nocreate'))   
%end

tic;
parfor k=1:K
    
disp('--------------------------------------')            
disp(['Simulating sector ',num2str(k)])            
disp('--------------------------------------')            

    
%% Solve for equilibrium
displaytype='Off';

if paral==1
    options=optimset('Display',displaytype,'TolFun',1e-14,'TolX',1e-14,'MaxFunEvals',2*10^6,'MaxIter',1e3 ,'UseParallel',true);
else
    options=optimset('Display',displaytype,'TolFun',1e-14,'MaxFunEvals',2*10^6,'MaxIter',10000);
end

% options for LSQNONLIN
opts1=  optimset('display',displaytype,'TolX',1e-20,'TolFun',1e-20,'MaxIter',1e5, 'MaxFunEvals',2e6);

% options for FMINSEARCH
optfmin = optimset('display','on','TolX',1e-20,'TolFun',1e-20,'MaxIter',1e5,'MaxFunEvals',1e6);

% init matrices
skit = zeros(Nk,T); 
pkit = zeros(Nk,T);
ykit = zeros(Nk,T);
mukit = zeros(Nk,T);
mckit = zeros(Nk,T);

lkit = zeros(Nk,T);
mkit = zeros(Nk,T);

log_firm_pdty = log_firm_pdty_all(:, :, k);
log_firm_fixed = log_firm_fixed_all(:, :, k);

costhare = zeros(Nk,T);
costhare_l = zeros(Nk,T);
costhare_m = zeros(Nk,T);

elasticity_l = zeros(Nk,T);
elasticity_l_bis = zeros(Nk,T);
elasticity_m = zeros(Nk,T);

Pkt = zeros(1,T);
Ykt = zeros(1,T);
mukt = zeros(1,T);

conv_issues = zeros(1,T);
fun_value = zeros(1,T);


for t=1:T
    disp(['-------------------------- period ' num2str(t)])
        
    %current period productivity, fixed factor, and demand shifter
    Zki = exp( log_firm_pdty(:,t) );
    Mki = exp( log_firm_fixed(:,t) );
    Aki = 1 ;
    
    %the residual function
    if TL==1
        fun_CD=@(x) RES_solve_sector_fixedfactor_prices_fixed(x(1:Nk),x((Nk+1):(2*Nk)),sigma, epsilonk,gamma,alpha,1,Comp,wage(t),PsigmaY(t),Zki,Mki,Aki);
        fun=@(x) RES_solve_sector_translog_fixedfactor_prices_fixed(x(1:Nk),x((Nk+1):(2*Nk)),x((2*Nk+1):(3*Nk)),sigma, epsilonk,gamma,alpha,eta,Comp,wage(t),PsigmaY(t),Zki,Mki,Aki,1);
        if solver == "fminsearch"
            fun_fmin = @(x) fun(x)'*fun(x);
        end
    else
        fun_CD=@(x) RES_solve_sector_fixedfactor_prices_fixed(x(1:Nk),x((Nk+1):(2*Nk)),sigma, epsilonk,gamma,alpha,1,Comp,wage(t),PsigmaY(t),Zki,Mki,Aki);
        fun=@(x) RES_solve_sector_fixedfactor_prices_fixed(x(1:Nk),x((Nk+1):(2*Nk)),sigma, epsilonk,gamma,alpha,eta,Comp,wage(t),PsigmaY(t),Zki,Mki,Aki);
    end
    
    
    if t==1 %faster but less stable
        %initial values
        ski_init = 1/Nk * ones(Nk,1);
        mcki_init = wage(t) .* Zki.^(-1);

        pki_monop = (epsilonk./(epsilonk-1)).*mcki_init;
        Pk_monop= sum( pki_monop.^(1-epsilonk)   ).^(1/(1-epsilonk));        

        %initial values for Cobb-Douglas
        if solver == "fsolve"
            x_CD= fsolve(fun_CD,[log(pki_monop);log(mcki_init)],options);
        elseif solver == "lsqnonlin" | solver == "iter" 
            lb = [ones(Nk,1)*10^(-8); ones(Nk,1)*10^(-8)];
            ub = [ones(Nk,1); ones(Nk,1)*10^(8)];
            [x_CD,resnorm,residual,exitflag,output]= lsqnonlin(fun_CD,[log(pki_monop);log(mcki_init)],lb,ub,opts1);
            if exitflag<=0  disp(['CD initial values NOT CONVERGING: exitflag = ' num2str(exitflag)]); end
        else
            x_CD= fsolve(fun_CD,[log(pki_monop);log(mcki_init)],options);
        end
        
        xt_CD = zeros(2*Nk,T); % init this variable
        xt_CD(:,t)=x_CD;
        
        %Solve for the other variables
        [crap0,crap,mcki_CD_temp,crap3,yki_CD_temp,crap4,crap5] = fun_CD(x_CD);
        
        %labor
        lkit_CD=gamma * alpha * (wage(t)./mcki_CD_temp).^(-1).* yki_CD_temp ;

        %initial values
        if TL==1
            x_init=real([x_CD; log(real(lkit_CD))]);
        else
            x_init=real(x_CD);
        end
        
       
    else
        %initial values
        ski_init = 1/Nk * ones(Nk,1);
        mcki_init = wage(t) .* Zki.^(-1);

        pki_monop = (epsilonk./(epsilonk-1)).*mcki_init;
        Pk_monop= sum( pki_monop.^(1-epsilonk)   ).^(1/(1-epsilonk));
        ski_monop = ( pki_monop./ Pk_monop ).^(1-epsilonk);

        
        %initial values for Cobb-Douglas
        if solver == "fsolve"
            x_CD= fsolve(fun_CD,[log(pki_monop);log(mcki_init)],options);
        elseif solver == "lsqnonlin" | solver == "iter"
            lb = [ones(Nk,1)*10^(-8); ones(Nk,1)*10^(-8)];
            ub = [ones(Nk,1); ones(Nk,1)*10^(8)];
            [x_CD,resnorm,residual,exitflag,output]= lsqnonlin(fun_CD,[log(pki_monop);log(mcki_init)],lb,ub,opts1);
            if exitflag<=0  disp(['CD initial values NOT CONVERGING: exitflag = ' num2str(exitflag)]); end
        else
            x_CD= fsolve(fun_CD,[log(pki_monop);log(mcki_init)],options);
        end
        
   
        %Solve for the other variables
        [crap0,crap,mcki_CD_temp,crap3,yki_CD_temp,crap4,crap5] = fun_CD(x_CD);
     
        %labor
        lkit_CD=gamma * alpha * (wage(t)./mcki_CD_temp).^(-1).* yki_CD_temp ;
        
        
        if TL==1
            x_init=abs(real([x_CD; log(real(lkit_CD)) ]));
        else
            x_init=real(x_CD);
        end
       
    end
    
   %Solve for equilibrium
    disp(['Solving....'])
    
    if solver == "fsolve"
        [x,fval,flag,output]= fsolve(fun,x_init,options);
    elseif solver == "lsqnonlin" 
        lb = [ones(Nk,1)*10^(-8); ones(Nk,1)*10^(-8); ones(Nk,1)*10^(-8)];
        ub = [ones(Nk,1); ones(Nk,1)*10^(8); ones(Nk,1)*10^(8)];
        [x,resnorm,residual,exitflag,output] = lsqnonlin(fun,x_init,lb,ub,opts1);
    elseif solver == "fminsearch"
        [x,fval,exitflag,output] = fminsearch(fun_fmin,x_init,optfmin);
    elseif solver == "iter"
        disp('Attempting FSOLVE...')
        solver_iter_check = 0;
        [x,fval,flag,output]= fsolve(fun,x_init,options);
        if flag <= 0
            disp('FSOLVE did not converge. Attempting LSQNONLIN...')
            solver_iter_check = 1;
            lb = [ones(Nk,1)*10^(-8); ones(Nk,1)*10^(-8); ones(Nk,1)*10^(-8)];
            ub = [ones(Nk,1); ones(Nk,1)*10^(8); ones(Nk,1)*10^(8)];
            [x,resnorm,residual,exitflag,output] = lsqnonlin(fun,x_init,lb,ub,opts1);
        end
    else
        disp('Solver Mis-spcified!!! Solving the model using FSOLVE')
        [x,fval,flag,output]= fsolve(fun,x_init,options);
    end    
    
    
    
    if solver=="fsolve"
        if flag<=0; disp( [ 'convergence problem: ' num2str(flag)]); end
    end
    if solver=="lsqnonlin"
        if exitflag<=0; disp( [ 'convergence problem: ' num2str(exitflag)]); end
    end
    
    %Solve for the other variables
    [RES,ski_temp,mcki_temp,muki_temp,yki_temp,pki_temp,Pk_temp] = fun(x);
    x = exp(x); %Transform variables back
    
    % replace complex numebers with real ones
    ski_temp = real(ski_temp);
    mcki_temp = real(mcki_temp);
    muki_temp = real(muki_temp);
    yki_temp = real(yki_temp);
    pki_temp = real(pki_temp);
    Pk_temp = real(Pk_temp);
    


    %disp(sum(ski_temp))
    %market share
    skit(:,t)=ski_temp;
    %marginal cost
    mckit(:,t)=mcki_temp;
    %price    
    pkit(:,t)=pki_temp;
    %quantity
    ykit(:,t)=yki_temp;
    %markup
    mukit(:,t)=muki_temp;
    %fixed factor
    mkit(:,t)=Mki;
    %labor
    if TL==1
        lki_temp = real( x((2*Nk+1):(3*Nk)) );
        lkit(:,t) = lki_temp;
    else
        lkit(:,t)=(gamma * alpha)^eta * (wage(t)./mcki_temp).^(-eta).* Zki.^((eta-1)/(gamma)) .* yki_temp.^((1+eta*(gamma-1))/gamma) ;
    end
    %sector price index
    Pkt(t) = Pk_temp;
    %sector level quantity
    Ykt(t) = Pkt(t).^(-sigma).*PsigmaY(t);
    %sector level markup
    mukt(t)=( sum( mukit(:,t).^(-1) .* skit(:,t)  )  )^(-1);
    
    %cost share
    costhare_l(:,t) = ( skit(:,t).* Pkt(t).*Ykt(t) )./ (lkit(:,t) * wage(t));
    costhare_m(:,t) = ( skit(:,t).* Pkt(t).*Ykt(t) )./ (mkit(:,t) * pm(t)); 
    
    %Elasticity
    elasticity_l_bis(:,t) = mukit(:,t) ./ costhare_l(:,t) ;
    if TL==1
        elasticity_l(:,t) = gamma * alpha.*( 1+ (1-alpha).*((eta-1)/eta).* log( lkit(:,t) ./ mkit(:,t)  )  );
    else        
        elasticity_l(:,t) = gamma * alpha .* lkit(:,t).^((eta-1)/eta) .* Zki.^((eta-1)/(eta*gamma)) .* ykit(:,t).^(-(eta-1)/(eta*gamma)) ;
    end
    

    if verbose==1
        disp(['Sum of market share: ' num2str(sum(skit(:,t)))])
        disp(['HHI :' num2str(sum(skit(:,t).^2))])
        disp(['mean(log(mu)) :' num2str(mean(log(muki_temp)))])
        disp(['std(log(mu)) :' num2str(std(log(muki_temp)))])
        disp(['Var(log(mu)) :' num2str(var(log(muki_temp)))])
    end
    
    if solver=="fsolve"
        disp( [ 'Convergence Flag : ' num2str(flag)]);
        disp( [ 'Convergence Fval : ' num2str( sqrt(fval'*fval)) ] );
        fun_value(t)=sqrt(fval'*fval);
        if flag<=0
           disp( [ 'Code stopped because of convergence error: ' num2str(flag)]);
           conv_issues(t)=1;
    %         return; % this line stops the code
        end
    end    
    if solver=="lsqnonlin"
        disp( [ 'Convergence Flag : ' num2str(exitflag)]);
        disp( [ 'Convergence Fval : ' num2str( resnorm ) ] );
        fun_value(t)=resnorm;
        if exitflag<=0 % && resnorm>1e-6
            disp( [ 'Exit Message : ' output.message ]);
            conv_issues(t)=1;
        end
    end
    if solver == "fminsearch"
        disp( [ 'Convergence Flag : ' num2str(exitflag)]);
        disp( [ 'Convergence Fval : ' num2str( fval ) ] );
        fun_value(t)=fval;
        if exitflag<=0
            disp( [ 'Exit Message : ' output.message ]);
            conv_issues(t)=1;
    %         return; % this line stops the code
        end
    end
    if solver=="iter"
        if solver_iter_check==0 % used FSOLVE
            disp( [ 'Convergence Flag : ' num2str(flag)]);
            disp( [ 'Convergence Fval : ' num2str( sqrt(fval'*fval)) ] );
            fun_value(t)=sqrt(fval'*fval);
            if flag<=0
               disp( [ 'Code stopped because of convergence error: ' num2str(flag)]);
               conv_issues(t)=1;
            %         return; % this line stops the code
            end
        elseif solver_iter_check==1 % used LSQNONLIN
            disp( [ 'Convergence Flag : ' num2str(exitflag)]);
            disp( [ 'Convergence Fval : ' num2str( resnorm ) ] );
            fun_value(t)=resnorm;
            if exitflag<=0
                disp( [ 'Exit Message : ' output.message ]);
                conv_issues(t)=1;
    %             return; % this line stops the code
            end
        end
    end    
    
    
end

%% Create Panel for Exporting


Panel_tmp = zeros(Nk*T, length(varname));

for t=1:T
        for i=1:Nk
            %disp(rows)
            rows = i + (t - 1)*Nk;

            %basics information
            Panel_tmp(rows,1)=t; %year
            Panel_tmp(rows,2)=k; %sector
%             Panel_tmp(rows,3)=i; %note that to uniquely identify a firm, one need to look at firmId and sector index k %firmIdSector in a sector
            Panel_tmp(rows,3)=k*10000 + i; % uniquely identify a firm
            
            %firm level things
            Panel_tmp(rows,4)= mukit(i,t) ; %markup
            Panel_tmp(rows,5)= skit(i,t).*Pkt(t)*Ykt(t); %saleslevel
            Panel_tmp(rows,5)= pkit(i,t).*ykit(i,t); %saleslevel
            Panel_tmp(rows,6)= skit(i,t); %salesshare
            Panel_tmp(rows,7)= pkit(i,t); %price
            Panel_tmp(rows,8)= ykit(i,t); %quantity
            Panel_tmp(rows,9)= lkit(i,t); %employement

            %sector info
            Panel_tmp(rows,10)= Pkt(t); %sector price
            Panel_tmp(rows,11)= Ykt(t); %sector output
            Panel_tmp(rows,12)= mukt(t); %sector markup

            %aggregate info
            Panel_tmp(rows,13)= wage(t); %wage

            %materials
            Panel_tmp(rows,14)= mkit(i,t); %materials
            Panel_tmp(rows,15)= pm(t); %materials price
            
            %productivity
            Panel_tmp(rows,16)= exp( log_firm_pdty(i,t) ); %productivity

            %elasticities
            Panel_tmp(rows,17)= elasticity_l(i,t); %elasticity of labor
            Panel_tmp(rows,18)= elasticity_m(i,t); %elasticity of materials
            
            % convergence issues
            Panel_tmp(rows,19)= conv_issues(t); %elasticity of materials
            Panel_tmp(rows,20)= fun_value(t); %elasticity of materials

            %if mod(rows,100)==0; disp(['Row ', num2str(rows), ' of ' , num2str(Nk*T)] );  end
        end
end

Panel(:,:, k) = Panel_tmp; % Fill in Panel -> to be compatible with parallelization

%

end % end of multisector loop
toc;

delete(gcp('nocreate'))

if max(max(imag(abs(Panel))))>10^(-5)
    disp('Check imaginary numbers')
end
    
Panel=real(Panel);
Panel=reshape(permute(Panel,[1,3,2]), K*Nk*T, length(varname)); % reshape panel

%% Deflator

Panel_defl = Panel;

% keep relevant variables [t k pk]
Panel_defl = Panel_defl(:,[1 2 10]);
% compute price^(1-sigma)
Panel_defl(:,end+1) = Panel_defl(:,3) .^ (1-sigma);
% drop duplicates rows
Panel_defl = unique(Panel_defl, 'rows');
% add mean
% Panel_defl(:,end+1) = sum(Panel_defl(:,end),1);
Panel_defl2 = [1:T]';
Panel_defl2(:,2) = zeros(1,T);
for t = [1:T]
    Panel_defl2(t,2) = mean( Panel_defl( Panel_defl(:,1)==t ,end) ) ^ (1 / (1-sigma));
end


%% Export the data in a csv file
%

if verbose==1; disp(['%%%%%%%%%%%%%%%%%%% Export the data in a csv file...' ]); end

tic;

% specify the folder
if repeat == 1
    folder = 'export/';
    rrr = '';
elseif repeat > 1 
    folder = 'export/repetition/';
    rrr = ['_rep',num2str(rr)];
end


figNameEnd_demand=['_sigma',strrep(num2str(sigma),'.','d'),'_epsi',strrep(num2str(epsilonk),'.','d')];

figNameEnd_prod=['_alpha',strrep(num2str(alpha),'.','d'),'_gamma',strrep(num2str(gamma),'.','d'),'_eta',strrep(num2str(eta),'.','d')];

figNameEnd=[figNameEnd_demand,  figNameEnd_prod];


exmu_str = '';
trend_str = '';


multi = '';


if TL==1
    export_filename= [folder,filename,multi,exmu_str,trend_str,figNameEnd,rrr,'.csv' ];
else
    export_filename= [folder,filename,multi,exmu_str,trend_str,figNameEnd,rrr,'.csv' ];
end

% deflator names
varname_defl={'t','price_defl'};
filename_defl = ['price_defl' erase(filename,'firm_panel')];
export_filename_defl = [folder,filename_defl,multi,exmu_str,trend_str,figNameEnd,rrr,'.csv' ];

%% Exporting

%the data
if Exporting == 1

    %%
    %main panel
    fid = fopen(export_filename, 'w') ;
    fprintf(fid, '%s,', varname{1:length(varname)-1}) ;
    fprintf(fid, '%s\n', varname{length(varname)}) ;
    fclose(fid) ;
    
    dlmwrite(export_filename, Panel, '-append', 'precision', 15) ;
    disp(['%%%%% Data are exported to : ' export_filename  ]);

    %%    
    %deflator
    fid = fopen(export_filename_defl, 'w') ;
    fprintf(fid, '%s,', varname_defl{1:length(varname_defl)-1}) ;
    fprintf(fid, '%s\n', varname_defl{length(varname_defl)}) ;
    fclose(fid) ;
    
    dlmwrite(export_filename_defl, Panel_defl2, '-append', 'precision', 15) ;

else
    disp('%%%%% Data NOT exported');
end

toc;



end; end; end; end; end; end; end; end; end; end;

end
