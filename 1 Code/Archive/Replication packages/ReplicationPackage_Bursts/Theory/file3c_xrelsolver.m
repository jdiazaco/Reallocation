function E = file3c_xrelsolver_add(x,t,p,i,z)

    
    %% guesses 
    eta_guess= x(1); %cost parameter r&d 
    for type = 1:length(t.K_target)
        x_rel_guess(type) = x(1+type); 
    end 

    %% solve the value function
   
    % initialize value function 
    valfun.Pi        = (1/(p.epsilon-1))*((p.epsilon-1)/p.epsilon)^p.epsilon;
    valfun.Y         = 1; %standardizes the revenue per product. Same as pi definition 
    valfun.w         = valfun.Y/(p.epsilon/(p.epsilon-1));
    valfun.Exp       = 1;
    valfun.tails(1) = (1+t.lambda_burst_type1) ./ ((1+t.lambda_burst_type1)-1);
    valfun.tails(2) = (1+t.lambda_burst_type2) ./ ((1+t.lambda_burst_type2)-1);
    valfun.q_exp     = valfun.tails ./ (valfun.tails-(p.epsilon-1));
    valfun.rel_q_exp = valfun.q_exp/valfun.q_exp(1); 

    % per type, find the innovation option value
    valfun.maxn    = 1; % irrelevant for quantification 
    valfun.nbartax = 0 ; %set to 0 for baseline quantification  
    taxvector = zeros(valfun.maxn,1);  
    xn = zeros(valfun.maxn,length(t.K_target));
    flag = cell(length(t.K_target),1);

    % calculate innovation rates 
    for type = 1:length(t.K_target)

        % define objects 
        revenueperproduct           =  1; %normalization 
        valfun.q_exp                =  valfun.tails ./ (valfun.tails-(p.epsilon-1));
        revenueperproduct_innovator =  valfun.rel_q_exp(type).*revenueperproduct ; 
        valfun.pi                   = (1/(p.epsilon-1))*((p.epsilon-1)/p.epsilon)^p.epsilon;
        valfun.Pi                   = valfun.pi/(p.rho+(p.epsilon-1)*i.target_growth+i.target_cd) * revenueperproduct_innovator; %fine to use target growth as this is true for the final calibartion of lambda_burst 
        [temp,xn(:,type),flag{type}] = file3d_innerloop_danial(p,i.target_cd,valfun,taxvector,eta_guess);
        v_sol(type) = temp(1);
        if flag{type}.flag 
            flag_err = 1;
            type_err = type;
        end
    end


    %% calculate overall innovation rate to update etaburst 
    
    % calculate actual cd rate
    cd_true = i.e;
    for type = 1:length(t.K_target)
        cd_true = cd_true + t.K_target(type)*xn(1,type)*z.minone/z.main; 
    end 
  

    %% evaluation 
    E(1)= -cd_true + i.target_cd; 
    for type = 1:length(t.K_target)
        E(1+type) = -x_rel_guess(type)  + xn(1,type)/xn(1,1); 
    end
end 