function E = file3a_stepsizesolver(x,p,i)

    % initialize variables 
    lambda = x(1);
    gamma  = x(2); 

    % find expected step size for own-product innovation 
    sStep= i.target_cd/(i.z+i.target_cd) * gamma * (1/(1-gamma*i.z/(i.z+i.target_cd))) * lambda  ;

    % find step sizes 
    E(1) =  - i.target_int + (p.epsilon-1)*(sStep* i.z - i.target_growth) + i.target_growth; %this is correct (checked 28/9/24). 
    E(2) =  - i.target_decline + (p.epsilon-1)* lambda *  gamma^1 *log(gamma); %this is correct (checked 28/9/24). Target should be based on decline year 1 vs year 2 (as s=1 in the target). Right now that's about -0.021

end 