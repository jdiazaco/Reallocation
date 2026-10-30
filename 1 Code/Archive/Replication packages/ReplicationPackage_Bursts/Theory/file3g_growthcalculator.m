function growth = file3g_growthcalculator(p,z,t,i,x_abs_type1,x_rel,G,lambda,gamma) 

    % find expected step size for own-product innovation 
    sStep= i.target_cd/(i.z+i.target_cd) * gamma * (1/(1-gamma*i.z/(i.z+i.target_cd))) * lambda  ;

    % obtain firm-size distribution 
    innovationrates = x_rel* x_abs_type1;
    K(1)= t.K_target(1); 
    K(2)= t.K_target(2); 
    lambda_burst(1) = t.lambda_burst_type1;
    lambda_burst(2) = t.lambda_burst_type2;

    % growth 
    growth = 0; 
    for type = 1:length(t.K_target)
        avg = 1+lambda_burst(type); 
        typemoment =avg/(avg-(p.epsilon-1)*(avg-1)); %the epsilon-1th moment of the step size distribution for the type 
        growth = growth + ((i.e*G(type) + innovationrates(type)*K(type)*z.minone/z.main)       *(typemoment-1))/(p.epsilon-1); 
    end 
    growth = growth + i.z*((1+sStep)^(p.epsilon-1)-1)/(p.epsilon-1);

end 