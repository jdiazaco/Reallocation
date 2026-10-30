function E = file3b_typesolver(x,a) 

    % initialize variables 
    lambda_burst_type1 =    x(1); 
    lambda_burst_type2 =    x(2);
    K_target           = [1-x(3) ,x(3)];
    pi1 = K_target(1);
    lambda1 = 1 + lambda_burst_type1;
    lambda2 = 1 + lambda_burst_type2;

    % moment 1
    mean = pi1*lambda1 + (1-pi1)*lambda2;   % keep your original 'mean'

    % moment 2 
    autoregbeta =   ( pi1*(1-pi1)*( 1/lambda1 - 1/lambda2 )^2 ) / ...
                        ( pi1*(1-pi1)*( 1/lambda1 - 1/lambda2 )^2 ...
                        + pi1*( (lambda1-1)/lambda1 )^2 ...
                        + (1-pi1)*( (lambda2-1)/lambda2 )^2 );

    % moment 3 
    W1 = log(lambda1);
    W2 = log(lambda2);
    EW = pi1*W1 + (1-pi1)*W2;
    VarLog = pi1*(W1 - EW)^2 + (1-pi1)*(W2 - EW)^2; 
    relvar = VarLog / (EW^2);
 
    % evaluate 
    E(1) = - a.relvar           + relvar; 
    E(2) = - a.autoregbeta  + autoregbeta; 
    E(3) = - a.mean             + mean; 


end 
