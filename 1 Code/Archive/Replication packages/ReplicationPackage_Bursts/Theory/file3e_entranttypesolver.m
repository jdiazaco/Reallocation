function E = file3e_entranttypesolver(x,i,z,s,t,p,x_abs_type1,x_rel)

    % x is now log(type1) or logit
    type1 = exp(x(1));  % or: type1 = 1/(1+exp(-x(1))) for logit
    
    tau = i.target_cd;
    innovationrate = x_rel.*x_abs_type1;
    G = [type1, 1-type1];
    M = file3f_firmsizedistribution(innovationrate,tau,G,i,s,z,t,p);
    
    E = -sum(M(1,1:s.NN).*(1:1:s.NN))/(sum(sum(M(:,1:s.NN).*(1:1:s.NN)))) + t.K_target(1);
    %sum(M(1,1:s.NN).*(1:1:s.NN))/(sum(sum(M(:,1:s.NN).*(1:1:s.NN))))
end


