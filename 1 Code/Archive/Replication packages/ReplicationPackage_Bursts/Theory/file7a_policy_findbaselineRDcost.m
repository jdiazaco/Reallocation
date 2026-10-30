function [eta, eta_entry, growth, v_sol] = file7a_policy_findbaselineRDcost(endo,i,z,p,t) 

%% manually define some inputs 

% revenue per product/profit flow 
revenueperproduct           =  1; % eta relative to this 
valfun.tails_type1          = (1+endo.lambdaburst_type1) ./ ((1+endo.lambdaburst_type1)-1);
valfun.tails_type2          = (1+endo.lambdaburst_type2) ./ ((1+endo.lambdaburst_type2)-1);
valfun.q_exp(1)             =  valfun.tails_type1 ./ (valfun.tails_type1-(p.epsilon-1));
valfun.q_exp(2)             =  valfun.tails_type2 ./ (valfun.tails_type2-(p.epsilon-1));
valfun.rel_q_exp            =  valfun.q_exp/valfun.q_exp(1);
revenueperproduct_innovator =  valfun.rel_q_exp.*revenueperproduct ; 

% profit flow, present value
growth           = i.target_growth; 
valfun.pi        = (1/(p.epsilon-1))*((p.epsilon-1)/p.epsilon)^p.epsilon;
valfun.Pi        = valfun.pi/(p.rho+(p.epsilon-1)*growth+i.target_cd) * revenueperproduct_innovator;


%% call the functions  

% specify target
i.x = i.xrate_target *z.main/z.minone; 

% choose initial values (eta that delivers guess for type 2 when option value is 0) 
v_guess = 0; 
eta_guess = i.x^(p.psi-1)*valfun.Pi(2)*z.minone/z.main; 

% find root
lsqoptions = optimoptions(@lsqnonlin,'Algorithm','levenberg-marquardt',...
            'MaxFunctionEvaluations',3000,...
            'FunctionTolerance',1e-10,...     % Decrease this (default is 1e-6)
            'StepTolerance',1e-10,...         % Decrease this (default is 1e-6)
            'OptimalityTolerance',1e-10) ;    % Decrease this (default is 1e-6)
[sol,~,~,flag] =  lsqnonlin(@(x) Optval(x,valfun,z,i), [v_guess, eta_guess,v_guess], [0,0+10^(-10),0], [], lsqoptions);
eta = sol(2)/p.psi; 
v_sol = [sol(1),sol(3)];

% check solution 
x_check = t.K_target(1)*(z.minone/z.main*(sol(1)+valfun.Pi(1))/(eta*p.psi))^(1/(p.psi-1)) + t.K_target(2)*(z.minone/z.main*(sol(3)+valfun.Pi(2))/(eta*p.psi))^(1/(p.psi-1)) ;
if abs(x_check-i.x)>10^(-4)
    warning('initial value not solved')
end
 
% find eta_entry 
eta_entry_timespsi = i.e^(-(p.psi-1))*(endo.G(1)*(sol(1)+valfun.Pi(1))+endo.G(2)*(sol(3)+valfun.Pi(2)));
eta_entry= eta_entry_timespsi/p.psi;


%% specify the functions 
function Res = Optval(x,valfun,z,i)

    % variables 
    v1= x(1); % option value of type 1 
    etatilde = x(2); % r&d cost shifter 
    v2 = x(3); % option value of type 2 

    % evaluation 
    Res(1) = -i.x + t.K_target(1)*(z.minone/z.main*(valfun.Pi(1)+v1)/etatilde)^(1/(p.psi-1)) + t.K_target(2)*(z.minone/z.main*(valfun.Pi(2)+v2)/etatilde)^(1/(p.psi-1)); 
    Res(2) = - (p.rho+i.target_cd)*v1 + ((p.psi-1)/p.psi)*etatilde.^(-1/(p.psi-1)) *(z.minone/z.main*(valfun.Pi(1)+v1))^(p.psi/(p.psi-1)); 
    Res(3) = - (p.rho+i.target_cd)*v2 + ((p.psi-1)/p.psi)*etatilde.^(-1/(p.psi-1)) *(z.minone/z.main*(valfun.Pi(2)+v2))^(p.psi/(p.psi-1)); 

    %Res(1) = -i.x + (z.minone/z.main*(valfun.Pi+v)/etatilde)^(1/(p.psi-1)); 
    %Res(2) = - (p.rho+i.target_cd)*v + ((p.psi-1)/p.psi)*etatilde.^(-1/(p.psi-1)) *(z.minone/z.main*(valfun.Pi+v))^(p.psi/(p.psi-1)); 

end 



end 