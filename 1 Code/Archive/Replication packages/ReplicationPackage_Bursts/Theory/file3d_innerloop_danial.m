function [V, gs, flags] = file3d_innerloop_danial(p,cd_rate,valfun,taxvector,etaburst)
% FILE3D_INNERLOOP_DANIAL  Solve HJB equation for innovation option values
%
% OPTIMIZATION: Cached M matrix using persistent variables
% DECISION: Matrix M depends only on (Ninf, tht), cache for reuse
% RATIONALE: M is expensive to compute (Ninf x Ninf matrix operations)
% NUMERICAL PRESERVATION: Mathematically identical computation

%% Parameters - translate paper's parameters to Danial's notation

% define parameters in Danial's notation
par.pi                      = valfun.Pi*valfun.w;
par.psi                     = p.psi-1;
par.rho                     = p.rho;
par.del                     = cd_rate;
par.tht                     = p.zetapar;
z.tht                       = zeta(par.tht);
z.thtminon                  = zeta(par.tht-1);
eta_danial                  = (1+par.psi).*valfun.w.*(1-taxvector).*etaburst;

Bl   = eta_danial(1).^(-1/par.psi);
Bh   = eta_danial(end).^(-1/par.psi);
Nb   = valfun.nbartax;
par.Bs    = @(n) Bl.*(n < Nb) + Bh.*(1-(n < Nb));

Ninfs = [valfun.maxn];
Nshow = 100;


%% Infinite N limit solution

v_tan  = ((par.rho+par.del)/Bh).^(par.psi)*(z.thtminon/z.tht).^(-par.psi-1) - par.pi;
v_tan0 = ((par.rho+par.del)/Bl).^(par.psi)*(z.thtminon/z.tht).^(-par.psi-1) - par.pi;


func_inf = @(v) (par.rho+par.del)*v- 1./(1+1./par.psi) * Bh*(z.thtminon/z.tht*(par.pi+v)).^(1+1./par.psi);
func_zer = @(v) (par.rho+par.del)*v- 1./(1+1./par.psi) * Bl*(z.thtminon/z.tht*(par.pi+v)).^(1+1./par.psi);
options = optimset('Display', 'off');
vinf = fzero(func_inf, 0, options);
vzer = fzero(func_zer, 0, options);

% If you don't find a solution, figure out how faraway you are from having a solution
flags.flag_zer  = (isnan(vzer));
flags.flag_inf  = (isnan(vinf));
flags.flag      = (flags.flag_zer + flags.flag_inf > 0);

if flags.flag_inf
    lsqoptions = optimoptions(@lsqnonlin,'Algorithm','levenberg-marquardt',...
        'MaxFunctionEvaluations',1500,'Display','Off');
    [flags.v_inf_min, flags.fval_inf] = lsqnonlin(func_inf, v_tan, [], [], [],[],[],[],[], lsqoptions);
end
if flags.flag_zer
    lsqoptions = optimoptions(@lsqnonlin,'Algorithm','levenberg-marquardt',...
        'MaxFunctionEvaluations',1500,'Display','Off');
    [flags.v_zer_min, flags.fval_zer] = lsqnonlin(func_zer, v_tan0, [], [], [],[],[],[],[], lsqoptions);
end

% innovation rates at zero and at n = infty
g_inf = (z.thtminon/z.tht*(vinf/valfun.w+valfun.Pi)/(etaburst*p.psi))^(1/(p.psi-1));
g_zer = (z.thtminon/z.tht*(vinf/valfun.w+valfun.Pi)/(etaburst*p.psi))^(1/(p.psi-1));


%% continue if there is a solution

if ~flags.flag

    Vinit = @(n) vzer.*(n < Nb) + vinf.*(1-(n < Nb));


    for index = 1:length(Ninfs)

        Ninf = Ninfs(index);

        % objective function
        ns   = (1:Ninf)';
        if index == 1
            V_init    = vinf*ones(Ninf, 1);
            V_init    = Vinit(ns);
        else
            V_init    = [V; vinf*ones(Ninf-length(V),1)];
        end

        % OPTIMIZATION: Get cached M matrix or compute if needed
        M = getCachedMatrix(Ninf, par.tht);

        objfun_lin = @(V) V - RHS(V, ns, par, M, z);

        lsqoptions = optimoptions(@lsqnonlin,'Algorithm','levenberg-marquardt',...
                                   'MaxFunctionEvaluations',1500,'Display','off');
        V = lsqnonlin(objfun_lin, V_init, [], [], lsqoptions);

        vals = V - RHS(V, ns, par, M, z);
        objval(index) = sqrt(mean(vals(1:min(100,valfun.maxn)).^2));
        gs   = innov_rate(V, ns);

        objval(index);
        1;
        error_flag = 0;

    end

else
    V  = zeros(Ninfs, 1);
    gs = zeros(Ninfs, 1);
    error_flag = 1;
end


%% Helper: Get cached M matrix
function M = getCachedMatrix(Ninf, tht)
    % OPTIMIZATION: Cache M matrix using persistent variables
    % M depends only on Ninf and tht, so we cache the last computed value
    persistent cached_M cached_Ninf cached_tht

    if isempty(cached_M) || cached_Ninf ~= Ninf || cached_tht ~= tht
        % Recompute M
        [I,J] = ndgrid(1:Ninf, 1:Ninf);
        M     = max(0, J-I);
        M     = M.^(-tht);
        M(isinf(M)) = 0;

        % Cache for next call
        cached_M = M;
        cached_Ninf = Ninf;
        cached_tht = tht;
    else
        % Return cached value
        M = cached_M;
    end
end

%% solvers
function Vl = RHS(Vr, ns,  par, M, z)

    matVr = M*(ns.*Vr)/z.tht;

    vecVr_old = ns.*hurwitzZeta(par.tht, max(ns)-ns+1) + hurwitzZeta(par.tht-1, max(ns)-ns+1);
    vecVr = ns.*p.Hurwitz_tht(max(ns)-ns+1)' + p.Hurwitz_tht_1(max(ns)-ns+1)';
    max(abs(vecVr-vecVr_old));

    vecVr = vecVr/z.tht.*Vr(end);

    term1 = par.del * [-Vr(1); ...
                       ns(1:end-1).*Vr(1:end-1)-ns(2:end).*Vr(2:end)];
    term2 = 1./(1+1./par.psi) * par.Bs(ns).*[ z.thtminon/z.tht*par.pi + matVr(1:end-1) + vecVr(1:end-1) - ns(1:end-1).*Vr(1:end-1); ...
                                              z.thtminon/z.tht*(par.pi + Vr(end))                                         ].^((1+1./par.psi));

    Vl    = (term1 + term2)/par.rho;

end


function g = innov_rate(Vr, ns)

    g = (z.thtminon./z.tht./(etaburst*p.psi).*([ valfun.Pi + ns(2:end).*Vr(2:end)./valfun.w - ns(1:end-1).*Vr(1:end-1)./valfun.w; ...
                                            valfun.Pi + Vr(end)/valfun.w ])).^(1/(p.psi-1));

end


end
