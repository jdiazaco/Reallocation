%%% Solve for the sector equilibrium


%%%
%ski = market share
%mcki = marginal cost
%sigma = elast over nest
%epsilonk = elast inner nest
%Comp = 0 monop, 1 Bertrand, 2 Cournot
%MUk = only if one to solve in binned data, MUk(t) = number of firms in bins t

%%%
%RESIDUAL TO BE PUT TO ZERO
%ski = market share
%muki = markup
%pki = price

%%%
%INPUT INTO FUNCTION
%ln_pki = log(pki)
%ln_mcki = log(mcki)

function [RES,ski,mcki,muki,yki,pki,Pk]=RES_solve_sector_fixedfactor_prices_fixed(ln_pki,ln_mcki,sigma, epsilonk,gamma,alpha,eta,Comp,wage,PsigmaY,Zki,Mki,Aki)

    %bidouille
    %ski=max(ski,0);
    %mcki=real(mcki);

    %Sector-level demand shifter
    Ak = 1;

    %Transform into levels => ensure positivity
    pki = exp(ln_pki);
    mcki = exp(ln_mcki);

    %Sector level price
    Pk = sum( Aki.* pki.^(1-epsilonk) ).^(1/(1-epsilonk));

    %back out market share
    ski = Aki.* (pki/Pk).^(1-epsilonk);

    %compute firm level markups
    if Comp==0
            muki=epsilonk./(epsilonk-1);

        %Bertrand
        elseif Comp==1
            muki= (  epsilonk.*(1-ski) + sigma.*ski  )./( epsilonk.*(1-ski) + sigma.*ski  - 1);
            
        %Cournot
        elseif Comp==2
            muki= (epsilonk )./( epsilonk-1-(epsilonk/sigma-1).*ski);
            
        %Perfect
        elseif Comp==-1
            muki= 1;
            
        %Other
        else disp('T es beau l agneau: ta structure de marche!')

    end
    
    %firm-level output level
    yki = Aki.^(-1/(epsilonk-1)) .* ski.^((epsilonk)/(epsilonk-1)) .* Ak .* Pk.^(-sigma) .* PsigmaY;
    
    %yki = Aki.^(-1/(epsilonk-1)) .* ski.^((epsilonk)/(epsilonk-1)) .* Ak .* Pk.^(-sigma) .* PsigmaY;
    
    %Marginal cost: Residual
    %RES3 = mcki - (  1/gamma .* wage .*  Zki.^(-1/gamma) .* yki.^( (1-gamma)/gamma )  .* ( 1/alpha - ((1-alpha).^(1/eta) )/alpha.* ( Zki .* Mki.^gamma ./ yki ).^( (eta-1)/(eta*gamma) )    ).^(1/(eta-1))   );
    if gamma == 1
        if eta == 1
            RES3 = mcki - (  1/alpha * wage .*  Zki.^(-1/(alpha)) .* yki.^( (1-alpha)/(alpha) ) .* (1./Mki).^((1-alpha)/alpha)   ); 

        else
            RES3 = mcki.^(eta-1) - (   (1/alpha* wage .*  Zki.^(-1) ).^(eta-1) .*  ( 1/alpha - (1-alpha)/alpha.* ( Zki .* Mki ./ yki ).^( (eta-1)/(eta) )    )   );
        end
    else
        if eta == 1
            RES3 = mcki - (  1/(alpha*gamma) .* wage .*  Zki.^(-1/(alpha*gamma)) .* yki.^( (1-alpha*gamma)/(alpha*gamma) ) .* (1./Mki).^((1-alpha)/alpha)   ); 
        else
            RES3 = mcki.^(eta-1) - (  (1/(alpha*gamma) .* wage .*  Zki.^(-1/gamma) .* yki.^( (1-gamma)/gamma )   ).^(eta-1) .* ( 1/alpha - (1-alpha)/alpha.* ( Zki .* Mki.^gamma ./ yki ).^( (eta-1)/(eta*gamma) )    )   );
        end
    end
    
    %Market Share: REsidual
    %RES2 = ski - (  ( Aki.* ( muki .* mcki ).^(1-epsilonk)  ) ./ ( sum( Aki.* ( muki .* mcki ).^(1-epsilonk)   ) )                );    
    
    %Price: Residual
    RES2 = muki .* mcki - pki;
    
    
    %RESIDUAL
    RES = [RES3; RES2 ];
    
    
    


end