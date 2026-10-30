function M = file3f_firmsizedistribution(x,tau,G,i,s,z,t,p)
% FILE3F_FIRMSIZEDISTRIBUTION  Compute stationary firm-size distribution
%
%   M = file3f_firmsizedistribution(x, tau, G, i, s, z, t, p)
%
%   Returns M(type, n) = mass of firms of type 'type' with n products
%
% OPTIMIZATION: Vectorized across types, precomputed power terms
% DECISION: Kept sequential linecount loop (has data dependency)
% RATIONALE: M(n+1) depends on M(1:n), cannot parallelize that dimension
% NUMERICAL PRESERVATION: Mathematically identical computation

nTypes = length(t.K_target);

% Initialize
M = zeros(nTypes, s.NNExt);

% Innovation vector: xn(type, n) = x(type) * n
xn = x' .* (1:s.NN);  % nTypes x NN matrix

% OPTIMIZATION: Vectorized computation for n=1,2 across all types
M(:,1) = i.e / tau * G';
M(:,2) = ((-i.e * G') + xn(:,1) .* M(:,1) + M(:,1) * tau) / (2*tau);

% OPTIMIZATION: Precompute power matrix for zeta terms
% powerMat(linecount, k) = (linecount - k)^(-zetapar) for k < linecount
% We need this for linecount = 2 to NN, k = 1 to linecount-1
maxN = s.NN;
powerMat = zeros(maxN, maxN);
for linecount = 2:maxN
    ks = 1:(linecount-1);
    powerMat(linecount, ks) = (linecount - ks).^(-p.zetapar);
end
powerMat = powerMat / z.main;  % Include z.main division

% Compute M for n > 2 (sequential in linecount due to dependency)
for linecount = 2:s.NN
    % OPTIMIZATION: Vectorized across types
    % Inner sum: sum_{k=1}^{linecount-1} -xn(type,k) * M(type,k) * powerMat(linecount,k)
    ks = 1:(linecount-1);
    innerSum = sum(-xn(:,ks) .* M(:,ks) .* powerMat(linecount,ks), 2);  % Sum over k, result is nTypes x 1

    % Full formula
    numerator = innerSum + xn(:,linecount) .* M(:,linecount) + M(:,linecount) * tau * linecount;
    M(:,linecount+1) = numerator / ((linecount+1) * tau);
end

end
