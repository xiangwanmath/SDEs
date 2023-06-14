function W = wienerProcess(T,N)
dt = T / N;   
dW = sqrt(dt) * randn(1, N);

% Compute Wiener process
W = [0,  cumsum(dW)];
end