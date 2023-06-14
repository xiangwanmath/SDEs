clear
clc
clf

%%
rng(100);

% Set parameters
T = 1;             
N = 5E+4;  
dt = T/N;
W=wienerProcess(T,N);
t = 0:dt:T;
plot(t, W);

ito = itoIntegral(W);
stratonovich = stratonovichIntegral(W);
right_sum = rightReinmann(W);
disp("Ito Integral: " + ito);
disp("Stratonovich Integral: " +stratonovich);
disp("Right Reinmann Sum: " + right_sum);


%Part b
ito = ((W(end) .* W(end))*0.5) -  (T*0.5);
strat = ((W(end) .* W(end))*0.5);
disp("Ito by def " + ito)
disp("Stratonovich by def " + strat)

