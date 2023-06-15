clear
clc
clf
%%
% Parameters
rng(100)
mu = 1;
sigma = 0.4;
a = 1;
b = 2;
num_simulations = 1E+4;  % Number of simulations
num_steps = 1000;  
dt = 0.01;  % Time step size
T = dt*num_steps;
xVec = linspace(a, b, num_steps);



%dX(t)
dX = @(X, deltaW) mu*X*dt + sigma*X*deltaW;

%check exit condition
exit_condition = @(X) X <= a | X >= b;

% Simulate the exit times
exit_times = zeros(num_simulations, 1);


mean_exit_time = linspace(a,b,num_steps);
dW = sqrt(dt) * randn(1, num_simulations);

for k = 1:size(xVec,2)
    for i = 1:num_simulations
        WP = wienerProcess(T, num_steps);
        X = zeros(num_steps +1, 1); % an extra step for X0
        X(1) = xVec(k);  % Initial value that solution is in the interval.
        for j = 1:num_steps
            dW = WP(j+1) - WP(j);
            X(j+1) = X(j) + dX( X(j), dW);
            %disp("hello")
            if exit_condition(X(j))
                exit_times(i) = j*dt;
                break;
            end
        end
    end
    % Compute the mean exit time
    mean_exit_time(k) = mean(exit_times);
end


%Solution with the BVP
u = @(xVec) (1./((1/2)*sigma.^2 - mu)) .* (log(xVec./a) ...
    - (1 - (xVec./a).^(1-2*mu./sigma.^2))./(1 - (b./a).^(1-2*mu./sigma.^2)).*log(b./a));


num_solution = u(xVec);

% Plotting
plot(xVec, num_solution, 'b-', 'LineWidth', 2);
hold on;
plot(xVec, mean_exit_time, 'r-', 'LineWidth', 1);

xlabel('x');
ylabel('..');
title('Solution of the BVP and mean exit time');

legend('Solution of the BVP', 'Mean exit time');







