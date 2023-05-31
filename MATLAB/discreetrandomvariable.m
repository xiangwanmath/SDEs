clear
clc
clf
%% Geometric variable

p_geometric = 0.5;   % Success probability for geometric distribution
N_geometric = 100;    % Number of trials for geometric distribution
x_geometric = 0:N_geometric;
y_geometric = geopdf(x_geometric, p_geometric);
% Plot the geometric distribution
figure(1)
bar(x_geometric, y_geometric);
xlabel('Number of Trials');
ylabel('Probability');
title('Geometric Distribution');

% Compute mean and standard deviation for geometric distribution
mean_geometric = 1 / p_geometric;
std_geometric = sqrt((1 - p_geometric) / (p_geometric^2));

fprintf('Geometric Distribution:\n');
fprintf('Mean: %.1f\n', mean_geometric);
fprintf('Standard Deviation: %.1f\n', std_geometric);

%% Binomial variable
n_binomial = 10;     % Number of trials for binomial distribution
p_binomial = 0.5;    % Success probability for binomial distribution
x_binomial = 0:n_binomial;
y_binomial = binopdf(x_binomial, n_binomial, p_binomial);

% Plot the binomial distribution
figure(2)
bar(x_binomial, y_binomial);
xlabel('Number of Successes');
ylabel('Probability');
title('Binomial Distribution');

% Compute mean and standard deviation for binomial distribution
mean_binomial = n_binomial * p_binomial;
std_binomial = sqrt(n_binomial * p_binomial * (1 - p_binomial));

fprintf('Binomial Distribution:\n');
fprintf('Mean: %.1f\n', mean_binomial);
fprintf('Standard Deviation: %.1f\n', std_binomial);


%% Poisson Variable

lambda_poisson = 5;  % Poisson parameter
x_poisson = 0:15;
y_poisson = poisspdf(x_poisson, lambda_poisson);

% Plot the Poisson distribution
figure(3)
bar(x_poisson, y_poisson);
xlabel('Number of Events');
ylabel('Probability');
title('Poisson Distribution');

% Compute mean and standard deviation for Poisson distribution
mean_poisson = lambda_poisson;
std_poisson = sqrt(lambda_poisson);

fprintf('Poisson Distribution:\n');
fprintf('Mean: %.1f\n', mean_poisson);
fprintf('Standard Deviation: %.1f\n', std_poisson); 