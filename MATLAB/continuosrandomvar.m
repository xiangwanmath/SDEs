clear
clc
clf
%% Uniform Variable

mew_normal = 1;  
b_uniform = 3;    
x_uniform = mew_normal:b_uniform;
y_uniform = unifpdf(x_uniform); %pdf of 1 through 5 in the continuous uniform distribution on the interval [2,4].
% Plot the Uniform distribution
figure(1)
plot(x_uniform, y_uniform);
xlabel('Distribution Interval');
ylabel('Probability Denstiy');
title('Uniform Distribution');

%% Exponential Variable
n_exponential = 100;    
lambda_exponential = 5;    
x_exponential= 0:n_exponential;
y_exponential= exppdf(x_exponential,lambda_exponential);

% Plot the exponential distribution
figure(2)
bar(x_exponential, y_exponential);
xlabel('Distribution interval');
ylabel('Probability density');
title('Exponential Distribution');

%% Gamma Variable
mew_normal = 3;   
sigma_normal = 5;    
x_uniform = 0:100;
y_normal = gampdf(x_uniform,mew_normal,sigma_normal); 
% Plot the gamma distribution
figure(3)
bar(x_uniform, y_normal);
xlabel('Distribution Interval');
ylabel('Probability Denstiy');
title('Gamma Distribution');

%% Normal Variable
mew_normal = 3;   
sigma_normal = 5;    
x_uniform = -50:50;
y_normal = normpdf(x_uniform,mew_normal,sigma_normal); 
% Plot the Uniform distribution
figure(4)
bar(x_uniform, y_normal);
xlabel('Distribution Interval');
ylabel('Probability Denstiy');
title('Normal Distribution');
