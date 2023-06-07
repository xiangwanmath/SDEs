clear 
clc
clf
%% PC Excercise 1.3.1


n = 100;       % Number of random numbers to generate
lambda = 0.5;  % Rate parameter for the exponential distribution

% Generate and display random numbers using Polar-Marsaglia method
randomNumbers = polarMarsaglia(n, lambda);
disp(randomNumbers);


% Function to generate random numbers using Polar-Marsaglia method
function randomNumbers = polarMarsaglia(n, lambda)
    % Initialize random numbers matrix to store the pseudorandom numbers
    randomNumbers = zeros(n, 3);
    
    % Generate pseudo-random numbers using Polar-Marsaglia method
    i = 1;
    while i <= n
        u = rand(2, 1) * 2 - 1; %Generating two uniform random numbers between -1 and 1
        s = sum(u.^2); %squared sum of the generated numbers
        if s <= 1 
            % Apply Box-Muller transformation to generate two random numbers
            z = u * sqrt(-2 * log(s) / s);  
            
            % Generate exponential random variables using inverse transform
            x = -log(rand(2, 1)) / lambda;  
            
            randomNumbers(i, :) = [x(1), z(1), z(2)];  % Store the generated random variables
            i = i + 1;
        end
    end
end

