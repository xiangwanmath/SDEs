% Function to generate random numbers using Polar-Marsaglia method
function randomNumbers = polarMarsaglia(n)
    % Initialize random numbers matrix
    randomNumbers = zeros(n, 1);
    
    % Generate pseudo-random numbers using Polar-Marsaglia method
    i = 1;
    while i <= n
        % Step 1: Generate two uniform random numbers between -1 and 1
        u = rand(2, 1) * 2 - 1;
        
        % Step 2: Calculate the squared sum of the generated numbers
        s = sum(u.^2);
        
        % Step 3: Check if the squared sum is within the acceptable range (0, 1)
        if s < 1 && s > 0
            % Step 4: Apply Box-Muller transformation to generate two random numbers
            z = u * sqrt(-2 * log(s) / s);  % Standard normal random variables
            randomNumbers(i) = (z(1) + 1) / 2;    % Store the generated random variable
            i = i + 1;
        end
    end
end
