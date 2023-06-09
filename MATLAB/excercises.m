clear
clc
clf

%%Excercise 1.3.1 is the polarMarsaglia function. 

%% (PC-Exercise 1.4.4, p. 16)

N = 1E+4;                     % Number of pseudo-random numbers to generate
subintervalLength = 5e-2;      % Length of subintervals
numSubintervals = 1 / subintervalLength;   % Number of subintervals

% Generate pseudo-random numbers using Polar-Marsaglia method by calling
% the funcion
randomNumbers = polarMarsaglia(N);

% Count the number of generated numbers falling into each subinterval
counts = histcounts(randomNumbers, numSubintervals);

% Compute the relative frequencies divided by subinterval length
relativeFrequencies = counts / (N * subintervalLength);

% Compute the bin centers for plotting the histogram
binCenters = (subintervalLength/2) : subintervalLength : 1 - (subintervalLength/2);

% Plot the histogram of the relative frequencies
figure(1)
bar(binCenters, relativeFrequencies);
xlabel('SubIntervals');
ylabel('Relative Frequency');
title('Histogram of Uniform random Pseudo-Random Numbers');

%calculating sample mean and sample average
sampleAverage = mean(randomNumbers);
sampleVariance = var(randomNumbers);
disp(['Sample Average: ', num2str(sampleAverage)]);
disp(['Sample Variance: ', num2str(sampleVariance)]);


%% (PC-Exercise 1.4.5, p. 16)

N = 1E+4;                     % Number of pseudo-random numbers to generate
subintervalLength = 0.1;       % Length of subintervals
numSubintervals = 2 / subintervalLength;   % Number of subintervals

% Generate pseudo-random numbers using Polar-Marsaglia method
randomNumbers = exppolarMarsaglia(N, 2.0);

% Count the number of generated numbers falling into each subinterval
counts = histcounts(randomNumbers, numSubintervals);

% Compute the relative frequencies divided by subinterval length
relativeFrequencies = counts / (N * subintervalLength);

% Compute the bin centers for plotting the histogram
binCenters = (subintervalLength/2) : subintervalLength : 2 - (subintervalLength/2);

% Plot the histogram of the relative frequencies
figure(2)
bar(binCenters, relativeFrequencies);
xlabel('Interval');
ylabel('Relative Frequency');
title('Histogram of Exponentially Distributed Pseudo Random Numbers');

% Compute the sample average and sample variance
sampleAverage = mean(randomNumbers);
sampleVariance = var(randomNumbers);

% Display the sample average and sample variance
disp(['Sample Average: ', num2str(sampleAverage)]);
disp(['Sample Variance: ', num2str(sampleVariance)]);

%%1.4.6

%% (PC-Exercise 1.4.10, p. 118)
lambda = 0.5;               % Parameter lambda for exponential distribution
n = 1E+3;          % Number of random numbers to generate
valuesOfA = [1, 2, 3, 4];   % Values of "a" for calculation

% Generate exponentially distributed random numbers using Polar-Marsaglia method
randomNumbers = exppolarMarsaglia(n, lambda);

% Calculate the average of the numbers for different values of "a"
for i = 1:length(valuesOfA)
    a = valuesOfA(i);
    average = mean(randomNumbers(randomNumbers >= a));
    disp(['Average for a = ', num2str(a), ': ', num2str(average)]);
end

%%1.4.12
%%1.4.13

%% PC-Exercise 4.8 (PC-Exercise 1.5.3, p. 24)

n= 1E+7;
 
%
X = rand(n,1);

%                                                                                                                                                                                                                                                      rating realizations of Z from N(0, 1/n)
Z_n = randn(n,1) ./sqrt(n);  


%compute Y_n
Y_n = X + Z_n;

    
%plot
histogram(Y_n, 'Normalization','pdf');
xlabel('Y_n');
ylabel('Density');
title('Density function estimate of Y');


%% PC-Exercise 4.9 (PC-Exercise 1.5.4, p. 25)
% Set the number of runs and values of n
numRuns = 1E+3;
nValues = [10, 100, 1000,1E+4];

% Compute the average A_n for each n
for n = nValues
    sum_AN = 0; % Accumulator for A_n
    
    % Perform multiple runs
    for i = 1:numRuns
        randomNumbers = polarMarsaglia(n); 
        Mean = mean(randomNumbers);
        sum_AN = sum_AN + Mean;
    end
    
    % Calculate the average A_n
    avg_AN = sum_AN / numRuns;
    
    % Display the results
    fprintf('n = %d, Average A_n: %.6f\n', n, avg_AN);
end

%%1.5.6

%% PC-Exercise 4.11 (PC-Exercise 1.5.7, p. 26)

n =1E+5; %number of steps in the walk

random_number = polarMarsaglia(n); 

%generating random walk
S_n = cumsum(2*(random_number >=0.5)-1);

%plot 
figure(4)
plot (1:n,S_n);
xlabel('n');
ylabel('S');
title('Random Walk');
grid on;


%%1.6.5
%%1.6.7

%% PC Exercise 4.15 (PC Exercise 1.8.2 , p.41)
% Parameters
N_values = 1E+4; % Values of N
T = 100; % Total time interval
dt = 0.01; % Time step

% Generate random numbers for N=1E+4
X = randn(1, N_values);

% Calculate sample path for N=100
t = 0:dt:(T-dt);
S_100 = cumsum(X) .* sqrt(dt);

% Plot sample path for N=100
figure;
plot(t, S_100);
%hold on;

% Generate and plot sample paths for increasing values of N
for N = N_values
    % Calculate sample path for current N
    tn = linspace(0, T, N+1);
    S_N = cumsum(X(1:N)) .* sqrt(diff(tn));
    plot(tn(2:end), S_N);
end

% Set labels and title
xlabel('t');
ylabel('S_N(t)');
title('Sample Path of S_N(t) for Increasing Values of N');
legend('N= 1000');
grid on;

%% PC-Exercise 4.16 (PC-Exercise 1.8.6, p. 42).


% Parameters
N_values = 500; % Values of N
T = 1; % Total time interval

% Generate random numbers for N=100
X = randn(max(N_values), T);

% Calculate and plot random walks for increasing N
for i = 1:length(N_values)
    N = N_values(i);
    t = linspace(0, T, N+1);
    S_N = cumsum(X(1:N, :));
   
    % Evaluate ratios for smaller values of h at t = 0.5
    h_values = [0.1, 0.05, 0.01, 0.001];
    ratios = zeros(size(h_values));
    for j = 1:length(h_values)
        h = h_values(j);
        idx = find(t >= 0.5, 1);
        ratios(j) = (S_N(idx+1) - S_N(idx)) / h;
    end
   
    % Plot ratios against h
    figure(6);
    plot(h_values, ratios, 'o-');
    title(['Ratios for N = ' num2str(N)]);
    xlabel('h');
    ylabel('Ratios');
end




