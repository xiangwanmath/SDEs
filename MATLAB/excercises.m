clear
clc
clf
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









