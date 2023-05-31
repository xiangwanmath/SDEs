clear
clc
clf
%% 
%start timer to measure computation time
tic;


%% 

% Parameters
number_Steps = 100; % Number of steps in each random walk
number_Walks = 1000; % Number of random walks

% Initialize matrix to store all random walks
totalWalk = zeros(number_Walks, number_Steps+1); % +1 to include the starting position

% Perform random walks
for walk = 1:number_Walks
    % Initialize the current walk
    position = zeros(number_Steps+1, 1);
   
    % Perform the random walk
    for step = 2:number_Steps+1
        % Generate a random step (-1 or +1) in x and y directions using a coin toss
        coinToss = randi(2) - 1; %gives random integers, either one or two
        new_step = 2 * coinToss - 1; %moves forward for heads and moves backward for tails
       
        % Update the position
        position(step) = position(step-1) + new_step;
    end
   
    % Store the current walk in the matrix
    totalWalk(walk, :) = position;
end

% Calculate the average of all random walks
averageWalk = mean(totalWalk);

% Plot the average walk
plot(0:number_Steps, averageWalk);
title('Average Random Walk (1D)');
xlabel('Steps');
ylabel('position');
grid on;
axis on;

%% %stop timer and display computation time
computationTime = toc;
disp(['Computation time: ' num2str(computationTime) ' seconds']);
