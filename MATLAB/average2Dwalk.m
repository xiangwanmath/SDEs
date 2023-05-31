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
allWalks = zeros(number_Walks, number_Steps+1, 2); % +1 to include the starting position

% Perform random walks
for walk = 1:number_Walks
    % Include starting position and initialize the current walk 
    currentWalk = zeros(number_Steps+1, 2);
   
    % Perform the random walk
    for step = 2:number_Steps+1
        % Generate a random step (-1 or +1) in x and y directions using a coin toss
        coin = randi(2,1,2)-1;
        new_step = 2 * coin - 1;
       
        % Update the position
        currentWalk(step, :) = currentWalk(step-1, :) + new_step;
    end
   
    % Store the current walk in the matrix
    allWalks(walk, :, :) = currentWalk;
end

% Calculate the average of all random walks
averageWalk = squeeze(mean(allWalks, 1)); %extract 2D from the 3D array using squeeze

% Plot the average walk
plot(averageWalk(:, 1), averageWalk(:, 2));
title('Average Random Walk in 2D');
xlabel('X-axis');
ylabel('Y-axis');
grid on;
axis equal;

%% 

%stop timer and display computation time
computationTime = toc;
disp(['Computation time: ' num2str(computationTime) ' seconds']);