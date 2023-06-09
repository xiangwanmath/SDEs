%for discrete random number generation
function x = discreteRand(states, probabilities)
    cumulativeProbs = cumsum(probabilities);
    r = rand();
    x = states(find(cumulativeProbs >= r, 1));
end