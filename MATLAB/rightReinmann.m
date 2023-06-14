% Ito sum
function right = rightReinmann(W)
right = 0;
for i = 2:length(W)-2
    right = right + W(i) * (W(i) - W(i-1));
end
end