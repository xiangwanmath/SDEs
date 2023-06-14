% Stratonovich sum
function stratonovich = stratonovichIntegral(W)
stratonovich = 0;
for i = 1:length(W)-1
    stratonovich = stratonovich + ((W(i) + W(i+1)) / 2) * (W(i+1) - W(i));
end
end