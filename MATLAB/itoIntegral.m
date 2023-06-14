% Ito sum
function ito = itoIntegral(W)
ito = 0;
for i = 1:length(W)-1
    ito = ito + W(i) * (W(i+1) - W(i));
end
end