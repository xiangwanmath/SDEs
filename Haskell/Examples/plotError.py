import matplotlib.pyplot as plt
import numpy as np
import re
import os

# Read data from the file
script_dir = os.path.dirname(__file__) 
rel_path = "data/emError.txt"
file_path = os.path.join(script_dir, rel_path)

with open(file_path, "r") as file:
    lines = file.readlines()

# Parse the 'h' range
range_match = re.search(r'h Range: (\d+)-(\d+)', lines[2])
if range_match:
    min_h = int(range_match.group(1))
    max_h = int(range_match.group(2))
else:
    raise ValueError("Failed to parse 'h' range from the input string.")

# Parse error data
weak_error_line = lines[5].strip("[]\n")
strong_error_line = lines[8].strip("[]\n")

weak_error = [float(val) for val in weak_error_line.split(",")]
strong_error = [float(val) for val in strong_error_line.split(",")]

h_values = np.linspace(min_h, max_h, len(weak_error))

# Plot data
plt.figure(figsize=(10, 6))
plt.loglog(h_values, weak_error, label="Weak Error")
plt.loglog(h_values, strong_error, label="Strong Error")

# Fit regression lines
degree = 1  # linear regression
weak_coeff = np.polyfit(np.log10(h_values), np.log10(weak_error), degree)
strong_coeff = np.polyfit(np.log10(h_values), np.log10(strong_error), degree)

# Extract slopes
weak_slope = weak_coeff[0]
strong_slope = strong_coeff[0]

# Create regression lines
weak_reg_line = np.polyval(weak_coeff, np.log10(h_values))
strong_reg_line = np.polyval(strong_coeff, np.log10(h_values))

# Plot regression lines
plt.loglog(h_values, 10**weak_reg_line, label=f"Weak Error Order (Slope: {weak_slope:.2f})", linestyle='dashed')
plt.loglog(h_values, 10**strong_reg_line, label=f"Strong Error Order (Slope: {strong_slope:.2f})", linestyle='dashed')

plt.xlabel("h")
plt.ylabel("error")
plt.legend()
plt.grid(True)

plt.savefig("emErrorPlot.png")
plt.show()
