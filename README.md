# asympDiag

![badge](https://github.com/Alvaro-Kothe/asympDiag/actions/workflows/R-CMD-check.yaml/badge.svg)

The `asympDiag` package provides tools for diagnosing statistical
regression models using Monte Carlo simulations.
It helps assess the adequacy of asymptotic approximations for Wald tests and
verify residuals with envelopes.

## Installation

You can install the development version of asympDiag from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("Alvaro-Kothe/asympDiag")
```

## Example

Below is an example using `envelope()` to create a residual diagnostic plot
based on Monte Carlo simulations.

```r
library(asympDiag)

# Example data
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9)
treatment <- gl(3, 3)

# Fitting a Poisson regression model
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())

# Creating an envelope plot for residual diagnostics
envelope(glm.D93)
```

![envelope](https://github.com/user-attachments/assets/edab6e53-4114-4d51-9236-b9bc56ecf1a0)

The `envelope()` function generates an envelope plot that compares observed
residuals to those expected under the model, helping to identify potential
model misspecifications.

Next, after fitting a valid model, you can use `simulate_wald_pvalues()` to generate
p-values through simulation and check if the sample size is sufficient for the
Wald test's asymptotic approximation:

```r
# Simulating p-values to assess asymptotic approximation validity
simulate_wald_pvalues(glm.D93, nsim = 10000)
```

![pvalues](https://github.com/user-attachments/assets/3310ad5d-4b20-4265-bbfb-ca1e70cdac0d)

The function `simulate_wald_pvalues()` provides a distribution of p-values from the Monte
Carlo simulation, enabling a deeper assessment of whether the sample size
supports reliable inference based on the Wald test.
