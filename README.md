# final_project_group2

<!-- badges: start -->
<!-- badges: end -->

## Overview

`finalprojectgroup2` implements a **Stepwise Model Selection with Multiple Paths** procedure. 
Instead of selecting a single "best" model, this package explores multiple promising 
model paths using AIC and validates them through resampling-based stability selection.

## Installation

```
remotes::install_github(R-4-Data-Science/finalprojectgroup2)
```

## Linear Usage Example
```
set.seed(1)
# Simulate a linear regression dataset
n <- 120; p <- 8
X <- matrix(rnorm(n * p), n, p)
beta <- c(2, -1.5, 0, 0, 1, rep(0, p - 5))
y <- as.numeric(X %*% beta + rnorm(n, sd = 1))
colnames(X) <- paste0("x", 1:p)
```

### 1. Multi-path forward selection on the full data
```
forest <- build_paths(
  X      = X,
  y      = y,
  family = "gaussian",
  K      = min(p, 10),
  eps    = 1e-6,
  delta  = 1,
  L      = 50
)
```
### 2. Stability via bootstrap resampling
```
stab <- stability(
  X             = X,
  y             = y,
  family        = "gaussian",
  B             = 50,
  resample_type = "bootstrap",
  K             = min(p, 10),
  eps           = 1e-6,
  delta         = 1,
  L             = 50
)
```
### 3. Plausible models: low AIC and high average stability
```
plaus <- plausible_models(
  forest = forest,
  stab   = stab,
  Delta  = 2,
  tau    = 0.6
)
```

## Binomal Usage

```
set.seed(2)

## Simulate a logistic regression dataset
n <- 200
p <- 8

X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("x", 1:p)

beta <- c(1.5, -1.0, 0.8, 0, 0, 0, 0, 0)
eta  <- X %*% beta
prob <- 1 / (1 + exp(-eta))  # plogis(eta)

y <- rbinom(n, size = 1, prob = prob)
```

### 1. Multi-path forward selection on full data
``` 
forest <- build_paths(
  X      = X,
  y      = y,
  family = "binomial",
  K      = min(p, 10),
  eps    = 1e-6,
  delta  = 1,
  L      = 50
)
```

### 2. Stability via bootstrap resampling
```
stab <- stability(
  X             = X,
  y             = y,
  family        = "binomial",
  B             = 30,        
  resample_type = "bootstrap",
  K             = min(p, 10),
  eps           = 1e-6,
  delta         = 1,
  L             = 50
)
```

### 3. Plausible models: AIC window + stability threshold
```
plaus <- plausible_models(
  forest = forest,
  stab   = stab,
  Delta  = 2,
  tau    = 0.6
)
```