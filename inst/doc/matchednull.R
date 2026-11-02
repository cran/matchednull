## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
has_mclust <- requireNamespace("mclust", quietly = TRUE)

## -----------------------------------------------------------------------------
library(matchednull)

set.seed(1)
x <- matrix(rnorm(400 * 3), 400, 3) %*%
  chol(matrix(c(1, .5, .3, .5, 1, .4, .3, .4, 1), 3, 3))
twin <- copula_null(x)

all(sort(twin[, 1]) == sort(x[, 1]))   # margins: identical
round(cor(x) - cor(twin), 2)           # correlations: close, not exact

## ----eval = has_mclust--------------------------------------------------------
suppressPackageStartupMessages(library(mclust))
pick_k <- function(d) Mclust(d, G = 1:4, modelNames = "VVV", verbose = FALSE)$G

# 1. Typeless data: the test should stay quiet.
set.seed(7)
matched_null_test(x, pick_k, R = 30)

## ----eval = has_mclust--------------------------------------------------------
# 2. Two genuine types, hidden in the dependence structure
#    (identical margins, opposite correlation orientation).
set.seed(42)
z <- sample(2, 400, replace = TRUE)
X <- matrix(rnorm(400 * 4), 400, 4)
L1 <- chol(matrix(c(1, .85, .85, 1), 2, 2))
L2 <- chol(matrix(c(1, -.85, -.85, 1), 2, 2))
X[z == 1, 1:2] <- X[z == 1, 1:2] %*% L1
X[z == 1, 3:4] <- X[z == 1, 3:4] %*% L1
X[z == 2, 1:2] <- X[z == 2, 1:2] %*% L2
X[z == 2, 3:4] <- X[z == 2, 3:4] %*% L2

set.seed(7)
matched_null_test(X, pick_k, R = 30)

## ----eval = FALSE-------------------------------------------------------------
# matched_null_test(x, pick_k, R = 200, copula = "t", df = 8)  # moderate tails
# matched_null_test(x, pick_k, R = 200, copula = "t", df = 3)  # heavy tails

