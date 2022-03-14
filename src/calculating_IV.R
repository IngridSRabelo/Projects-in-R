install.packages("AER")
install.packages("stargazer")
library("AER")
library("stargazer")

data("CigarettesSW")
summary(CigarettesSW)
data85 <- subset(CigarettesSW, year == "1985")

# Creating variables
y <- data85$packs
x <- (data85$price / data85$cpi)
z <- ((data85$taxs - data85$tax) / data85$cpi) # instrumental variable

I <- matrix(rep(1, 48), 48, 1)
X <- as.matrix(cbind(I, x))
Y <- as.matrix(cbind(y))
Z <- as.matrix(cbind(I, z))

# Calculating the b_iv estimator
b_iv <- (solve(t(Z) %*% X) %*% (t(Z) %*% Y))
b_iv

# Checking
reg_iv <- ivreg(Y ~ X | Z)
reg_iv

# The results are the same, so let's calculate the variance
u <- as.matrix(reg_iv$residuals)
u2 <- t(u) %*% u
sigma <- as.matrix(mean(u2 / (46)))
sigma
var_iv <- as.vector(sigma) * (solve(t(Z) %*% X)) %*% (t(Z) %*% Z) %*% (solve(t(X) %*% Z))
var_iv

# Checking
vcov(reg_iv)
