# S610 Homework 6
# Shibi He


## Use Newton's method to compute a MLE of location parameter

MLE_Newton = function(theta0, x, epsilon){
    while (abs(dloglik_cauchy(theta0, x)) > epsilon ){
        theta1 = theta0 - dloglik_cauchy(theta0, x)/d2loglik_cauchy(theta0, x)
        theta0 = theta1
    }
    return(theta0)
}


loglik_cauchy = function(theta, x){
    sum(log(1/pi) + log(1/(1+(x-theta)^2)))
}

dloglik_cauchy = function(theta, x){
    sum(2*(x-theta)/(1+(x-theta)^2))
}

d2loglik_cauchy = function(theta, x){
    sum((2*(x-theta)^2-2)/(1+(x-theta)^2)^2)
}


# Compute maximum likelihood
epsilon1 = 0.000001
x1 = c(-2.09, -2.68, -1.92, -1.76, -2.12, 2.21, 1.97, 1.61, 1.99, 2.18)
theta_vec = c(-2, -1, 0, 1, 2)

# The MLE of theta:
theta.MLE = sapply(theta_vec, MLE_Newton, x1, epsilon1)
print(theta.MLE)

# The maximum log-likelihood:
max_loglik = sapply(theta.MLE, loglik_cauchy, x1)
print(max_loglik)

# The maximum likelihood:
max_likelihood = exp(max_loglik)
print(max_likelihood)


# Plot log-likelihood function 
# theta=seq(-3, 3, 0.01)
# plot(theta, sapply(theta, loglik_cauchy, x1))




## One-step estimation:
OneStep = function(x){
    theta0 = median(x)
    theta1 = theta0 - dloglik_cauchy(theta0, x)/d2loglik_cauchy(theta0, x)
    return(theta1)
}


## Investigate estimator efficiency:

# Modify MLE using Newton's method so that it uses the median as starting value
MLE_Newton2 = function(x, epsilon){
    theta0 = median(x)
    while (abs(dloglik_cauchy(theta0, x)) > epsilon ){
        theta1 = theta0 - dloglik_cauchy(theta0, x)/d2loglik_cauchy(theta0, x)
        theta0 = theta1
    }
    return(theta0)
}


# Draw randome sample from Cauchy distribution:
set.seed(610)
X1 = rcauchy(10, location=0, scale=1)
X2 = rcauchy(100, location=0, scale=1)
X3 = rcauchy(1000, location=0, scale=1)

sample = list(X1, X2, X3)


# Compute MLE using Newton's method:
theta.MLE = sapply(sample, MLE_Newton2, epsilon1)
print(theta.MLE)


# Compute one-step estimate:
theta.onestep = sapply(sample, OneStep)
print(theta.onestep)



# Repeat 500 times

get.estimate = function(i){
    X1 = rcauchy(10, location=0, scale=1)
    X2 = rcauchy(100, location=0, scale=1)
    X3 = rcauchy(1000, location=0, scale=1)
    sample = list(X1, X2, X3)
    theta.MLE = sapply(sample, MLE_Newton2, epsilon1)
    theta.onestep = sapply(sample, OneStep)
    estimate = c(theta.MLE, theta.onestep)
    return(estimate)
}

result <- t(sapply(1:500, get.estimate))
colnames(result) = c("MLE10", "MLE100", "MLE1000", "Onestep10", "Onestep100", "Onestep1000")

# compute variance of the MLE estimator and one-step estimator
variance = apply(result, 2, var)
print(variance)














