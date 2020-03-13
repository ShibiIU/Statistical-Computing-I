mlecauchy=function(x, toler=.001){      #x is a vector here
    startvalue=median(x)
    n=length(x);
    thetahatcurr=startvalue;
    # Compute first deriviative of log likelihood
    firstderivll=2*sum((x-thetahatcurr)/(1+(x-thetahatcurr)^2))
    # Continue Newton’s method until the first derivative
    # of the likelihood is within toler of 0.001
    while(abs(firstderivll)>toler){
        # Compute second derivative of log likelihood
        secondderivll=2*sum(((x-thetahatcurr)^2-1)/(1+(x-thetahatcurr)^2)^2);
        # Newton’s method update of estimate of theta
        thetahatnew=thetahatcurr-firstderivll/secondderivll;
        thetahatcurr=thetahatnew;
        # Compute first derivative of log likelihood
        firstderivll=2*sum((x-thetahatcurr)/(1+(x-thetahatcurr)^2))
    }
    list(thetahat=thetahatcurr);
}


x1 = c(-2.09, -2.68, -1.92, -1.76, -2.12, 2.21, 1.97, 1.61, 1.99, 2.18)
theta0=1
mle=mlecauchy(x1, theta0, 0.0001)

print(mle)

