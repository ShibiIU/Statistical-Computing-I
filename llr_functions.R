
# llr function


llr = function(x, y, z, omega) {
    fits = sapply(z, compute_f_hat, x, y, omega)
    return(fits)
}


multiply_fun=function(vec1, vec2){
    product=vec1*vec2
    return(product)
}



compute_f_hat = function(z, x, y, omega) {
    Wz = make_weight_matrix(z, x, omega)
    X = make_predictor_matrix(x)
    f_hat = c(1, z) %*% solve(t(X) %*% sweep(X, 1, diag(Wz), "*")) %*% t(X) %*% (diag(Wz)*y)
    return(f_hat)
}



make_weight_matrix = function(z, x, omega) {
    r = abs(x - z) / omega  # this is a vector of the same length as x
    w = sapply(r, W)  # this is a vector of the same length as x and r
    Wz = diag(w) # this is a diagonal matrix with elements from w
    return(Wz)
}


W = function(r) {
    if (abs(r) < 1) {
        return((1 - abs(r) ** 3) ** 3)
    } else {
        return(0)
    }
}



make_predictor_matrix = function(x) {
    n = length(x)
    return(cbind(rep(1, n), x))
}



