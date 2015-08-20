set.seed(917)

f <- function(x){
    (2/sqrt(2*pi))*exp(-x^2/2)
}
g <- function(x){
    exp(-x)
}

plot(f, ylim=c(0,2), xlim=c(0,5))
plot(g, add=TRUE, col="red",  ylim=c(0,2), xlim=c(0,5))

h <- function(x){
    f(x)/g(x)
}
plot(h, xlim=c(0.1,4), ylim=c(0,2))

M <- 2


# ACEPTACIÓN RECHAZO

genera.una <- function(...){
    MAXITER = 10000
    continuar = TRUE
    iter <- 1
    while(continuar | iter <= MAXITER){
        # 1) -haz exponencial # # o usen rexp
        Y <- log(1/(1-runif(1))) # exponencial
        U <- runif(1) # unif
        # ----------------#
        # 2) acepto o rechazo
        if( U <= f(Y)/(M*g(Y))){
            X <- Y
            continuar = FALSE
        }
        iter <- iter + 1
    } 
    return(X)
}
genera.muchas <- function(n){
    sapply(1:n, FUN=genera.una)
}

vec <- genera.muchas(500)
hist(vec)
