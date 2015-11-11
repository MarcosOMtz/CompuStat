library(Rcpp)
library(ggplot2)
library(dplyr)
library(calibrate)

# Tarea: Hacer ejemplo de regresión lineal con IRIS. Quitar especie y hacer la estimación
# de los parámetros

# FREQUENTIST
mod <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
summary(mod)
plot(mod)

# A MANITA
N <- nrow(iris)
X <- as.matrix(cbind(1, iris[ ,2:4]))
Y <- iris[ ,1]
beta.hat <- solve(t(X) %*% X, t(X) %*% Y)
pred <- X %*% beta.hat
residuals <- Y-pred
hist(residuals, breaks=20)
qqnorm(residuals)
plot(residuals, pred)
plot(Y, pred)
SS <- sqrt(sum((Y-pred)^2)/(150-4))
cov.betas <- (SS^2)*solve(t(X) %*% X)
sqrt(diag(cov.betas))


# BAYESIANO
# La apriori que me de la gana
prior.beta <- function(x) dnorm(x, 0, 100)
plot(prior.beta, col="darkblue", xlim=c(-100,100), lwd="2", main="Prior for mean", ylab="density")

prior.sigma <- function(x) dgamma(x, 5, 0.5)
plot(prior.sigma, col="darkblue", xlim=c(0,100), lwd="2", main="Prior for mean", ylab="density")


# Funcion objetivo VEROSIMILITUD*APRIORI en logaritmos
cppFunction('
            double objdens(NumericMatrix X, NumericVector y, NumericVector theta, double sigma){
            double lkh, logprior, yhat;
            int m=X.nrow(), p=X.ncol();
            NumericVector aux(m);
            // Compute loglikelihood
            lkh=0;
            for (int i=0; i<m; i++){
              aux = X(i,_)*theta;
              yhat = std::accumulate(aux.begin(), aux.end(), 0.0);
              lkh += -.5/pow(sigma,2)*pow(y[i] - yhat,2);
            }
            // Compute logprior
            logprior = 0.0;
            for(int j=0; j<p; j++){
              logprior += R::dnorm(theta[j], 0.0, 100, true); // Aquí la inicial!!
            }
            logprior += R::dgamma(sigma, 5.0, 0.01, true);
            // Log of target density
            return lkh + logprior;
}')
x <- iris[c('Sepal.Width','Petal.Length','Petal.Width')]
objdens(as.matrix(x), Y, 1:3, 1)

# PROPUESTA: Caminata aleatoria con brinco automatico
cppFunction('
            NumericVector proposal(NumericVector theta, double sigma){
            int nparam = theta.size();
            double jump = 0.1; 
            NumericVector newtheta(nparam+1);
            for (int i=0; i<nparam; i++){
              newtheta[i] = R::rnorm(theta[i], jump);
            }
            newtheta[nparam] = R::rnorm(sigma, jump);
            if(newtheta[nparam] <= 0){
              newtheta[nparam] = 0.0001;
            }
            return newtheta;
}')
proposal(c(0,0,0), 1)

# Cargar la funcion
sourceCpp('/Users/Marcos/Documents/Maestria_ITAM/3er_Semestre/Est_Comput/Tareas/Tarea05/BayesianMH_Tarea5.cpp')

nsim <- 10000
init <- c(0,0,0,0) # Take intercept into account, tambien puede ser ncol(X)+1
sigma_init <- 1.1
X <- cbind(1, as.matrix(x))
mh.samp <- MHBayesLinReg(nsim, init, sigma_init, objdens, proposal, X, Y) # 1 for intercept
estims <- mh.samp$theta
estims_sigma <- mh.samp$sigma
str(mh.samp)


# DIAGNOSTICO
### 1) Exploracion de parametros
pts <- seq(1, nrow(estims), by=5)
plot(estims[pts, ], type = 'l', asp = 1)
cor(estims)

### 2) Aceptacion
rejections <- mh.samp$rejection[-1]
trials <- rejections + 1
acc.rate <- 1-cumsum(rejections)/cumsum(trials)
plot(100*acc.rate, type='l', ylim=c(0,100), main='Acceptance rate', ylab='%') # 29% ideal tasa de rechazo

### 3) Autocorrelacion
par(mfrow=c(2,3))
for(i in 1:ncol(estims)){
    acf(estims[, i], main=paste('theta',i))
}
acf(estims_sigma)


### 4) BURNIN Y ADELGAZAMIENTO
### Burnin and subsampling
burnin <- round(0.2*nsim) # Le quito el 20% (aceptables 5-20)
estims <- estims[-(1:burnin), ]
estims_sigma <- estims_sigma[-(1:burnin)]
thinning <- 15
pts <- seq(1, nsim-burnin, by=thinning)
estims <- estims[pts, ]
estims_sigma <- estims_sigma[pts]
par(mfrow=c(2,3))
for(i in 1:ncol(estims)){
    acf(estims[, i])
}
acf(estims_sigma)
par(mfrow=c(1,1))


# ESTIMADORES BAYESIANOS (pointwise estimates)
beta_estim <- apply(estims, 2, mean)
beta_sd <- apply(estims, 2, sd)
sigma_estim <- mean(estims_sigma)



# COMPARAMOS RESULTADOS
summary(mod)$coefficients
