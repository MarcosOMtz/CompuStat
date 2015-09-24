library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(parallel)

mc.intervals <- function(phi, N, rg=runif, fg=dunif, alpha=0.05, mc.cores=4){

    results <- mclapply(mc.cores=mc.cores, N, function(nsim){
        x <- rg(nsim)
        gx <- sapply(x, fg)
        phix <- sapply(x, phi)
        estim <- mean(phix/gx)
        s2 <- var(phix/gx)
        quant <- qnorm(alpha/2, lower.tail = F)
        int_upper <- estim + sqrt(s2/nsim)*quant
        int_lower <- estim - sqrt(s2/nsim)*quant
        salida <- data.frame(N=nsim, Estimacion=estim, Lim_Inferior=int_lower, Lim_Superior=int_upper)
        return(salida)
    })
    return(ldply(results) %>% mutate(i = row_number()))
}

f   <- function(x) dunif(x,-1,1)
rf  <- function(n) runif(n,-1,1)
g1  <- function(x) dnorm(x,0,1)
rg1 <- function(n) rnorm(n,0,1)
g2  <- function(x) dnorm(x,0,3)
rg2 <- function(n) rnorm(n,0,3)
g3  <- function(x) dnorm(x,-1,1)
rg3 <- function(n) rnorm(n,-1,1)

gs <- list(list(f,rf),list(g1,rg1),list(g2,rg2),list(g3,rg3))

shinyServer(function(input, output, session){
    
    N <- reactive(rep(input$n, input$N))
    mc <- reactive({
        phi <- function(x){
            input$k / (1 + abs(x)^input$m)
        }
        phif <- function(x) f(x)*phi(x)
        i <- as.numeric(input$g1)
        g <- gs[[i]]
        mc.intervals(phif, N(), g[[2]], g[[1]], alpha = 0.05, mc.cores = 4)
    })
    output$mc_data <- renderTable(mc())
    output$mc_plot <- renderPlot(
        ggplot(mc(), aes(i, Estimacion)) +
            geom_line() +
            geom_ribbon(aes(ymin=Lim_Inferior, ymax=Lim_Superior))
    )
    
    is <- reactive({
        phi <- function(x){
            input$k / (1 + abs(x)^input$m)
        }
        phif <- function(x) f(x)*phi(x)
        i <- as.numeric(input$g2)
        g <- gs[[i]]
        mc.intervals(phif, N(), g[[2]], g[[1]], alpha = 0.05, mc.cores = 4)
    })
    output$is_data <- renderDataTable(is())
    output$is_plot <- renderPlot(
        ggplot(is(), aes(i, Estimacion)) +
            geom_line() +
            geom_ribbon(aes(ymin=Lim_Inferior, ymax=Lim_Superior))
    )
    
    output$hist <- renderPlot({
        dat <- rbind(cbind(method='x ~ g1',mc()), cbind(method='x ~ g2',is()))
        ggplot(dat, aes(Estimacion, fill=method, color=method)) +
            geom_density(alpha=0.5) + theme_bw() + theme(legend.position="bottom") +
            scale_fill_manual(values=c("red", "steelblue")) + scale_color_manual(values=c("red", "steelblue"))
    })
    output$func <- renderPlot({
        phi <- function(x){
            input$k / (1 + abs(x)^input$m)
        }
        phif <- function(x) f(x)*phi(x)
        gg1 <- gs[[as.numeric(input$g1)]][[1]]
        gg2 <- gs[[as.numeric(input$g2)]][[1]]
        par(mfrow=c(2,2))
        plot(gg1, xlim=c(-2,2))
        plot(gg2, xlim=c(-2,2))
        plot(phi, xlim=c(-2,2))
        plot(phif, xlim=c(-2,2))
        par(mfrow=c(1,1))
    })
    
})
