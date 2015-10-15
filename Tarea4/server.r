library(shiny)
library(car)
library(boot)
library(MASS)
ej <- read.csv(file='/Users/Marcos/Documents/Maestria_ITAM/3er_Semestre/Est_Comput/Tareas/Tarea04/Ejemplo.csv',
               header=T)



shinyServer(
  function(input, output){

      reg <- reactive({
          mod.duncan.hub <- rlm(prestige~income+education, data=Duncan, maxit=200)
      })
      
      
      reg.boot <- reactive({
          duncan.boot <- Boot(reg(), R=input$nsim)
      })
      
      
      output$summary1 <- renderPrint({
          reg <- lm(y~x, data=ej)
          coefficients(reg)
      })
      
      
      output$summary2 <- renderPrint({
          reg <- lm(y~x, data=ej)
          coefficients(reg)[2]/coefficients(reg)[1]
      })
      
      
      output$plot1 <- renderPlot({
          N <- nrow(ej) # Número de observaciones en la base de datos
          store.coef <- rep(0, N)
          
          for (i in 1:input$nsim){
              idx <- sample(1:N, N, replace = T)
              newdata.df <- ej[idx,]
              reg <- lm(y ~ x, data = newdata.df) # ~~> varY = a + b(varX) + e
              store.coef[i] <- coefficients(reg)[2]/coefficients(reg)[1]
          }
          
          hist(store.coef, breaks=25, col='gold', border = 'white',
               main = 'Histograma de estimaciones de theta', xlab = 'Thetas', ylab = 'Frecuencias')
          abline(v=-0.1850725, lwd=2, lty=2, col='blue')
          
      })
      
      
      output$summary3 <- renderPrint({
          summary(Duncan)
      })
      
      
      output$summary4 <- renderPrint({
          summary(reg())
      })      
      
      
      output$summary5 <- renderPrint({
          summary(reg.boot(), high.moments = T)
      })      
      
      
      output$summary6 <- renderPrint({
          confint(reg.boot(), parm = 2:3, level = c(0.68, 0.90, 0.95), type = 'perc')
      })
      
      
      output$plot2 <- renderPlot({
          hist(reg.boot(), legend = 'separate')
      })
      
      
    
  }
  )