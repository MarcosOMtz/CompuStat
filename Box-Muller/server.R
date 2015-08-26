library(shiny)
library(nortest)


# Define server logic for random distribution application
shinyServer(function(input, output) {
    
    data1 <- reactive({
        runif(input$n)
    })
    
    data2 <- reactive({
        runif(input$n)
    })
    
    x <- rep(0, length(data1))
    y <- rep(0, length(data2))
    
    output$plot1 <- renderPlot({

        for (i in 1:input$n){
            x[i] = sqrt(-2*log(data1()[i]))*cos(2*pi*data2()[i])
            y[i] = sqrt(-2*log(data1()[i]))*sin(2*pi*data2()[i])
        }
        
        plot(density(c(x,y)), main = 'Gráfica de densidad', col='blue', type = 'h')
        
    })

        
    output$prueba1 <- renderPrint({
        for (i in 1:input$n){
            x[i] = sqrt(-2*log(data1()[i]))*cos(2*pi*data2()[i])
            y[i] = sqrt(-2*log(data1()[i]))*sin(2*pi*data2()[i])
        }
        
        lillie.test(c(x,y))
    })
    
    
    output$table1 <- renderTable({
        head(data.frame(Uniforme_1=data1(), Uniforme_2=data2(), Normal_1=x, Normal_2=y),20)
    })
    
    
})