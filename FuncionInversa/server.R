library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {
    
    data <- reactive({
        runif(input$n)
    })
    
    output$plot1 <- renderPlot({     
        
        hist(data(), main='Números aleatórios Uniformes', col = 'darkolivegreen3', border = 'white',
             ylab = 'Frecuencias', xlab = 'Datos')
    })
    
    
    output$plot2 <- renderPlot({
        data2 <- -log(data()) / input$lambda
        
        hist(data2, main='Números aleatórios Exponenciales', col = 'steelblue', border = 'white',
             ylab = 'Frecuencias', xlab = 'Datos')
    })
    
    
    # Generate a summary of the data
    output$summary1 <- renderPrint({
        summary(data())
    })
    
    
    output$summary2 <- renderPrint({
        summary(-log(data()) / input$lambda)
    })
    
    # Generate an HTML table view of the data
    output$table <- renderTable({
        data2 <- -log(data()) / input$lambda
        
        data.frame(Uniforme=data(), Exponencial=data2)
    })
    
    output$downloadData <- downloadHandler(
        filename = 'uniforme.csv',
        content = function(file) {
            write.csv(data(), file)
        }
    )
    
    
})