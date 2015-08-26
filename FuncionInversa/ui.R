library(shiny)

shinyUI(fluidPage(
    
    titlePanel("Estadística computacional; Tarea 01"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("n", "Número de observaciones:", 
                        value = 500,
                        min = 1,
                        max = 1000),
            br(),
            
            numericInput('lambda', label = 'Párametro lambda (exponencial):', 0.5,
                         min = 0.1, max = 5, step = 0.1),
            br(),
            
            downloadButton('downloadData', 'Download')),
        
        mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Gráficas", p('Para generar los números aleatórios exponenciales derivado de números aleatórios con distribución Uniforme (X), se debe hacer la siguiente transformación:'),
                                 p(withMathJax('$$Y = \\frac{-ln(X)}{\\lambda}$$')),
                                 plotOutput("plot1"), plotOutput('plot2')), 
                        tabPanel("Resumen", br(), h5(p('Datos con distribución Uniforme.')),
                                 verbatimTextOutput("summary1"), br(),
                                 h5(p('Datos con distribución Exponencial')),
                                 verbatimTextOutput("summary2")), 
                        tabPanel("Tabla de valores", tableOutput("table"))
            )
        )
    )
))