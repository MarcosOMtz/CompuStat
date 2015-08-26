library(shiny)

shinyUI(fluidPage(
    
    titlePanel("CompuStat :: Tarea 02 :: Box Muller"),
    
    sidebarLayout(
        sidebarPanel(
            helpText(h5('Alumno: Marcos Olguín Martínez, ITAM 2015')),
            hr(),
            selectInput("n", "Número de simulaciones:", 
                        choices = c(50,500,1000,2000,2500,4000),
                        selected = 50)
            ),
        
        mainPanel(
            p('Gráfica para identificar si la transformación realizada sigue una distribución normal, considerando la siguiente transformación:'),
            br(), p(withMathJax('$$Z_{0} = R * cos({\\Theta}) = \\sqrt{-2 log(U_{1})} * cos(\\pi U_{2})$$')),
            p(withMathJax('$$Z_{1} = R * sin({\\Theta}) = \\sqrt{-2 log(U_{1})} * sin(\\pi U_{2})$$')),
            br(),
            p('Siendo que U1 y U2 tienen distribución Uniforme (0,1)'),
            plotOutput("plot1"),
            verbatimTextOutput("prueba1")
            
            )
    )
))