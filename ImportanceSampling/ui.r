library(shiny)

shinyUI(function(input, output){
    fluidPage(
        titlePanel('CompuStat :: Tarea 03 :: Importance Sampling'),
        sidebarPanel(
            helpText(h5('Alumno: Marcos Olguín Martínez, ITAM 2015')),
            hr(),
            sliderInput('N', 'Número de simulaciones',
                        value = 50, min = 1, max = 1000, step = 1),
            sliderInput('n', 'Número de puntos por simulación',
                        value = 100, min = 1, max = 1000, step = 1),
            sliderInput('k', 'Numerador de phi', value = 10,
                        min = 1, max = 100000, step = 1),
            sliderInput('m', 'Exponente de phi', value = 10,
                        min = 1, max = 10, step = 1),
            selectInput('g1', 'Función de muestreo g1', c('f [Unif(-1,1)]'=1,'Normal(0,1)'=2,'Normal(0,3)'=3,
                                                          'N(-1,1)'=4), selected = 1),
            selectInput('g2', 'Función de muestreo g2', c('f [Unif(-1,1)]'=1,'Normal(0,1)'=2,'Normal(0,3)'=3,
                                                          'N(-1,1)'=4), selected = 2)
            #actionButton('reset_input', 'Reset input')
        ),
        mainPanel(
            tabsetPanel(
                tabPanel('Gráficas',
                         p(withMathJax('$$\\int_{-1}^1 \\phi(x) dx$$')),
                         p(withMathJax('$$\\text{donde: } \\phi(x) = \\frac{k}{1 + |x|^m}$$')),
                         p(h5('Histograma')),
                         plotOutput('hist', width = '6.4in', height = '5in'),
                         br(),
                         p(h5('Funciones')),
                         plotOutput('func', width = '6.4in', height = '5in')),
                tabPanel('Datos', br(), tableOutput('mc_data'))
            )
        )
    )
})