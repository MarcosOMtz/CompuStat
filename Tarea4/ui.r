#getwd()
#setwd()
library(shiny)

shinyUI(fluidPage(
  
  titlePanel(title='Tarea4 :: Bootstrapping Regression'),
  
  sidebarLayout(
      sidebarPanel(
          img(src='logo_itam.png', height=48, width=141),
          helpText(h5('Alumno: Marcos Olguín Martínez, ITAM 2015')),
          hr(),
          sliderInput("nsim", "Número de simulaciones:", min = 100, max = 2000, value = 300, step = 100)
      ),
      
      mainPanel(
          tabsetPanel(type='tabs',
                      tabPanel('Ejemplo 1', br(),
                               p('Retomando el ejemplo de la corrosión que viene en esta tarea, donde tenemos
                                 13 medidas de pérdida de corrosión (y) en aleaciones de cobre y niquel, cada
                                 una con un contenido específico de hierro (x). De interés es el cambio en la
                                 pérdida de la corrosión en las aleaciones a medida que aumenta el contenido
                                 de hierro, con relación a la pérdida de la corrosión cuando no hay hierro.
                                 Por lo tanto, considerar la estimación en una regresión lineal simple de:'),
                               p(withMathJax('$$\\theta = \\frac{\\beta_{1}}{\\beta_{0}}$$')),
                               p('Los coeficientes estimados de la regresión original son los siguiente:'),
                               verbatimTextOutput("summary1"),
                               p('Donde el valor de theta es:'),
                               verbatimTextOutput("summary2"),
                               p('Estimaciones bootstrap de "theta":'),
                               plotOutput('plot1'),
                               p('La línea punteada es el valor original de theta.')
                               ),
                      tabPanel('Ejemplo 2', br(),
                               p('En este ejemplo se realiza la regesión del prestigio en el ingreso y la
                                 educación de 45 ocupaciones. Aquí se aplica la función "rlm" (paquete MASS).'),
                               p('Aquí una descripción de la base "Duncan"'),
                               verbatimTextOutput("summary3"),
                               p('La salida de la función "rlm" es la siguiente:'),
                               verbatimTextOutput("summary4"),
                               p('Los coeficientes errores estándar reportados por “rlm” se basan en
                                 aproximaciones asintóticas, y pueden no ser dignos de confianza en una muestra
                                 de tamaño 45. Volviendo, pues, al bootstrap. Hemos ajustado el argumento maxit
                                 en “rlm” en previsión del bootstrap, ya que algunas de las muestras bootstrap
                                 pueden necesitar más iteraciones para converger. Hay dos formas generales para
                                 el bootstrap en una regresión como esta: Podemos tratar los predictores como
                                 aleatorios, que podrían cambiar de una muestra a otra, o tenerlos fijos.'),
                               p('Ampliando el alcance de la discusión, suponemos que queremos ajustar un modelo
                                 de regresión con variable respuesta y y los predictores x1, x2, …, xk. Tenemos
                                 una muestra de n observaciones zi=(yi, xi1, xi2, …, xik), donde i=1,…,n. En el
                                 caso del remuestreo, simplemente elegimos R muestras bootstrap. Este es el
                                 método por defecto utilizado por la función “Boot” en “car”.'),
                               p('La función Boot, regresa un objeto de la clase "Boot", aquí mostramos el
                                 resultado:'),
                               verbatimTextOutput("summary5"),
                               p('Analizando el resultado, tenemos que da los valores de la muestra original
                                 para cada componente de los estadìstico con bootstrap, aunado con la estimación
                                 bootstrap del sesgo. el cual es la diferencia entre el promedio de los valores
                                 bootstrap y los valores de la muestra original. La estimación bootstrap del
                                 error estándar fueron calculados como la desviación estándar de las réplicas
                                 boostrap.'),
                               p('Aquí se dan varios intervalos de confianza que fueron calculados con el método
                                 de los percentiles y también se obtienen los cuantiles para distintos
                                 intervalos simultáneamente.'),
                               p('Finalmente, se dan los histogramas para cada coeficiente bootstrapeado.'),
                               verbatimTextOutput("summary6"),
                               plotOutput('plot2')))
                
      )
    
    )
  )
  
)











