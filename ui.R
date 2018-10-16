library(shiny)
if (!require('shinythemes')) {install.packages('shinythemes')}
library(shinythemes)


shinyUI(fluidPage(theme = shinytheme("paper"),
                  
                  # Application title
                  titlePanel("RelDist -> Distribuciones de Confiabilidad"),
                  
                  
                  sidebarLayout(
                    sidebarPanel(
                      
                      selectInput(inputId="Distribucion",
                                  label="Elija la distribución: ",
                                  choices = c("Exponencial Weibull",
                                             "Flexible Weibull Extension"),
                                  selected="Exponencial Weibull"),
                      
                      HTML("Modifique los valores de los parámetros  
                           observe lo que sucede"),
                      
                      br(),
                      br(),
                      
                      sliderInput(inputId = "mu.link",
                                  label   = HTML("Elija el valor del parámetro de Escala &alpha;:"),
                                  min     = 0.1,
                                  max     = 10,
                                  value   = 2,
                                  step    = 0.1,
                                  animate = TRUE),
                      
                      sliderInput(inputId = "sigma.link",
                                  label   = HTML("Elija el valor del parámetro Forma &beta;:"),
                                  min     = 0.001,
                                  max     = 10,
                                  value   = 1.3,
                                  step    = 0.1,
                                  animate = TRUE),
                      
                      conditionalPanel(condition="input.Distribucion=='Exponencial Weibull'", 
                      
                      sliderInput(inputId = "nu.link",
                                  label   = HTML("Elija el valor del parámetro Localización &gamma;:"),
                                  min     = 0.001,
                                  max     = 10,
                                  value   = 3,
                                  step    = 0.1,
                                  animate = TRUE)
                      
                      ),
                      
                      sliderInput(inputId = "x",
                                  label   = "Elija el máximo valor de x:",
                                  min     = 0.001,
                                  max     = 30,
                                  value   = 7,
                                  step    = 1,
                                  animate = TRUE),
                      
                      br(),
                      
                      
                      h4("Librería RelDist:"),
                      h5(" Autores"),
                      h6(tags$p(tags$a(href="https://github.com/JohanMarin", "Johan D. Marin B."))),
                      h6(tags$p(tags$a(href="https://github.com/ousuga", "Olga C. Usuga M.")),
                      tags$p(tags$a(href="https://github.com/JohanMarin", "Carmen E. Patiño R.")),
                      tags$p(tags$a(href="https://github.com/fhernanb", "Freddy Hernandez B."))),
                      tags$p(tags$a(href="http://www.udea.edu.co/wps/portal/udea/web/inicio/investigacion/grupos-investigacion/ingenieria-tecnologia/incas", "Grupo INCAS,"), " Universidad de Antioquia"),
                      tags$p(tags$a(href="https://ciencias.medellin.unal.edu.co/escuelas/estadistica/herramientas.html", "Semillero de R,"), " Universidad Nacional de colombia Sede Medellìn"),
                      tags$img(src='logo-udea.png', height = 60, aling="center"),
                      tags$img(src='logo-unal.png', height = 60, aling="center"),
                      tags$p(tags$a(href="https://github.com/ousuga/RelDists", "GitHub"))
                      
                      ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                      h3(textOutput("titleDistribucion"), align = "center"),
                      plotOutput("graficos", width="550px", height="450px"),
                      verbatimTextOutput('med.var')
                    ))))
