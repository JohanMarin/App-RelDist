library(shiny)
if (!require('shinythemes')) {install.packages('shinythemes')}
library(shinythemes)


shinyUI(fluidPage(theme = shinytheme("paper"),
                  
                  # Application title
                    sidebarLayout(
                    sidebarPanel(
                      tags$img(src='RelDists4.3_gris.png', height = 150, aling="center"),
                      
                      br(),
                      br(),
                      
                      h6("En este paquete están disponibles varias distribuciones para el análisis de confiabilidad."),
                      
                      br(),
                      br(),
                      
                      selectInput(inputId="Distribucion",
                                  label="Elija la distribución: ",
                                  choices = c("Exponencial Weibull",
                                             "Flexible Weibull Extension",
                                             "Weibull Modificada Generalizada",
                                             "Power Weibull Generalizada",
                                             "Weibull Inversa",
                                             "Kumaraswamy Weibull Modificada",
                                             "Kumaraswamy Weibull",
                                             "Log-weibull",
                                             "Weibull Modificada",
                                             "Weibull Extension Modificada",
                                             "Weibull Modificada de Almaki y Yuan",
                                             "Weibull Extraña",
                                             "Power Lindley",
                                             "Weibull Reflejada",
                                             "Weibull Modificada de Sarhan y Zaindins",
                                             "Geométrica de Weibull"),
                                  selected="Exponencial Weibull"),
                      
                      x1 <- c("Flexible Weibull Extension",
                             "Weibull Inversa", 
                             "Log-weibull",
                             "Weibull Modificada de Almaki y Yuan",
                             "Power Lindley",
                             "Weibull Reflejada",
                             "Exponencial Weibull",
                             "Power Weibull Generalizada",
                             "Weibull Inversa", 
                             "Log-weibull",
                             "Weibull Modificada",
                             "Weibull Extension Modificada",
                             "Weibull Extraña",
                             "Weibull Modificada de Sarhan y Zaindins",
                             "Geométrica de Weibull",
                             "Weibull Modificada Generalizada",
                             "Kumaraswamy Weibull "
                            ),
                      
                      x2 <- c("Exponencial Weibull",
                              "Power Weibull Generalizada",
                              "Weibull Inversa", 
                              "Log-weibull",
                              "Weibull Modificada",
                              "Weibull Extension Modificada",
                              "Weibull Extraña",
                              "Weibull Modificada de Sarhan y Zaindins",
                              "Weibull Modificada Generalizada",
                              "Kumaraswamy Weibull ",
                              "Kumaraswamy Weibull Modificada",
                              "Weibull Modificada de Almaki y Yuan"
                            ),

                      x3 <- c("Weibull Modificada Generalizada",
                              "Kumaraswamy Weibull ",
                              "Weibull Modificada de Almaki y Yuan"
                            ),
                      
                      x4 <- c("Kumaraswamy Weibull Modificada",
                              "Weibull Modificada de Almaki y Yuan"
                      ),
                      
                      HTML("Modifique los valores de los parámetros  
                           observe lo que sucede"),
                      
                      br(),
                      br(),
                      
                      #conditionalPanel(condition="input.Distribucion %in% x1",
                      
                      sliderInput(inputId = "mu.link" ,
                                  label   = HTML("Elija el valor del parámetro de Escala &alpha;:"),
                                  min     = 0.1,
                                  max     = 10,
                                  value   = 2,
                                  step    = 0.1,
                                  animate = TRUE),
                      
                      conditionalPanel(condition="input.Distribucion!='Kumaraswamy Weibull Modificada'",
                                       
                      sliderInput(inputId = "sigma.link",
                                  label   = HTML("Elija el valor del parámetro Forma &beta;:"),
                                  min     = 0.001,
                                  max     = 10,
                                  value   = 1.3,
                                  step    = 0.1,
                                  animate = TRUE)
                    ),
                    
                    conditionalPanel(condition="input.Distribucion" %in% "x4",
                                     
                                     sliderInput(inputId = "theta.Link",
                                                 label   = HTML("Elija el valor del parámetro  &theta;:"),
                                                 min     = 0.001,
                                                 max     = 10,
                                                 value   = 0.6,
                                                 step    = 0.1,
                                                 animate = TRUE)
                    ),
                    
                    conditionalPanel(condition="input.Distribucion=='Geométrica de Weibull'",
                                     
                                     sliderInput(inputId = "nu2.link",
                                                 label   = HTML("Elija el valor del parámetro &lambda;:"),
                                                 min     = 0.0001,
                                                 max     = 1,
                                                 value   = 0.01,
                                                 step    = 0.01,
                                                 animate = TRUE)
                    ),
                      
                      conditionalPanel(condition="input.Distribucion" %in% "x2",
                      
                      sliderInput(inputId = "nu.link",
                                  label   = HTML("Elija el valor del parámetro Localización &lambda;:"),
                                  min     = 0.001,
                                  max     = 10,
                                  value   = 3,
                                  step    = 0.1,
                                  animate = TRUE)
                      
                      ),
                    
                      
                      conditionalPanel(condition="input.Distribucion" %in% "x3",
                                       
                                       sliderInput(inputId = "tau.link",
                                                   label   = HTML("Elija el valor del parámetro  &gamma;:"),
                                                   min     = 0.001,
                                                   max     = 10,
                                                   value   = 1,
                                                   step    = 0.1,
                                                   animate = TRUE)
                                       
                      ),
                    
                    
                    
                    conditionalPanel(condition="input.Distribucion=='Kumaraswamy Weibull Modificada'",
                                     
                                     sliderInput(inputId = "a.link",
                                                 label   = HTML("Elija el valor del parámetro  a:"),
                                                 min     = 0.001,
                                                 max     = 10,
                                                 value   = 2,
                                                 step    = 0.1,
                                                 animate = TRUE),
                                     
                                     sliderInput(inputId = "b.link",
                                                 label   = HTML("Elija el valor del parámetro  b:"),
                                                 min     = 0.001,
                                                 max     = 10,
                                                 value   = 1.2,
                                                 step    = 0.1,
                                                 animate = TRUE)
                                     
                    ),

                      sliderInput(inputId = "x",
                                  label   = "Elija el el tamaño del eje x:",
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
