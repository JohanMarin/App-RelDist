library(shiny)
if (!require('devtools')) install.packages('devtools')
if (!require('RelDists')) devtools::install_github('ousuga/RelDists', force=TRUE)
library(RelDists)

shinyServer(function(input, output){
  
  output$titleDistribucion <- renderText(
    paste("Distribución", input$Distribucion)
  )
  
  output$graficos <- renderPlot({
    par(mfrow=c(2,2))
    
    if(input$Distribucion == "Exponencial Weibull"){  
    curve(dEW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link ),
          ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
          main="Función Densidad de Probabilidad", las = 1)
    grid()
    
    curve(pEW(mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link, x),
          xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1, lwd=2, col="deepskyblue3", 
          main= "Función de Confiabilidad")
    grid()

    curve(hEW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link ),
          ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3", main= "Funcion de Riesgo")
    grid()
    
    }else if(input$Distribucion == "Flexible Weibull Extension"){
      curve(dFWE(x, mu=input$mu.link, sigma=input$sigma.link),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pFWE(mu=input$mu.link, sigma=input$sigma.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1, lwd=2, col="deepskyblue3", 
            main= "Función de Confiabilidad")
      grid()
      
      curve(hFWE(x, mu=input$mu.link, sigma=input$sigma.link ),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3", main= "Función de Riesgo")
      grid()  
    }

    
  })
  

  
  output$med.var <- renderText({
    
    if(input$Distribucion == "Exponencial Weibull"){ 
      fun1       <- function(x, mu, sigma, nu) x * RelDists::dEW(x, mu = mu, 
                                                                 sigma = sigma,
                                                                 nu = nu)
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link)
      fun2       <- function(x, mu, sigma, nu) {
                    media <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                             sigma = input$sigma.link, nu = input$nu.link)$value
                    (x-media)^2 * RelDists::dEW(x, mu = mu, sigma = sigma,nu = nu
                )}
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link)
    
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
      
    }else if(input$Distribucion == "Flexible Weibull Extension"){
      fun1       <- function(x, mu, sigma) x * RelDists::dFWE(x, mu = mu, 
                                                                 sigma = sigma)
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link)
      fun2       <- function(x, mu, sigma) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                            sigma = input$sigma.link)$value
        (x-media)^2 * RelDists::dFWE(x, mu = mu, sigma = sigma
        )}
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
      }
              
  })
  
})