library(shiny)
if (!require('devtools')) {install.packages('devtools')}
if (!require('RelDists')) {devtools::install_github('ousuga/RelDists', force=TRUE)}
library(RelDists)

shinyServer(function(input, output){
  
  output$titleDistribucion <- renderText(
    paste("Distribución", input$Distribucion)
  )
  
  output$graficos <- renderPlot({
    par(mfrow=c(2,2))
    
    ######################################################################
    #                     flexible weibull extension                     #
    ######################################################################
    
    if(input$Distribucion == "Flexible Weibull Extension"){
      curve(dFWE(x, mu=input$mu.link, sigma=input$sigma.link),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pFWE(mu=input$mu.link, sigma=input$sigma.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1, 
            lwd=2, col="deepskyblue3", main= "Función de Confiabilidad")
      grid()
      
      curve(hFWE(x, mu=input$mu.link, sigma=input$sigma.link ),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3",
            main= "Función de Riesgo")
      grid()  
    }
    
    ######################################################################
    #                         weibull inversa                        #
    ######################################################################
    
    else if(input$Distribucion == "Weibull Inversa"){
      curve(dIW(x, mu=input$mu.link, sigma=input$sigma.link),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pIW(mu=input$mu.link, sigma=input$sigma.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3",main= "Función de Confiabilidad")
      grid()
      
      curve(hIW(x, mu=input$mu.link, sigma=input$sigma.link ),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3", 
            main= "Función de Riesgo")
      grid()  
    }
    
    ######################################################################
    #                               Log-weibull                          #
    ######################################################################
    
    else if(input$Distribucion == "Log-weibull"){
      curve(dLW(x, mu=input$mu.link, sigma=input$sigma.link),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pLW(mu=input$mu.link, sigma=input$sigma.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3",main= "Función de Confiabilidad")
      grid()
      
      curve(hLW(x, mu=input$mu.link, sigma=input$sigma.link ),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3", 
            main= "Función de Riesgo")
      grid()  
    }
    
    ######################################################################
    #                             Power Lindley                          #
    ######################################################################
    
    else if(input$Distribucion == "Power Lindley"){
      curve(dPL(x, mu=input$mu.link, sigma=input$sigma.link),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pPL(mu=input$mu.link, sigma=input$sigma.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3",main= "Función de Confiabilidad")
      grid()
      
      curve(hPL(x, mu=input$mu.link, sigma=input$sigma.link ),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3", 
            main= "Función de Riesgo")
      grid()  
    }
    
    ######################################################################
    #                          Weibull Reflejada                         #
    ######################################################################
    else if(input$Distribucion == "Weibull Reflejada"){
      curve(dRW(x, mu=input$mu.link, sigma=input$sigma.link),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pRW(mu=input$mu.link, sigma=input$sigma.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3",main= "Función de Confiabilidad")
      grid()
      
      curve(hRW(x, mu=input$mu.link, sigma=input$sigma.link ),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3", 
            main= "Función de Riesgo")
      grid()  
    }
    
    ######################################################################
    #                         Exponencial Weibull                        #
    ######################################################################
    
    else if(input$Distribucion == "Exponencial Weibull"){  
    curve(dEW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link ),
          ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
          main="Función Densidad de Probabilidad", las = 1)
    grid()
    
    curve(pEW(mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link, x),
          xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
          lwd=2, col="deepskyblue3", main= "Función de Confiabilidad")
    grid()

    curve(hEW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link ),
          ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3",
          main= "Funcion de Riesgo")
    grid()
    
    }
    
    
    ######################################################################
    #                      Power Weibull Generalizada                    #
    ######################################################################
    
    else if(input$Distribucion == "Power Weibull Generalizada"){  
      curve(dGPW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link ),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pGPW(mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3", main= "Función de Confiabilidad")
      grid()
      
      curve(hGPW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link ),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3",
            main= "Funcion de Riesgo")
      grid()
      
    }
    
    ######################################################################
    #                          Weibull Modificada                        #
    ######################################################################
    
    if(input$Distribucion == "Weibull Modificada"){  
      curve(dMW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link ),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pMW(mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3", main= "Función de Confiabilidad")
      grid()
      
      curve(hMW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link ),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3",
            main= "Funcion de Riesgo")
      grid()
      
    }
    
    ######################################################################
    #                     Weibull Extension Modificada                   #
    ######################################################################
    
    else if(input$Distribucion == "Weibull Extension Modificada"){  
      curve(dMWEx(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link ),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pMWEx(mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3", main= "Función de Confiabilidad")
      grid()
      
      curve(hMWEx(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link ),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3",
            main= "Funcion de Riesgo")
      grid()
      
    }
    
    ######################################################################
    #              Weibull Modificada de Sarhan y Zaindins               #
    ######################################################################
    
    else if(input$Distribucion == "Weibull Modificada de Sarhan y Zaindins"){  
      curve(dSZMW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link ),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pSZMW(mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3", main= "Función de Confiabilidad")
      grid()
      
      curve(hSZMW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link ),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3",
            main= "Funcion de Riesgo")
      grid()
      
    }
    
    ######################################################################
    #                       Geométrica de Weibull                        #
    ######################################################################
    
    else if(input$Distribucion == "Geométrica de Weibull"){  
      curve(dWG(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu2.link ),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pWG(mu=input$mu.link, sigma=input$sigma.link, nu=input$nu2.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3", main= "Función de Confiabilidad")
      grid()
      
      curve(hWG(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu2.link ),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3",
            main= "Funcion de Riesgo")
      grid()
      
    }

    
    ######################################################################
    #                            Weibull Extraña                         #
    ######################################################################
    else if(input$Distribucion == "Weibull Extraña"){  
      curve(dOW(x, mu=input$mu.link, theta=input$sigma.link, nu=input$nu.link),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pOW(mu=input$mu.link, theta=input$sigma.link, nu=input$nu.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3", main= "Función de Confiabilidad")
      grid()
      
      curve(hOW(x, mu=input$mu.link, theta=input$sigma.link, nu=input$nu.link),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3",
            main= "Funcion de Riesgo")
      grid()
      
    }
    
    ######################################################################
    #                  Weibull Modificada Generalizada                   #
    ######################################################################
    else if(input$Distribucion == "Weibull Modificada Generalizada"){  
      curve(dGMW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link, tau=input$tau.link),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pGMW(mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link, tau=input$tau.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3", main= "Función de Confiabilidad")
      grid()
      
      curve(hGMW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link, tau=input$tau.link ),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3",
            main= "Funcion de Riesgo")
      grid()
      
    }
    
    ######################################################################
    #                          Kumaraswamy Weibull                       #
    ######################################################################
    else if(input$Distribucion == "Kumaraswamy Weibull"){  
      curve(dKW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link, tau=input$tau.link),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pKW(mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link, tau=input$tau.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3", main= "Función de Confiabilidad")
      grid()
      
      curve(hKW(x, mu=input$mu.link, sigma=input$sigma.link, nu=input$nu.link, tau=input$tau.link ),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3",
            main= "Funcion de Riesgo")
      grid()
      
    }
    
    ######################################################################
    #                  Kumaraswamy Weibull Modificada                    #
    ######################################################################
    else if(input$Distribucion == "Kumaraswamy Weibull Modificada"){  
      curve(dKMW(x, alpha=input$mu.link, theta=input$theta.link, lambda=input$nu.link, a=input$a.link, b=input$b.link),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pKMW(alpha=input$mu.link, theta=input$theta.link, lambda=input$nu.link, a=input$a.link, b=input$b.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3", main= "Función de Confiabilidad")
      grid()
      
        curve(hKMW(x, alpha=input$mu.link, theta=input$theta.link, lambda=input$nu.link, a=input$a.link, b=input$b.link),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3",
            main= "Función de Riesgo")
      grid()
    }
    
    ######################################################################
    #              Weibull Modificada de Almaki y Yuan                   #
    ######################################################################
    else if(input$Distribucion == "Weibull Modificada de Almaki y Yuan"){  
      curve(dNMW(x, alpha=input$mu.link, beta=input$sigma.link, theta=input$theta.link, gamma=input$tau.link, lambda=input$nu.link),
            ylab="Densidad", xlim=c(0,input$x), lwd=2, col="deepskyblue3",
            main="Función Densidad de Probabilidad", las = 1)
      grid()
      
      curve(pNMW(alpha=input$mu.link, beta=input$sigma.link, theta=input$theta.link, gamma=input$tau.link, lambda=input$nu.link, x),
            xlab="x",ylab = "Confiabilidad", ylim=c(0,1), xlim=c(0,input$x),las=1,
            lwd=2, col="deepskyblue3", main= "Función de Confiabilidad")
      grid()
      
      curve(hNMW(x,  alpha=input$mu.link, beta=input$sigma.link, theta=input$theta.link, gamma=input$tau.link, lambda=input$nu.link),
            ylab="Riesgo", xlim=c(0,input$x), lwd=2, las = 1, col="deepskyblue3",
            main= "Función de Riesgo")
      grid()
    }

  })
  
  
  
  
  #-------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------

  
  
  
  
  output$med.var <- renderText({
    
    ######################################################################
    #                     flexible weibull extension                     #
    ######################################################################
    
    if(input$Distribucion == "Flexible Weibull Extension"){
      fun1       <- function(x, mu, sigma) x * RelDists::dFWE(x, mu = mu, 
                                                              sigma = sigma)
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link)
      fun2       <- function(x, mu, sigma) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                            sigma = sigma)$value
        (x-media)^2 * RelDists::dFWE(x, mu = mu, sigma = sigma
        )}
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
    
    ######################################################################
    #                         weibull inversa                        #
    ######################################################################
    
    else if(input$Distribucion == "Weibull Inversa"){
      fun1       <- function(x, mu, sigma) x * RelDists::dIW(x, mu = mu, 
                                                             sigma = sigma)
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link)
      
      fun2       <- function(x, mu, sigma) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                            sigma = sigma)$value
        (x-media)^2 * RelDists::dIW(x, mu = mu, sigma = sigma
        )}
      varianza   <- integrate (fun2, lower = 0, upper = 100000, mu = input$mu.link, 
                               sigma = input$sigma.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4),"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
    
    ######################################################################
    #                               Log-Weibull                          #
    ######################################################################
    
    else if(input$Distribucion == "Log-weibull"){
      fun1       <- function(x, mu, sigma) x * RelDists::dLW(x, mu = mu, 
                                                             sigma = sigma)
      
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link)
      
      fun2       <- function(x, mu, sigma) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                            sigma = sigma)$value
        (x-media)^2 * RelDists::dLW(x, mu = mu, sigma = sigma
        )}
      
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4),"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
    
    ######################################################################
    #                             Power Lindley                          #
    ######################################################################
    
    else if(input$Distribucion == "Power Lindley"){
      fun1       <- function(x, mu, sigma) x * RelDists::dPL(x, mu = mu, 
                                                             sigma = sigma)
      
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link)
      
      fun2       <- function(x, mu, sigma) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                            sigma = sigma)$value
        (x-media)^2 * RelDists::dPL(x, mu = mu, sigma = sigma
        )}
      
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4),"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
    
    
    ######################################################################
    #                          Weibull Reflejada                         #
    ######################################################################
    
    else if(input$Distribucion == "Weibull Reflejada"){
      fun1       <- function(x, mu, sigma) x * RelDists::dRW(x, mu = mu, 
                                                             sigma = sigma)
      
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link)
      
      fun2       <- function(x, mu, sigma) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                            sigma = sigma)$value
        (x-media)^2 * RelDists::dRW(x, mu = mu, sigma = sigma
        )}
      
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4),"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    } 
    ######################################################################
    #                       Geométrica de Weibull                        #
    ######################################################################
    
    else if(input$Distribucion == "Geométrica de Weibull"){ 
      fun1       <- function(x, mu, sigma, nu) x * RelDists::dWG(x, mu = mu, 
                                                             sigma = sigma, nu=nu)
      
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu2.link)
      
      fun2       <- function(x, mu, sigma) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                            sigma = sigma, nu = nu)$value
        (x-media)^2 * RelDists::dWG(x, mu = mu, sigma = sigma, nu = nu
        )}
      
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu =input$nu2.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
    

    ######################################################################
    #                         Exponencial Weibull                       #
    ######################################################################
    
    if(input$Distribucion == "Exponencial Weibull"){ 
      fun1       <- function(x, mu, sigma, nu) x * RelDists::dEW(x, mu = mu, 
                                                                 sigma = sigma,
                                                                 nu = nu)
      
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link)
      
      fun2       <- function(x, mu, sigma, nu) {
                    media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                             sigma = sigma, nu = nu)$value
                    (x-media)^2 * RelDists::dEW(x, mu = mu, sigma = sigma,nu = nu
                    )}
      
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link)
    
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
      
      ######################################################################
      #                      Power Weibull Generalizada                    #
      ######################################################################
      
      else if(input$Distribucion == "Power Weibull Generalizada"){ 
        fun1       <- function(x, mu, sigma, nu) x * RelDists::dGPW(x, mu = mu, 
                                                                   sigma = sigma,
                                                                   nu = nu)
        
        esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                                 sigma = input$sigma.link, nu = input$nu.link)
        
        fun2       <- function(x, mu, sigma, nu) {
          media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                              sigma = sigma, nu = nu)$value
          (x-media)^2 * RelDists::dGPW(x, mu = mu, sigma = sigma,nu = nu
          )}
        
        varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                                 sigma = input$sigma.link, nu = input$nu.link)
        
        paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
              'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
      }
      
      ######################################################################
      #                          Weibull Modificada                        #
      ######################################################################
    
    else if(input$Distribucion == "Weibull Modificada"){ 
      fun1       <- function(x, mu, sigma, nu) x * RelDists::dMW(x, mu = mu, 
                                                                  sigma = sigma,
                                                                  nu = nu)
      
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link)
      
      fun2       <- function(x, mu, sigma, nu) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                            sigma = sigma, nu = nu)$value
        (x-media)^2 * RelDists::dMW(x, mu = mu, sigma = sigma,nu = nu
        )}
      
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
    
    ######################################################################
    #                     Weibull Extension Modificada                   #
    ######################################################################
    
    else if(input$Distribucion == "Weibull Extension Modificada"){ 
      fun1       <- function(x, mu, sigma, nu) x * RelDists::dMWEx(x, mu = mu, 
                                                                 sigma = sigma,
                                                                 nu = nu)
      
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link)
      
      fun2       <- function(x, mu, sigma, nu) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                            sigma = sigma, nu = nu)$value
        (x-media)^2 * RelDists::dMWEx(x, mu = mu, sigma = sigma,nu = nu
        )}
      
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
    
    ######################################################################
    #              Weibull Modificada de Sarhan y Zaindins               #
    ######################################################################
    
    else if(input$Distribucion == "Weibull Modificada de Sarhan y Zaindins"){ 
      fun1       <- function(x, mu, sigma, nu) x * RelDists::dSZMW(x, mu = mu, 
                                                                   sigma = sigma,
                                                                   nu = nu)
      
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link)
      
      fun2       <- function(x, mu, sigma, nu) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                            sigma = sigma, nu = nu)$value
        (x-media)^2 * RelDists::dSZMW(x, mu = mu, sigma = sigma,nu = nu
        )}
      
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
    
    
    ######################################################################
    #                            Weibull Extraña                         #
    ######################################################################
    else if(input$Distribucion == "Weibull Extraña"){ 
      fun1       <- function(x, mu, sigma, nu) x * RelDists::dOW(x, mu = mu, 
                                                                 theta = sigma,
                                                                   nu = nu)
      
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link)
      
      fun2       <- function(x, mu, sigma, nu) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                            sigma = sigma, nu = nu)$value
        (x-media)^2 * RelDists::dOW(x, mu = mu, theta = sigma,nu = nu
        )}
      
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
    
    ######################################################################
    #                  Weibull Modificada Generalizada                   #
    ######################################################################
    else if(input$Distribucion == "Weibull Modificada Generalizada"){ 
      fun1       <- function(x, mu, sigma, nu, tau) x * RelDists::dGMW(x, mu = mu, 
                                                                 sigma = sigma,
                                                                 nu = nu, tau = tau)
      
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link,
                               tau=input$tau.link)
      
      fun2       <- function(x, mu, sigma, nu, tau) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                            sigma = sigma, nu = nu,
                            tau = input$tau.link)$value
        (x-media)^2 * RelDists::dGMW(x, mu = mu, sigma = sigma, nu = nu, tau = tau
        )}
      
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link,
                               tau = input$tau.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
     
    ######################################################################
    #                          Kumaraswamy Weibull                       #
    ######################################################################
    else if(input$Distribucion == "Kumaraswamy Weibull"){ 
      fun1       <- function(x, mu, sigma, nu, tau) {x * RelDists::dKW(x, mu = mu, 
                                                                       sigma = sigma,
                                                                       nu = nu, tau = tau)}
      
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link,
                               tau=input$tau.link)
      
      fun2       <- function(x, mu, sigma, nu, tau) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                            sigma = sigma, nu = nu,
                            tau = tau)$value
        (x-media)^2 * RelDists::dKW(x, mu = mu, sigma = sigma, nu = nu, tau = tau
        )}
      
      varianza   <- integrate (fun2, lower = 0, upper = Inf, mu = input$mu.link, 
                               sigma = input$sigma.link, nu = input$nu.link,
                               tau = input$tau.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
    
    ######################################################################
    #                  Kumaraswamy Weibull Modificada                    #
    ######################################################################
    else if(input$Distribucion == "Kumaraswamy Weibull Modificada"){ 
      fun1       <- function(x, alpha, theta, lambda, a, b) x * RelDists::dKMW(x, alpha = alpha, 
                                                                      theta = theta,
                                                                      lambda = lambda, a=a, b=b)
      
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, alpha=input$mu.link, 
                               theta=input$theta.Link, lambda=input$nu.link,
                               a=input$a.link, b=input$b.link)
      
      fun2       <- function(x, alpha, theta, lambda, a, b) {
        media <- integrate (fun1, lower = 0, upper = Inf, alpha = alpha, 
                            theta = theta, lambda = lambda,
                            a=a, b=b)$value
        
        (x-media)^2 * RelDists::dKMW(x, alpha=alpha, theta=theta,lambda=lambda,
                                    a=a, b=b
        )}
      
      varianza   <- integrate (fun2, lower = 0, upper = Inf, alpha = input$mu.link, 
                               theta = input$theta.Link, lambda = input$nu.link,
                               a=input$a.link, b=input$b.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
    
      
      ######################################################################
    #              Weibull Modificada de Almaki y Yuan                   #
    ######################################################################
    else if(input$Distribucion == "Weibull Modificada de Almaki y Yuan"){ 
      
      fun1       <- function(x, alpha, beta, theta, gamma, lambda) {
                            x * RelDists::dNMW(x, alpha=alpha, beta=beta, 
                            theta=theta, gamma=gamma, lambda=lambda
                            )}
      
      esperanza  <- integrate (fun1, lower = 0, upper = Inf, alpha = input$mu.link, 
                               beta = input$sigma.link, theta = input$theta.Link, 
                               gamma=input$tau.link, lambda=input$nu.link
                               )
      
      fun2       <- function(x, alpha, beta, theta, gamma, lambda) {
        media <- integrate (fun1, lower = 0, upper = Inf, mu = mu, 
                            theta = theta, nu = nu,
                            a=a, b=b)$value
        
        (x-media)^2 * RelDists::dNMW(x, alpha=alpha, beta=beta, theta=theta, 
                                     gamma=gamma, lambda=lambda
        )}
      
      varianza   <- integrate (fun2, lower = 0, upper = Inf, alpha = input$mu.link,
                               beta = input$sigma.link, theta = input$theta.Link, 
                               gamma = input$tau.link, lambda = input$nu.link)
      
      paste("La esperanza de vida E(X) es ",round(as.numeric(esperanza[1]),4) ,"\n",
            'y la varianza Var(X) es', round(as.numeric(varianza[1]),4))
    }
  })
  
})
