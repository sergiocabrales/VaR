# INGENIERÍA FINANCIERA
#################################################
# Librerias 

library(timeDate)
library(timeSeries)
library(fBasics)
library(zoo)
library(xts)
library(tseries)
library(quantmod)

## Cargar Precios
# Exxon Mobil. XOM
getSymbols("XOM",from="2010-01-01") 

## Retornos
# Calcular los retornos de las 2 acciones

XOM$Ret = dailyReturn(XOM$XOM.Adjusted, type="log")

T=length(XOM$XOM.Close) # Número de Datos

# Cálculo de la volatilidad
# Modelo 
W=252

# ARCH(1,1)

Garch=garch(XOM$Ret,order=c(1,1))

summary(Garch)

for (i in 1:T){
  if(i >= W){
    XOM$GarchProns = Garch$coef[1] + Garch$coef[2]*XOM$Ret[i-1]^2 + Garch$coef[3]*Garch$fitted.values[i-1,1]^2
    XOM$VolGARCH[i] <- XOM$GarchProns ^0.5
  } else {
    XOM$VolGARCH[i] <- NA
  }
}  

plot(1:T,XOM$VolGARCH*sqrt(252),'l')


RetPromedio = mean(XOM$Ret)

# Cálculo del VaR al 99%

alpha_1=0.01 # Nivel de significancia

ValorCritico_1=qnorm(alpha_1)

# VaR semanal

for (i in 1:T){
  if(i >= W){
    XOM$VaR_W_1[i] <- 5*RetPromedio + sqrt(5)*ValorCritico_1*XOM$VolGARCH[i]
  } else {
    XOM$VaR_W_1[i] <- NA
  }
} 


# Cálculo del VaR al 95%

alpha_5=0.05 # Nivel de significancia

ValorCritico_5=qnorm(alpha_5)

# VaR semanal

for (i in 1:T){
  if(i >= W){
    XOM$VaR_W_5[i] <- 5*RetPromedio + sqrt(5)*ValorCritico_5*XOM$VolGARCH[i]
  } else {
    XOM$VaR_W_5[i] <- NA
  }
} 

plot(1:T,XOM$Ret,'l', ylim=c(-0.15, 0.10))
lines(1:T,XOM$VaR_W_1,'l', col=34)
lines(1:T,XOM$VaR_W_5,'l', col=24)
