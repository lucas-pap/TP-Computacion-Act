rm(list=ls())
##Librerias----

library(tseries)
library(corrplot)
library(lmtest)
library(ggplot2)
library(gridExtra)
library(GGally)
library(rriskDistributions)
library(readxl)
library(pastecs)
library(lessR)
library(moments)
library(tidyverse)
library(ggplot2)
library(car)
library(fastDummies)
library(forecast)
library(dplyr)


directorio <- choose.dir()
setwd(directorio)
getwd()


### Leemos el Archivo y depuramos los datos----

cacao <- read_excel("TP1/flavors_of_cacao.xlsx")

cacao2 <- subset(cacao, cacao$`Specific Bean Origin or Bar Name`=="The") ##Extraigo la parte que quiero arreglar
colnames(cacao2) = c('Company','Bar_Name', 'Ref', 'ReviewDate','CocoaPercent','CompanyLocation','Rating','BeanType','BroadBeanOrigin','Delete')
colnames(cacao) <- colnames(cacao2)
cacao2$'Company' <- paste(cacao2$'Bar_Name',cacao2$'Company')
for(i in 2:(ncol(cacao2)-1)){
  cacao2[,i] <- cacao2[,i+1]
}
cacao <- rbind(cacao2,subset(cacao, cacao$`Bar_Name`!="The"))
rm(cacao2,i)
cacao <- subset(cacao, select = -Delete)
cacao$CocoaPercent <- as.numeric(cacao$CocoaPercent)
cacao$Rating<- as.numeric(cacao$Rating)
cacao$Ref<- as.numeric(cacao$Ref)
cacao[is.na(cacao)==1]=''


##Agregamos una columna con la categoria de cada chocolate según su rating----

cacao2 <- cacao
cacao2$'Rating' <- 0

Categoria = c(rep(NA, nrow(cacao)))

cacao['Categoria'] = Categoria

for (i in 1:length(Categoria)){
  if (cacao$Rating[i] >= 5.00){
    cacao$Categoria[i] = 'Elite'
    cacao2$Rating[i] = 1
  }
  else if (cacao$Rating[i] >= 4.00 & cacao$Rating[i] < 5.00){
    cacao$Categoria[i] = 'Premium'
    cacao2$Rating[i] = 1
  }
  else if (cacao$Rating[i] >= 3.75 & cacao$Rating[i] < 4.00){
    cacao$Categoria[i] = 'Remarcable'
  }
  else if (cacao$Rating[i] >= 3.00 & cacao$Rating[i] < 3.75){
    cacao$Categoria[i] = 'Satisfactorio'
  }
  else if (cacao$Rating[i] >= 2.00 & cacao$Rating[i] < 3.00){
    cacao$Categoria[i] = 'Decepcionante'
  }
  else if (cacao$Rating[i] >= 1.00 & cacao$Rating[i] < 2.00){
    cacao$Categoria[i] = 'Implacentero'
  }
}


##Transformamos las variables categoricas en Factores----

cacao$CompanyLocation = factor(cacao$CompanyLocation)
cacao$Company = factor(cacao$Company)
cacao$Bar_Name = factor(cacao$Bar_Name)
cacao$BeanType = factor(cacao$BeanType)
cacao$BroadBeanOrigin = factor(cacao$BroadBeanOrigin)
cacao$Categoria = factor(cacao$Categoria)

levels(cacao$BeanType)[2]=''
levels(cacao$BroadBeanOrigin)[2]='' 


#################
#### Punto 1 ####
#################

###Chequeo de Distribucion----

cacaoporcent_Dist<- fit.cont(cacao$CocoaPercent)
rating_Dist<- fit.cont(cacao$Rating)


###DATOS ESTADÍSTICOS----

MediaRating = mean(cacao$Rating)
DesvioRating = sd(cacao$Rating)
IQR_Rating = IQR(cacao$Rating)
MAD_Rating = mad(cacao$Rating)
Variacion_Rating = 100*DesvioRating/MediaRating
Kurt_Rating = kurtosis(cacao$Rating)
Asim_Rating = skewness(cacao$Rating)

MediaPercent = mean(cacao$CocoaPercent)
DesvioPercent = sd(cacao$CocoaPercent)
IQR_Percent = IQR(cacao$CocoaPercent)
MAD_Percent = mad(cacao$CocoaPercent)
Variacion_Percent = 100*DesvioPercent/MediaRating
Kurt_Percent = kurtosis(cacao$CocoaPercent)
Asim_Percent = skewness(cacao$CocoaPercent)


cor(cacao$Rating,cacao$CocoaPercent) 
cor(cacao$Rating,cacao$Ref)         
cor(cacao$Rating,cacao$ReviewDate)

fivenum(cacao$Rating)
fivenum(cacao$CocoaPercent)
quantile(cacao$CocoaPercent, probs = c(0.15,0.3,0.45,0.6,0.9))
quantile(cacao$Rating, probs = c(0.15, 0.3, 0.45, 0.6, 0.9))

summary(cacao)
stat.desc(cacao)


###Cálculo de Momentos----

# Funcion que arroja el momento absoluto de orden y de la veriable x.
Mom_Abs<-function(x,y){
  sum((x^y)/length(x))
}

# Funcion cuyo outputs son los momentos listados abajo, filtrando la variable x con la condición y.
estadisticos<-function(x,y){
  n <- as.data.frame(table(y))[,2] 
  a<-as.data.frame(tapply(x,y,mean))
  b<-as.data.frame(tapply(x,y,sd))
  c<-as.data.frame(tapply(x,y,var))
  d<-as.data.frame(tapply(x,y,kurtosis))
  e<-as.data.frame(tapply(x,y,skewness))
  mc <- a - MediaRating 
  f<-data.frame(n,a,mc,b,c,d,e)
  colnames(f)<-c('n',"Media",'M. Centrada',"Desvío","Varianza","Kurtosis","Asimetría")
  return(f)
}


### Establecemos un ranking del Rating filtrando por: Company, Company location, Specific Origin, Broad Bean Origin y Bean Type----

company <- as.data.frame(estadisticos(cacao$"Rating",cacao$"Company"))
company <- company[order(-company$Media,-company$n),]

company_location <- as.data.frame(estadisticos(cacao$"Rating",cacao$"CompanyLocation"))
company_location <- company_location[order(-company_location$Media,-company_location$n),]

bar_name <- as.data.frame(estadisticos(cacao$"Rating",cacao$"Bar_Name"))
bar_name <- bar_name[order(-bar_name$Media,-bar_name$n),]

broad_bean <- as.data.frame(estadisticos(cacao$"Rating",cacao$"BroadBeanOrigin"))
broad_bean <- broad_bean[order(-broad_bean$Media,-broad_bean$n),]

bean_type <- as.data.frame(estadisticos(cacao$'Rating',cacao$'BeanType'))
bean_type <- bean_type[order(-bean_type$Media,-bean_type$n),]



### Algunos graficos----

Histogram(Rating, data = cacao, density = T, main = "Densidad", xlab = "Rating")
Histogram(Rating, data = cacao, breaks = 10, main = "Histograma", xlab = "Rating")
hist(cacao$Rating, breaks = 50, main = "Histograma Rating", ylab = "Frecuencia", xlab = "Ratings")
par(new=T)
plot(seq(1,5,0.01),dnorm(seq(1,5,0.01),MediaRating,DesvioRating),axes=F,type='l',xlab=NA,ylab=NA,cex=1.2,col='red')

scatter.smooth(cacao$CocoaPercent,cacao$Rating)


#Graficos Autocorrelacion
ggAcf(cacao$CocoaPercent)
ggAcf(cacao$Rating)          

#Graficos Autocorrelacion parcial
ggPacf(cacao$Rating)         
ggPacf(cacao$CocoaPercent)      


### Respuesta a las preguntas----

## a) ¿Dónde se producen los mejores granos de cacao? con BroadBeanOrigin

Top_grano <- broad_bean[broad_bean$n > 10& broad_bean$n!=74,]
Top_grano <-  Top_grano[order(-Top_grano$Media),]
Top_grano


## b) ¿Qué paises producen las barras de cacao mejor con mejor calificación? con CompanyLocation

Top_Paises<-company_location[company_location$n > 10,]
Top_Paises<-Top_Paises[order(-Top_Paises$Media),]
Top_Paises


## c) ¿Qué relación hay entre el porcentaje de cacao en una barra y su calificación?

Cacao_percent = cut(cacao$CocoaPercent,breaks = c(seq(0.4,1,0.05)))


#################
###  Punto 2 ####
#################

### Transformación de variables Categóricas
## Le aplicamos la media del Rating de cada valor que puede llegar a tomar la variable categórica:

#Para la variable Company Location:

tipos_comp<-matrix(cacao$CompanyLocation)

info_c_location<-function(y){
  a = c(rep(NA, length(y)))
  for (i in 1:length(y)) {
    if(y[i]==""){
      a[i]<-mean(cacao$Rating)
    }
    else if(company_location[y[i],"n"]>10)
    {
      a[i]<-company_location[y[i],"Media"]
    }else{
      a[i]<-mean(cacao$Rating)
    }
    
  } 
  return(a)
}
info_c_location(tipos_comp)


# Para la variable Broad Bean Origin

tipos_broad<-matrix(cacao$BroadBeanOrigin)

info_broad_bean<-function(y){
  a = c(rep(NA, length(y)))
  for (i in 1:length(y)) {
    if(y[i]==""){
      a[i]<-mean(cacao$Rating)
    }
    else if(broad_bean[y[i],"n"]>6)
    {
      a[i]<-broad_bean[y[i],"Media"]
    }else{
      a[i]<-mean(cacao$Rating)
    }
    
  } 
  return(a)
}
info_broad_bean(tipos_broad)


# Para la variable Bean Type

tipos_granos<-matrix(cacao$BeanType)

info_bean_type<-function(y){
  a = c(rep(NA, length(y)))
  for (i in 1:length(y)) {
    if(y[i]==""){
      a[i]<-mean(cacao$Rating)
    }
    else if(bean_type[y[i],"n"]>6)
    {
      a[i]<-bean_type[y[i],"Media"]
    }else{
      a[i]<-mean(cacao$Rating)
    }
    
  } 
  return(a)
}
info_bean_type(tipos_granos)


# Para la variable Companies

companias<-matrix(cacao$Company)

info_companias<-function(y){
  a = c(rep(NA, length(y)))
  for (i in 1:length(y)) {
    if(y[i]==""){
      a[i]<-mean(cacao$Rating)
    }
    else if(company[y[i],"n"]>10)
    {
      a[i]<-company[y[i],"Media"]
    }else{
      a[i]<-mean(cacao$Rating)
    }
    
  } 
  return(a)
}
info_companias(companias)


## Unimos los datos en un dataframe sobre el cual vamos a trabajar

a<-as.data.frame(info_companias(companias))
b<-as.data.frame(cacao$Rating)
c<-as.data.frame(info_bean_type(tipos_granos))
d<-as.data.frame(info_broad_bean(cacao$BroadBeanOrigin))
e<-as.data.frame(info_c_location(cacao$CompanyLocation))
cacao5<-as.data.frame(c(a,b,c,d,e))
cacao5<-cbind(as.data.frame(c(a,b,c,d,e)),cacao[,'CocoaPercent'])

cacao5

DATOS<-cacao5[c(1:(round(0.7*1795))),]
DATOS2<-cacao5[c((round(0.7*1795)):1795),]
rownames(DATOS2) <- 1:nrow(DATOS2)


ggpairs(DATOS, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")


#####################################
#### Modelo de Regresión Lineal  ####
#####################################

Modelo<-lm(cacao.Rating~ CocoaPercent+info_companias.companias.+info_bean_type.tipos_granos.+info_broad_bean.cacao.BroadBeanOrigin.+info_c_location.cacao.CompanyLocation.
           + CocoaPercent,data=DATOS) 

summary(Modelo)

step(object = Modelo, direction = "both", trace = 1)


### De los modelos que planteamos, el siguiente es el de AIC mas bajo:

Modelo2<-lm(cacao.Rating~ CocoaPercent+info_companias.companias.+info_bean_type.tipos_granos.,data=DATOS) 


summary(Modelo2)


### Validacion de Supuestos de Gauss Markov----

plot1 <- ggplot(data =DATOS, aes(DATOS$cacao.Rating, Modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = DATOS, aes(DATOS$info_companias.companias., Modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = DATOS, aes(DATOS$info_bean_type.tipos_granos., Modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = DATOS, aes(DATOS$CocoaPercent, Modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)



#### Test de normalidad de los residuos

qqnorm(Modelo2$residuals)
qqline(Modelo2$residuals)


#### Dist de Residuos 

cacaoporcent_Dist<- fit.cont(Modelo2$residuals)

### Shapiro Test

shapiro.test(Modelo2$residuals)

### Test de Jarque Bera

jarque.bera.test(Modelo2$residuals)

### Ljung Box

Box.test(Modelo2$residuals, lag = 1, type =  "Ljung-Box", fitdf = 0)

Box.test(Modelo2$residuals,lag = 1,type =  "Box-Pierce", fitdf = 0)

### Ploteo de residuos

ggplot(data = DATOS, aes(Modelo2$fitted.values, Modelo2$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()


### Test Homocedasticidad

bptest(Modelo2)


### Posibles valores atípicos (verdes) o influyentes

DATOS$studentized_residual <- rstudent(Modelo2)
ggplot(data = DATOS, aes(x = predict(Modelo2), y = abs(studentized_residual))) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
  geom_point(aes(color = ifelse(abs(studentized_residual) > 4, 'green', 'blue'))) +
  scale_color_identity() +
  labs(title = "Distribución de los residuos studentized",
       x = "predicción modelo") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


### Posicion de los Outliers 

which(abs(DATOS$studentized_residual) > 4)




#### Prediccion 

predict(Modelo2,newdata=DATOS2)




#######################################
#### Modelo de Regresión Logistica ####
#######################################

MatrixCompany<-matrix(cacao2$Company)
MatrixBarName<-matrix(cacao2$Bar_Name)
MatrixCompLoc<-matrix(cacao2$CompanyLocation)
MatrixBeanType<-matrix(cacao$BeanType)
MatrixBeanOrigin<-matrix(cacao$BroadBeanOrigin)

#Armamos dos funciones con las que categorizamos cada variable segun su media en Bueno o Malo:
categorizador<-function(y,x){
  a = c(rep(NA, length(y)))
  for (i in 1:length(y)) {
    if(x[y[i],"Media"]>3.5)
    {
      a[i]<-'Bueno'
    }else{
      a[i]<-'Malo'
    }
  } 
  return(a)
}

categorizador2<-function(y,x){
  a = c(rep(NA, length(y)))
  for (i in 1:length(y)) {
    if(y[i]==""){
      a[i]<-'Malo'
    }
    else if(x[y[i],"Media"]>3.5)
    {
      a[i]<-'Bueno'
    }else{
      a[i]<-'Malo'
    }
  } 
  return(a)
}


Company_Binario = categorizador(MatrixCompany,company)
BarName_Binario = categorizador(MatrixBarName,bar_name)
CompanyLoc_Binario = categorizador(MatrixCompLoc,company_location)
BeanType_Binario = categorizador2(MatrixBeanType,bean_type)
BeanOrig_Binario = categorizador2(MatrixBeanOrigin,broad_bean)

categorias_unidas = data.frame(Company_Binario,BarName_Binario,CompanyLoc_Binario,BeanType_Binario,BeanOrig_Binario)

# Agrupamos la variable Cacao Percent

Cacao_percent = cut(cacao2$CocoaPercent,breaks = c(seq(0.3999,1.00,0.100)),right = F,left = T,include.lowest = T)

intervalos = c(levels(Cacao_percent))

cacao2[,'CacaoPercent'] = Cacao_percent

cacao2$CacaoPercent[is.na(cacao2$CacaoPercent)==T]="[0.9,1]"

cacao2 = cbind(cacao2,categorias_unidas)


### Armamos el Data Frame con el que vamos a trabajar, transformando las variables a Dummies

cacao_dummy = dummy_columns(cacao2,select_columns = c('Company_Binario','BarName_Binario','CompanyLoc_Binario',
                                                      'BeanType_Binario','BeanOrig_Binario','CacaoPercent')) %>%
  dplyr::select(-c('Company','Bar_Name','CompanyLocation','CocoaPercent','BeanType','BroadBeanOrigin',
                   'Ref','ReviewDate','Company_Binario','BarName_Binario','CompanyLoc_Binario',
                   'BeanType_Binario','BeanOrig_Binario','CacaoPercent'))
cacao_dummy2 = subset(cacao_dummy,select= - 12)

cacao_dummy_train = cacao_dummy[c(1:(round(0.7*nrow(cacao_dummy)))),]
cacao_dummy2_train = cacao_dummy2[c(1:(round(0.7*nrow(cacao_dummy2)))),]

cacao_dummy_val = cacao_dummy[c(seq(round(0.7*nrow(cacao_dummy))+1,nrow(cacao_dummy),1)),]
cacao_dummy2_val = cacao_dummy2[c(seq(round(0.7*nrow(cacao_dummy2))+1,nrow(cacao_dummy2),1)),]


rownames(cacao_dummy_val) = seq(1,nrow(cacao_dummy_val))
rownames(cacao_dummy2_val) = seq(1,nrow(cacao_dummy2_val))


### Corremos dos modelos de regresión logistica:

modelo_logistico = glm(Rating ~ .,data=cacao_dummy_train,family = binomial(link = "logit"))
modelo_logistico2 = glm(Rating ~ .,data=cacao_dummy2_train,family = binomial(link = "logit"))

summary(modelo_logistico)
summary(modelo_logistico2)

## Predicción

predict(modelo_logistico,cacao_dummy_val)
predict(modelo_logistico2,cacao_dummy2_val)


#### Test de normalidad de los residuos

qqnorm(modelo_logistico$residuals)
qqline(modelo_logistico$residuals)

qqnorm(modelo_logistico2$residuals)
qqline(modelo_logistico2$residuals)


#### Shapiro Test

shapiro.test(modelo_logistico$residuals)
shapiro.test(modelo_logistico2$residuals)


#### Test de Jarque Bera

jarque.bera.test(modelo_logistico$residuals)
jarque.bera.test(modelo_logistico2$residuals)


#### Se usa para contrastar la normalidad de un conjunto de datos

Box.test(modelo_logistico$residuals, lag = 1, type =  "Ljung-Box", fitdf = 0)
Box.test(modelo_logistico2$residuals, lag = 1, type =  "Ljung-Box", fitdf = 0)

Box.test(modelo_logistico$residuals,lag = 1,type =  "Box-Pierce", fitdf = 0)
Box.test(modelo_logistico2$residuals,lag = 1,type =  "Box-Pierce", fitdf = 0)

#### Confección de Lista 

Datos_continua<-list(DATOS,DATOS2)
Datos_discreta<-list(cacao_dummy2_val,cacao_dummy_val,cacao_dummy2_train,cacao_dummy_train)
Lista_de_datos<-list(Datos_continua,Datos_discreta)


#############################
#### Enfoque Alternativo ####
#############################

bestcompanies = as.data.frame(table(cacao[cacao$Rating>=4,]$Company))
bestcompanies = bestcompanies[bestcompanies$Freq>0,]
top5copmanies = bestcompanies[order(-bestcompanies$Freq),][1:5,]

bestpercentages = as.data.frame(table(cacao[cacao$Rating>=4,]$CocoaPercent))
bestpercentages = bestpercentages[bestpercentages$Freq>0,]
top5percentages = bestpercentages[order(-bestpercentages$Freq),][1:5,]

bestlocation = as.data.frame(table(cacao[cacao$Rating>=4,]$CompanyLocation))
bestlocation = bestlocation[bestlocation$Freq>0,]
top5locations = bestlocation[order(-bestlocation$Freq),][1:5,]

bestorigins = as.data.frame(table(cacao[cacao$Rating>=4,]$BroadBeanOrigin))
bestorigins = bestorigins[bestorigins$Freq>0,]
top5origins = bestorigins[order(-bestorigins$Freq),][1:5,]

bestbeantypes = as.data.frame(table(cacao[cacao$Rating>=4,]$BeanType))
bestbeantypes = bestbeantypes[bestbeantypes$Freq>0,]
top5beans = bestbeantypes[order(-bestbeantypes$Freq),][1:5,]

CuadroTop5 = cbind(top5copmanies,top5beans,top5origins,top5percentages,top5locations)


worstcompanies = as.data.frame(table(cacao[cacao$Rating>=2,]$Company))
worstcompanies = worstcompanies[worstcompanies$Freq>0,]
worst5copmanies = worstcompanies[order(-worstcompanies$Freq),][1:5,]

worstpercentages = as.data.frame(table(cacao[cacao$Rating>=2,]$CocoaPercent))
bestpercentages = worstpercentages[worstpercentages$Freq>0,]
worst5percentages = worstpercentages[order(-worstpercentages$Freq),][1:5,]

worstClocation = as.data.frame(table(cacao[cacao$Rating>=2,]$CompanyLocation))
worstClocation = worstClocation[worstClocation$Freq>0,]
worst5locations = worstClocation[order(-worstClocation$Freq),][1:5,]

worstorigins = as.data.frame(table(cacao[cacao$Rating>=2,]$BroadBeanOrigin))
worstorigins = worstorigins[worstorigins$Freq>0,]
worst5origins = worstorigins[order(-worstorigins$Freq),][1:5,]

worstbeantypes = as.data.frame(table(cacao[cacao$Rating>=2,]$BeanType))
worstbeantypes = worstbeantypes[worstbeantypes$Freq>0,]
worst5beans = worstbeantypes[order(-worstbeantypes$Freq),][1:5,]

CuadroWorst5 = cbind(worst5copmanies,worst5beans,worst5origins,worst5percentages,worst5locations)


colnames(CuadroTop5) = c('Compañia','Freq','Tipo de Grano','Freq','Origen','Freq','%','Freq','Pais')
colnames(CuadroWorst5) = c('Compañia','Freq','Tipo de Grano','Freq','Origen','Freq','%','Freq','Pais')

CuadroTop5
CuadroWorst5


