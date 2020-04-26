setwd("C:/Users/alexg/Google Drive/05-Proyectos/C_antarcticus")
rawdata0<-read.csv('data/rawCryptogyrus.csv')
rawdata<-rawdata0[,c(2,3,6:29)]

#Estructura de los datos
rawdata$EDAD<-factor(rawdata$EDAD,levels=c('Desconocido','Joven','Preadulto','Adulto'))
rawdata$LADO<-factor(rawdata$LADO,levels=c('Izquierdo','Central','Derecho'))
str(rawdata)
summary(rawdata)
#Fin de preparacion de datos

#Subset para datos en puntos y lados
rawdata<-rawdata[!rawdata$PUNTO=="",]

#Solo datos de adultos y preadultos
PR_ADdata<-rawdata[!rawdata$SEXO=="Desconocido",]


tabanalisis<-PR_ADdata


library(ComplexHeatmap)
mat<-as.matrix(tabanalisis[,-c(1:5)])

sex<- tabanalisis$SEXO
edad<-tabanalisis$EDAD
rownames(mat)<-edad

Heatmap(mat,
        column_names_side = 'top',
        row_names_side = 'left',
        row_names_gp = gpar(cex=0.3))

library(tidyr)
library(ggbiplot)
x<-rawdata[,]
mat<-na.omit(x)
mat1<-mat[,-c(1:5)]

mat.pca <- prcomp(mat1, scale. = TRUE)

class<-mat$EDAD
g<-ggbiplot(mat.pca, obs.scale = 1, var.scale = 1, groups = class, ellipse = TRUE, circle = TRUE)+
  theme_classic()
library(plotly)
ggplotly(g)
dev.off()

library(pca3d)
pca3d(mat.pca,biplot=TRUE,gr=mat$PUNTO,legend = 'bottomleft',
      show.labels = TRUE)

library(ggdendro)
hc<-hclust(dist(mat1))

#Graficos generales
library(ggplot2)
library(GGally)

#Individuos por sexo, relleno edad
ggplot(tabanalisis,aes(x=SEXO,fill=EDAD))+
  geom_bar()+
  ggtitle('Individuos totales por sexo y edad')

#Individuos por edad, relleno sexo
ggplot(tabanalisis,aes(x=EDAD,fill=SEXO))+
  geom_bar()+
  ggtitle('Individuos totales por edad y sexo')

table1<-table(tabanalisis$EDAD,tabanalisis$SEXO)
mosaicplot(t(table1))


#EJES DOS VARIABLES NUMERICAS, REPRESENTACION EN FORMA DE PUNTOS Y CONTORNO,
#AGRUPADOS POR SEXO Y/O EDAD, LATERALES CON CURVAS NORMALES
varvet<-colnames(rawdata)
varvetnum<-varvet[8:28]

listcor<-list()

i<-8
j<-9

VAR.X<-rawdata[,i]
VAR.Y<-rawdata[,j]
    
    
ggplot(rawdata,aes(x=VAR.X,y=VAR.Y))+
      geom_point()+
      geom_density2d()+
      facet_grid(SEXO~EDAD)+
      geom_smooth(method="lm", se=F)+
      xlab(varvet[i])+
      ylab(varvet[j])+
      ggtitle(as.character(paste(varvet[i],'vs',varvet[j])))


ggplot(rawdata, aes(EDAD,LONG.TOT))+
  geom_boxplot(aes(fill=factor(SEXO)))

ggplot(rawdata, aes(EDAD,LONG.TOT))+
  geom_violin(aes(fill=factor(SEXO)))


#Da la impresion que el grupo desconocido se compone de dos poblaciones bimodales para
#la variable longitud

Varfill<-rawdata$EDAD

ggplot(rawdata,aes(x=rawdata$LONG.TOT,fill=Varfill))+
    geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ANTREAL1,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ANTREAL2,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ANTREAL3,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ANTREAL4,fill=Varfill))+
  geom_density(alpha=0.5)


ggplot(rawdata,aes(x=rawdata$ANTTOT,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$DIAMCAB,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$U?A3,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$EMPODIO3,fill=Varfill))+
  geom_density(alpha=0.5)+
  geom_rug()

ggplot(rawdata,aes(x=rawdata$MAN,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$DENS,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$MUCRON,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$FURTOT,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$PAO,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$OJO,fill=Varfill))+
  geom_density(alpha=0.5)+
  geom_rug()

ggplot(rawdata,aes(x=rawdata$TORAX2,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOM1,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOM4,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOM5,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOM6,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOMTOT,fill=Varfill))+
  geom_density(alpha=0.5)


###DENSIDADES POR SEXO####
#Por sexo, no se puede determinar a partir de ningun dato la variable sexo del
#grupo desconocido
Varfill<-rawdata$SEXO

ggplot(rawdata,aes(x=rawdata$LONG.TOT,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ANTREAL1,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ANTREAL2,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ANTREAL3,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ANTREAL4,fill=Varfill))+
  geom_density(alpha=0.5)


ggplot(rawdata,aes(x=rawdata$ANTTOT,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$DIAMCAB,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$U?A3,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$EMPODIO3,fill=Varfill))+
  geom_density(alpha=0.5)+
  geom_rug()

ggplot(rawdata,aes(x=rawdata$MAN,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$DENS,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$MUCRON,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$FURTOT,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$PAO,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$OJO,fill=Varfill))+
  geom_density(alpha=0.5)+
  geom_rug()

ggplot(rawdata,aes(x=rawdata$TORAX2,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOM1,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOM4,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOM5,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOM6,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOMTOT,fill=Varfill))+
  geom_density(alpha=0.5)

###DENSIDADES POR GRUPO#####

#Por GRUPO
Varfill<-rawdata$GRUPO

ggplot(rawdata,aes(x=rawdata$LONG.TOT,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ANTREAL1,fill=Varfill))+
  geom_density(alpha=0.5)


ggplot(rawdata,aes(x=rawdata$ANTREAL2,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ANTREAL3,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ANTREAL4,fill=Varfill))+
  geom_density(alpha=0.5)




ggplot(rawdata,aes(x=rawdata$ANTTOT,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$DIAMCAB,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$U?A3,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$EMPODIO3,fill=Varfill))+
  geom_density(alpha=0.5)+
  geom_rug()

ggplot(rawdata,aes(x=rawdata$MAN,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$DENS,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$MUCRON,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$FURTOT,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$PAO,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$OJO,fill=Varfill))+
  geom_density(alpha=0.5)+
  geom_rug()

ggplot(rawdata,aes(x=rawdata$TORAX2,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOM1,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOM4,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOM5,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOM6,fill=Varfill))+
  geom_density(alpha=0.5)

ggplot(rawdata,aes(x=rawdata$ABDOMTOT,fill=Varfill))+
  geom_density(alpha=0.5)


##PROPUESTA GRAFICO CORRELACION

#vARIABLE SEXO NO PARECE MUY IMPORTANTE, REALIZAR TUKEYS PARA COMPROBAR DIFERENCIAS
#DE MEDIAS POR SEXO, ELIMINANDO SEXOS DESCONOCIDOS

boxplot(rawdata[,12]~rawdata$SEXO)



#ANOVA MULTIFACTORIAL EDAD~SEXO

ggplot(rawdata,aes(x=rawdata$EDAD))+
  facet_grid(MUESTRA~.)+
  geom_density()

