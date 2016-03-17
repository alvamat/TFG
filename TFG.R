library(lattice)
library(dplyr)
library(fpc)
library(Rmixmod)
library(ggplot2)
library(cluster)
#Programa para la lectura de datos csv en R

#Empezamos leyendo el documento del cual vamos a extraer los datos:

Datos<-read.csv("PublicUSefulBankDataFurtherReduced.txt",sep=",",header=TRUE)

#we are going to select only a few variables
clients <- data.frame(Datos$Risk.Country,Datos$Customer.Code, Datos$Line.Of.Business, 
                      Datos$Industry, Datos$Customer.Type, Datos$Profit.Center.Area, 
                      Datos$Segment, Datos$Area)
clients <- unique(clients)
rowsOK <- complete.cases(clients)
clients <- clients[rowsOK,]
clients <- clients[!duplicated(clients[,c('Datos.Customer.Code')]),]
clients <- filter(clients, Datos.Risk.Country=="BRAZIL");



clients$Datos.Customer.Type<-as.numeric(clients$Datos.Customer.Type)
clients$Datos.Line.Of.Business<-as.numeric(clients$Datos.Line.Of.Business)
clients$Datos.Industry<-as.numeric(clients$Datos.Industry)
clients$Datos.Area<-as.numeric(clients$Datos.Area)
clients$Datos.Profit.Center.Area<-as.numeric(clients$Datos.Profit.Center.Area)
clients$Datos.Segment<-as.numeric(clients$Datos.Segment)

dat<-select(clients, Datos.Profit.Center.Area,Datos.Customer.Type, Datos.Line.Of.Business, 
            Datos.Industry, Datos.Profit.Center.Area, Datos.Segment)

dat<-scale(dat)

set.seed(33)
numcenters = 4;
ClusterKmeans<-kmeans(dat,numcenters,iter.max=10,algorithm = "Forgy")
#with(clients, pairs(dat, col=c(1:20)[ClusterKmeans$cluster])) 

#plotcluster(dat, clients$cluster)
#clusplot(dat, ClusterKmeans$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

Cluster1 <- data.frame(clients[(ClusterKmeans$cluster==1),])
Cluster2 <- data.frame(clients[(ClusterKmeans$cluster==2),])
Cluster3 <- data.frame(clients[(ClusterKmeans$cluster==3),])
Cluster4 <- data.frame(clients[(ClusterKmeans$cluster==4),])

clients_plot <- clients

levelsIndustry <-
  levels(clients_plot$Datos.Industry)[-50][-49][-48]


#png(file="plot9.png")

ggplot(Cluster1,aes(Datos.Industry)) +
  geom_freqpoly(data=Cluster1,color = "green", alpha = 1)+
  geom_freqpoly(data=Cluster2,color = "red", alpha = 1)+
  geom_freqpoly(data=Cluster3,color = "black", alpha = 1)+
  geom_freqpoly(data=Cluster4,color = "blue", alpha = 1)+
  scale_x_discrete(breaks=1:length(levelsIndustry),
                   labels=levelsIndustry)+
theme(axis.text.x = element_text(angle = 90, hjust = 1))




#Cluster con picos más poblados
sort(table(Cluster4$Datos.Industry))

