library(lattice)
library(dplyr)
library(fpc)
library(Rmixmod)
library(ggplot2)
library(cluster)
library(xlsx)
#Programa para la lectura de datos csv en R

#Empezamos leyendo el documento del cual vamos a extraer los datos:


Datos<-read.csv("PublicUSefulBankDataFurtherReduced.txt",sep=",",header=TRUE)

#rowsOK <- complete.cases #elimina filas con Na's
#Datos <- Datos[rowsOK,]

#creamos un fichero para saber que producto tiene cada valor
Datose<-data.frame(Datos$Product.Description, Datos$Spread.Rate.Nominal)
Datose$Datos.Product.Description1<-as.numeric(Datose$Datos.Product.Description)

excel<-aggregate( formula = Datos.Spread.Rate.Nominal~Datos.Product.Description+Datos.Product.Description1, 
           data = Datose,
           FUN = mean );



#excel<-data.frame(excel, Datose$Datos.Product.Description)
#excel <- excel[!duplicated(excel[,c('Datos.Product.Description')]),] 

write.xlsx(excel, "Productos_Bancarios.xlsx", sheetName="Sheet1",
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)




#we are going to select only a few variables
clients <- data.frame(Datos$Risk.Country,Datos$Customer.Code, Datos$Line.Of.Business, 
                      Datos$Industry, Datos$Customer.Type, Datos$Profit.Center.Area, 
                      Datos$Segment, Datos$Area, Datos$Product.Description, Datos$Spread.Rate.Nominal, Datos$Total.Rate.Nominal)
#clients <- unique(clients)
#rowsOK <- complete.cases(clients)
#clients <- clients[rowsOK,]
#clients <- clients[!duplicated(clients[,c('Datos.Customer.Code')]),] no importa si nos salen clientes por
#duplicado, lo que nos interesa son todas sus paquetes bancarios

clients$Datos.Customer.Type<-as.numeric(clients$Datos.Customer.Type)
clients$Datos.Line.Of.Business<-as.numeric(clients$Datos.Line.Of.Business)
clients$Datos.Industry<-as.numeric(clients$Datos.Industry)
clients$Datos.Area<-as.numeric(clients$Datos.Area)
clients$Datos.Profit.Center.Area<-as.numeric(clients$Datos.Profit.Center.Area)
clients$Datos.Segment<-as.numeric(clients$Datos.Segment)
clients$Datos.Product.Description<-(Datose$Datos.Product.Description1)


clients <- filter(clients, Datos.Risk.Country=="UNITED STATES");





#hacemos los clusters
dat<-select(clients,Datos.Customer.Type, Datos.Line.Of.Business, 
            Datos.Industry, Datos.Segment, Datos.Profit.Center.Area,Datos.Area)

dat<-scale(dat)

set.seed(3)
numcenters = 5;
ClusterKmeans<-kmeans(dat,numcenters,iter.max=10,algorithm = "Forgy")
#with(clients, pairs(dat, col=c(1:20)[ClusterKmeans$cluster])) 

#plotcluster(dat, clients$cluster)
#clusplot(dat, ClusterKmeans$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

Cluster1 <- data.frame(clients[(ClusterKmeans$cluster==1),])
Cluster2 <- data.frame(clients[(ClusterKmeans$cluster==2),])
Cluster3 <- data.frame(clients[(ClusterKmeans$cluster==3),])
Cluster4 <- data.frame(clients[(ClusterKmeans$cluster==4),])
Cluster5 <- data.frame(clients[(ClusterKmeans$cluster==5),])
Cluster6 <- data.frame(clients[(ClusterKmeans$cluster==6),])
Cluster7 <- data.frame(clients[(ClusterKmeans$cluster==7),])
Cluster8 <- data.frame(clients[(ClusterKmeans$cluster==8),])
Cluster9 <- data.frame(clients[(ClusterKmeans$cluster==9),])

clients_plot <- clients

levelsIndustry <-
  levels(clients_plot$Datos.Industry)[-50][-49][-48]


#png(file="plot9.png")

ggplot(Cluster1,aes(Datos.Industry)) +
  geom_freqpoly(data=Cluster1,color = "green", alpha = 1)+
  geom_freqpoly(data=Cluster2,color = "red", alpha = 1)+
  geom_freqpoly(data=Cluster3,color = "black", alpha = 1)+
  geom_freqpoly(data=Cluster4,color = "blue", alpha = 1)+
  geom_freqpoly(data=Cluster5,color = "yellow", alpha = 1)+
  geom_freqpoly(data=Cluster6,color = "pink", alpha = 1)+
  geom_freqpoly(data=Cluster7,color = "orange", alpha = 1)+
  geom_freqpoly(data=Cluster8,color = "brown", alpha = 1)+
  geom_freqpoly(data=Cluster9,color = "black", alpha = 1)+
  scale_x_discrete(breaks=1:length(levelsIndustry),
                   labels=levelsIndustry)+
theme(axis.text.x = element_text(angle = 90, hjust = 1))



#Detectar cluster más poblado
Vectormayor<-nrow(data.frame(clients[(ClusterKmeans$cluster==1),]))
ClusterImportante<-data.frame(clients[(ClusterKmeans$cluster==1),])

for(i in 1:numcenters)
  {
 if(Vectormayor<=nrow(data.frame(clients[(ClusterKmeans$cluster==i),])))
   {
  Vectormayor<-nrow(data.frame(clients[(ClusterKmeans$cluster==i),]));
  ClusterImportante<-data.frame(clients[(ClusterKmeans$cluster==i),]);
  }
}

#UsuariosCluster<-ClusterImportante$Datos.Customer.Code;

#Cluster con picos más poblados (Primero ordenamos y luego seleccionamos)
Productos_cluster<-(sort((ClusterImportante$Datos.Product.Description)))
Productos_cluster<-as.numeric(names(which.max(table(Productos_cluster))))



#############
#Creamos una tabla
#ClusterFinal<-data.frame(Datos$Risk.Country,Datos$Customer.Code, Datos$Line.Of.Business, 
                         #Datos$Industry, Datos$Customer.Type, Datos$Profit.Center.Area, 
                         #Datos$Segment, Datos$Area, Datos$Product.Description,Datos$Base.Rate.Nominal,
                         #Datos$Spread.Rate.Nominal, Datos$Total.Rate.Nominal);

#for(i in )
#ClusterFinal <- filter(ClusterFinal, Datos.Customer.Code==UsuariosCluster);
############





#NumPrd variable que contiene cuantos productos parecidos hay
Numprd=matrix(0,nrow = length(Vectprd),ncol = 2)
#hay que guardar en un data.frame las empresas que solo tienen
#los productos por debajo del mejor
for(i in 1:length(Vectprd)){
  if(Vectprd[i]==L1[:]){#que podemos hacer para recorrer todo el vector sin saber cuantos valores hay?
    Numprd[1,1]=Numprd[1,1]+1;
  }
  if(Vectprd[i]==L2[:]){
    Numprd[1,1]=Numprd[1,1]+1;
  }
  #para todos los vecotores hay que hacer lo mismo
}



#Up-selling
#if(NumPrd >=2){
  #for(i in 1:length(Producto1)){
  #haria una recomendación para que los clientes que tienen
    #if(Producto1_Clientes$Datos.Customer.Code[i]=!Productos_Cliente2$Datos.Customer.Code[i] || Producto1_Clientes$Datos.Customer.Code[i]=!Productos_Cliente3$Datos.Customer.Code[i]){
    # up_sellings$Datos.Customer.Code[i]<-Producto2_Clientes$Datos.Customer.Code[i];
    # up_sellings$Datos.Customer.Code[i+length(Productos_Cliente2)]<-Producto3_Clientes$Datos.Customer.Code[i];
    # }
    #el producto con un mayor spread.rate
    #frase1<-("Se recomiendo realizar un UP-Selling des de los productos siguientes de la siguiente columna al producto que se encuentra a la tercera")
    #mostrará los dos productos siguientes ha recomendar en un arxivo xlm
    #excel<-data.frame(frase1,P1,P2, up_sellings$Datos.Customer.Code,)
    #excel <- excel[!duplicated(excel[,c('up_selling$Datos.Customer.Code')]),] 
    
    #write.xlsx(excel, "Empresas_aplicar_UpSll.xlsx", sheetName="Sheet1",col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)
    
    
#}



#Cross-selling

#comprobar que empresas del producto más poblado no tienen los dos siguientes
Clientes_Potenciales<-filter(ClusterImportante, Datos.Product.Description!=Productos_cluster)
#clientes que ya tienen el primero hay que quitarlos de la lista


    #mostrará los dos productos siguientes ha recomendar en un arxivo xlm
excel<-data.frame(Cross_Selling$Datos.Customer.Code,)
excel <- excel[!duplicated(excel[,c('Datos.Customer.Code')]),] 
write.xlsx(excel, "Empresas_aplicar_CrSll.xlsx", sheetName="Sheet1",col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)
    
    
    
########


