ptm <- proc.time()
library(dplyr)
library(ggplot2)
library(cluster)
library(rJava)
library(xlsx)
library(Rmixmod)



#Para que país en concreto? EN MAYUS
Pais<-"ARGENTINA"

#Programa para la lectura de datos csv en R

#Empezamos leyendo el documento del cual vamos a extraer los datos:
Datos<-read.csv("PublicUSefulBankDataFurtherReduced.txt",sep=",",header=TRUE)

#seleccionamos aquellas variables que usaremos para juntar empresas según similitud
clients <- data.frame(Datos$Risk.Country,Datos$Customer.Code, Datos$Line.Of.Business, 
                      Datos$Industry, Datos$Customer.Type, Datos$Profit.Center.Area, 
                      Datos$Segment, Datos$Area, Datos$Product.Description, Datos$Spread.Rate.Nominal, Datos$Total.Rate.Nominal)

clients <- filter(clients, Datos.Risk.Country==Pais);

clients[is.na(clients)] <- 0

#creamos un fichero para saber que producto tiene cada valor
clientse<-data.frame(clients$Datos.Product.Description, clients$Datos.Spread.Rate.Nominal,clients$Datos.Product.Description)

clientse$clients.Datos.Product.Description<-as.numeric(clientse$clients.Datos.Product.Description)
clients$Datos.Product.Description<-clientse$clients.Datos.Product.Description

excel<-aggregate( formula = clients.Datos.Spread.Rate.Nominal~clients.Datos.Product.Description+clients.Datos.Product.Description.1, 
                  data = clientse,
                  FUN = mean);

write.xlsx(excel, "Productos_Bancarios.xlsx", sheetName="Sheet1",
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)



clients$Datos.Customer.Type<-as.numeric(clients$Datos.Customer.Type)
clients$Datos.Line.Of.Business<-as.numeric(clients$Datos.Line.Of.Business)
clients$Datos.Industry<-as.numeric(clients$Datos.Industry)
clients$Datos.Area<-as.numeric(clients$Datos.Area)
clients$Datos.Profit.Center.Area<-as.numeric(clients$Datos.Profit.Center.Area)
clients$Datos.Segment<-as.numeric(clients$Datos.Segment)


#hacemos los clusters
dat<-select(clients,Datos.Customer.Type, Datos.Line.Of.Business, 
            Datos.Industry, Datos.Segment, Datos.Profit.Center.Area,Datos.Area)

dat<-scale(dat)

set.seed(3)
numcenters = 5;
ClusterKmeans<-kmeans(dat,numcenters,iter.max=10,algorithm = "Forgy")


Cluster1 <- data.frame(clients[(ClusterKmeans$cluster==1),])
Cluster2 <- data.frame(clients[(ClusterKmeans$cluster==2),])
Cluster3 <- data.frame(clients[(ClusterKmeans$cluster==3),])
Cluster4 <- data.frame(clients[(ClusterKmeans$cluster==4),])
Cluster5 <- data.frame(clients[(ClusterKmeans$cluster==5),])


clients_plot <- clients

levelsIndustry <-
  levels(clients_plot$Datos.Industry)[-50][-49][-48]

pab1 <- "Histograma"
pab2 <- ".png"
plot <- paste(pab1, Pais, pab3,sep="")

ggplot(Cluster1,aes(Datos.Industry)) +
  geom_freqpoly(data=Cluster1,color = "green", alpha = 1, binwidth = 0.5)+
  geom_freqpoly(data=Cluster2,color = "red", alpha = 1, binwidth = 0.5)+
  geom_freqpoly(data=Cluster3,color = "black", alpha = 1, binwidth = 0.5)+
  geom_freqpoly(data=Cluster4,color = "blue", alpha = 1, binwidth = 0.5)+
  geom_freqpoly(data=Cluster5,color = "yellow", alpha = 1, binwidth = 0.5)+
  scale_x_discrete(breaks=1:length(levelsIndustry),
                   labels=levelsIndustry)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(plot)

#Detectar cluster más poblado
Vectormayor<-nrow(data.frame(clients[(ClusterKmeans$cluster==1),]))
ClusterImportante<-data.frame(clients[(ClusterKmeans$cluster==1),])

for(i in 1:numcenters)
  {
 if(Vectormayor<=nrow(data.frame(clients[(ClusterKmeans$cluster==i),])))
   {
  Vectormayor<-nrow(data.frame(clients[(ClusterKmeans$cluster==i),]));
  ClusterImportante<-data.frame(clients[(ClusterKmeans$cluster==i),]);
  cnum<-i;
  }
}

#UsuariosCluster<-ClusterImportante$Datos.Customer.Code;

#Cluster con picos más poblados (Primero ordenamos y luego seleccionamos)
Productos_cluster<-(sort((ClusterImportante$Datos.Product.Description)));
Productos_cluster<-as.numeric(names(which.max(table(Productos_cluster))));

Segundo_producto<-(filter(ClusterImportante, Datos.Product.Description!=Productos_cluster));
Segundo_producto<-as.numeric(names(which.max(table(sort(Segundo_producto$Datos.Product.Description)))));

Tercero_producto<-(filter(ClusterImportante, Datos.Product.Description!=Segundo_producto & Datos.Product.Description!=Productos_cluster));
Tercero_producto<-as.numeric(names(which.max(table(sort(Tercero_producto$Datos.Product.Description)))));




#Cross-selling

#comprobar que empresas del producto más poblado no tienen los dos siguientes
Clientes_Potenciales_P2<-filter(ClusterImportante, Datos.Product.Description!=Productos_cluster & Datos.Product.Description==Segundo_producto);
Clientes_Potenciales_P3<-filter(ClusterImportante, Datos.Product.Description!=Productos_cluster & Datos.Product.Description==Tercero_producto);
#clientes que ya tienen el primero hay que quitarlos de la lista


#mostrará los dos productos siguientes ha recomendar en un arxivo xlm
#2
Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster)
Spread=Clientes_Potenciales_P2$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate

excel2<-data.frame(Clientes_Potenciales_P2)
colnames(excel2)=c('País','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual', 'Spread Rate Nominal Futuro')

write.csv(excel2, file='Empresas_aplicar_Cs_con_Prodcuto2',na="NA", row.names=TRUE)
write.csv(Producto_mayor, file='productov1.2',na="NA", row.names=TRUE)

#3
Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster)
Spread=Clientes_Potenciales_P3$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate


excel3<-data.frame(Clientes_Potenciales_P3,Spread)

colnames(excel3)=c('País','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual','Spread Rate Nominal Futuro')

write.csv(excel3, file='Empresas_aplicar_Cs_con_Prodcuto3',na="NA", row.names=TRUE)
write.csv(Producto_mayor, file='productov1.3',na="NA", row.names=TRUE)


proc.time() - ptm
