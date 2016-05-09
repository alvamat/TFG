library(dplyr)
library(ggplot2)
library(cluster)
library(rJava)
library(xlsx)
library(Rmixmod)

#Qué programas queremos usar? a=Sólo repeticiones; b=sólo spread rate
a=1;
b=1;

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


clients <- filter(clients, Datos.Risk.Country=="GUATEMALA");





#hacemos los clusters
dat<-select(clients,Datos.Customer.Type, Datos.Line.Of.Business, 
            Datos.Industry, Datos.Segment, Datos.Profit.Center.Area,Datos.Area)

dat<-scale(dat)

set.seed(3)
numcenters = 3;
ClusterKmeans<-kmeans(dat,numcenters,iter.max=10,algorithm = "Forgy")
#with(clients, pairs(dat, col=c(1:20)[ClusterKmeans$cluster])) 

#plotcluster(dat, clients$cluster)
#clusplot(dat, ClusterKmeans$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

Cluster1 <- data.frame(clients[(ClusterKmeans$cluster==1),])
Cluster2 <- data.frame(clients[(ClusterKmeans$cluster==2),])
Cluster3 <- data.frame(clients[(ClusterKmeans$cluster==3),])
Cluster4 <- data.frame(clients[(ClusterKmeans$cluster==4),])
Cluster5 <- data.frame(clients[(ClusterKmeans$cluster==5),])


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


#Comprovamos que valor tiene el más reptido
Productos_cluster<-(sort((ClusterImportante$Datos.Product.Description)));
Productos_cluster<-as.numeric(names(which.max(table(Productos_cluster))));
Quitar=Productos_cluster;

Rep_num<-(filter(ClusterImportante, Datos.Product.Description==Productos_cluster));
R=nrow(Rep_num)/nrow(ClusterImportante);

Spread_Rate<-filter(excel, excel$Datos.Product.Description1==Productos_cluster);
S=Spread_Rate$Datos.Spread.Rate.Nominal;


P=(R*(0.65*a)+S*(0.35*b));
Producto=Quitar;
pob_mej=1;


#Hacemos lo mismo para los demás productos del vector


t=0;
Productos_cluster1<-data.frame(sort((ClusterImportante$Datos.Product.Description)));
Longitud<-(sort(table(ClusterImportante$Datos.Product.Description)));
Long=dim(Longitud)-1;

while(Long>=1){
    
  Productos_cluster1<-filter(Productos_cluster1,Productos_cluster1$sort..ClusterImportante.Datos.Product.Description..!=Quitar)
  Productos_cluster2<-as.numeric(names(which.max(table(Productos_cluster1))));
  Quitar=Productos_cluster2;
  
  Rep_num<-(filter(ClusterImportante, Datos.Product.Description==Productos_cluster2));
  R=nrow(Rep_num)/nrow(ClusterImportante);
  
  Spread_Rate<-filter(excel, excel$Datos.Product.Description1==Productos_cluster2);
  S=Spread_Rate$Datos.Spread.Rate.Nominal;
  
  P1=(R*(0.65*a)+S*(0.35*b));
  
  if(P1>=P){
    P=P1
    Producto=Quitar;
    pob_mej=t;
    }
  
  Long=Long-1;
  t=t+1;
  
  
}



#Generamos fichero

if(pob_mej==1){
  Segundo_producto<-(filter(ClusterImportante, Datos.Product.Description!=Productos_cluster));
  Segundo_producto<-as.numeric(names(which.max(table(sort(Segundo_producto$Datos.Product.Description)))));
  
  Clientes_Potenciales<-filter(ClusterImportante, Datos.Product.Description!=Producto & Datos.Product.Description==Segundo_producto);
  
  Producto_mayor<-filter(excel, excel$Datos.Product.Description1==Producto)
  Spread=Clientes_Potenciales$Datos.Spread.Rate.Nominal+Producto_mayor$Datos.Spread.Rate
  
  
  excel2<-data.frame(Clientes_Potenciales,Spread)
  
  colnames(excel2)=c('País','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual','Total Rate Nominal Actual', 'Spread Rate Nominal Futuro')
  
  write.csv(excel2, file="Empresas_aplicar_Cs_v2_2pob.csv",na="NA", row.names=TRUE)

}else{
  Clientes_Potenciales<-filter(ClusterImportante, Datos.Product.Description!=Producto & Datos.Product.Description==Productos_cluster);
  
  Producto_mayor<-filter(excel, excel$Datos.Product.Description1==Producto)
  Spread=Clientes_Potenciales$Datos.Spread.Rate.Nominal+Producto_mayor$Datos.Spread.Rate
  
  
  excel2<-data.frame(Clientes_Potenciales,Spread)
  
  colnames(excel2)=c('País','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual','Total Rate Nominal Actual', 'Spread Rate Nominal Futuro')
  
  write.csv(excel2, file="Empresas_aplicar_Cs_v2_2pob.csv",na="NA", row.names=TRUE)
  
}



