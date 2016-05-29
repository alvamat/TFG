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
plot <- paste(pab1, Pais, pab2,sep="")

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



#Bucle para determinar el C-S para cada uno de los Cluster
for(i in 1:numcenters){
  file<-toString(i)
  
  Vectormayor<-nrow(data.frame(clients[(ClusterKmeans$cluster==i),]));
  ClusterImportante<-data.frame(clients[(ClusterKmeans$cluster==i),]);
      
  
  #Comprovamos que valor tiene el más reptido
  Productos_cluster<-(sort((ClusterImportante$Datos.Product.Description)));
  Productos_cluster<-as.numeric(names(which.max(table(Productos_cluster))));
  Quitar=Productos_cluster;
  
  Rep_num<-(filter(ClusterImportante, Datos.Product.Description==Productos_cluster));
  Freq=1/nrow(Rep_num);
  
  Spread_Rate<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster);
  S=1/Spread_Rate$clients.Datos.Spread.Rate.Nominal;
  
  
  norma<-c(Freq,S);
  if(norma[2]==Inf){
    P=1000;
  }else{
    P=norm(norma, type="2");
  }
  
  
  
  #Hacemos lo mismo para los demás productos del vector
  
  
  t=0;
  Productos_cluster1<-data.frame(sort((ClusterImportante$Datos.Product.Description)));
  Longitud<-(sort(table(ClusterImportante$Datos.Product.Description)));
  if((!is.null(length(Longitud)) && length(Longitud)>1)){
    Long=length(Longitud)-1;
  }else{
    Long=0;
  }
  
  
  if(Long==0){
    excel2<-data.frame(ClusterImportante,Spread_Rate)
    write.csv(excel2, file="No_es_valido_CS",na="NA", row.names=TRUE)
  }else{
    
    while(Long>=1){
      
      Productos_cluster1<-filter(Productos_cluster1,Productos_cluster1$sort..ClusterImportante.Datos.Product.Description..!=Quitar)
      Productos_cluster2<-as.numeric(names(which.max(table(Productos_cluster1))));
      Quitar=Productos_cluster2;
      
      Rep_num<-(filter(ClusterImportante, Datos.Product.Description==Productos_cluster2));
      R=1/nrow(Rep_num);
      
      Spread_Rate<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster2);
      S=1/Spread_Rate$clients.Datos.Spread.Rate.Nominal;
      
      norma<-c(Freq,S);
      
      if(norma[2]==Inf){
        P1=1000;
      }else{
        P1=norm(norma, type="2");
      }
      
      
      if(P1<=P){
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
      
      Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Producto)
      Spread=Clientes_Potenciales$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate
      
      
      excel2<-data.frame(Clientes_Potenciales,Spread)
      
      colnames(excel2)=c('País','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual','Total Rate Nominal Actual', 'Spread Rate Nominal Futuro')
      
      pab1 <- "Clientes C-S"
      pab2 <- file
      union1 <- paste(pab1, Pais, pab2,sep="_")
      write.csv(excel2, file=union1,na="NA", row.names=TRUE)
      pab1 <- "producto para"
      pab2 <- file
      union2 <- paste(pab1, Pais, pab2,sep="_")
      write.csv(Producto_mayor, file=union2,na="NA", row.names=TRUE)
    
    }else{
      Clientes_Potenciales<-filter(ClusterImportante, Datos.Product.Description!=Producto & Datos.Product.Description==Productos_cluster);
      
      Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Producto)
      Spread=Clientes_Potenciales$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate
      
      
      excel2<-data.frame(Clientes_Potenciales,Spread)
      
      colnames(excel2)=c('País','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual','Total Rate Nominal Actual', 'Spread Rate Nominal Futuro')
      
      pab1 <- "Clientes C-S"
      pab2 <- file
      union1 <- paste(pab1, Pais, pab2,sep="_")
      write.csv(excel2, file=union1,na="NA", row.names=TRUE)
      pab1 <- "producto para"
      pab2 <- file
      union2 <- paste(pab1, Pais, pab2,sep="_")
      write.csv(Producto_mayor, file=union2,na="NA", row.names=TRUE)
    }
  }

}



#necesitamos que todo sea numérico


Cluster1n<-Cluster1[,-2]
Cluster1n$Datos.Risk.Country<-as.numeric(Cluster1n$Datos.Risk.Country)
Cluster2n<-Cluster2[,-2]
Cluster2n$Datos.Risk.Country<-as.numeric(Cluster2n$Datos.Risk.Country)
Cluster3n<-Cluster3[,-2]
Cluster3n$Datos.Risk.Country<-as.numeric(Cluster3n$Datos.Risk.Country)
Cluster4n<-Cluster4[,-2]
Cluster4n$Datos.Risk.Country<-as.numeric(Cluster4n$Datos.Risk.Country)
Cluster5n<-Cluster5[,-2]
Cluster5n$Datos.Risk.Country<-as.numeric(Cluster5n$Datos.Risk.Country)

train=rbind(Cluster1n,Cluster2n,Cluster3n, Cluster4n,Cluster5n)

# Class labels vector (attached to each class instance)
cl=factor(c(rep(1,nrow(Cluster1n)),rep(2,nrow(Cluster2n)),rep(3,nrow(Cluster3n)),rep(4,nrow(Cluster4n)),rep(5,nrow(Cluster5n))))

# The object to be classified
test=c(1,
       3,
       12,
       1,
       5,
       10,
       10,
       43,
       0.03172,
       0.30)

# Load the class package that holds the knn() function
library(class)

# call knn() and get its summary
resultado<-(knn(train, test, cl, k = 1))
attributes(.Last.value)

pab1 <- "producto para"
pab2 <- resultado
union3 <- paste(pab1, Pais, pab2,sep="_")
nuevo<-read.csv(file=union3,sep=",", header=TRUE)

pab1 <- "Al nuevo cliente se le recomienda el C-S del Cluster"
pab2 <- resultado
union4 <- paste(pab1, pab2,sep="_")
write.csv(nuevo, file=union4, na="NA", row.names=TRUE)


proc.time() - ptm