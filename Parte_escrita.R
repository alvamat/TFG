ptm <- proc.time()
library(dplyr)
library(ggplot2)
library(cluster)
library(rJava)
library(xlsx)
library(Rmixmod)





#Programa para la lectura de datos csv en R

#Empezamos leyendo el documento del cual vamos a extraer los datos:
Datos<-read.csv("PublicUSefulBankDataFurtherReduced.txt",sep=",",header=TRUE)

#Para que pa�s en concreto? EN MAYUS
Pais<-"UNITED STATES"

#seleccionamos aquellas variables que usaremos para juntar empresas seg�n similitud
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

carga<-proc.time() - ptm
#----------------------------------------------------------------------------------------------------------------------v1
ptm <- proc.time()

for(i in 1:numcenters){
  file<-toString(i)
  #Detectar cluster m�s poblado
  Vectormayor<-nrow(data.frame(clients[(ClusterKmeans$cluster==i),]))
  ClusterImportante<-data.frame(clients[(ClusterKmeans$cluster==i),])
  

  
  #UsuariosCluster<-ClusterImportante$Datos.Customer.Code;
  
  #Cluster con picos m�s poblados (Primero ordenamos y luego seleccionamos)
  Productos_cluster<-(sort((ClusterImportante$Datos.Product.Description)));
  Productos_cluster<-as.numeric(names(which.max(table(Productos_cluster))));
  Spread_Rate<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster);
  
  Segundo_producto<-(filter(ClusterImportante, Datos.Product.Description!=Productos_cluster));
  
  Rep_num<-(filter(ClusterImportante, Datos.Product.Description==Productos_cluster));
  valorrep <- nrow(Rep_num)
  
  if(length(Segundo_producto)==0){
    excel2<-data.frame(ClusterImportante,Spread_Rate)
    write.csv(excel2, file="No_es_valido_CS",na="NA", row.names=TRUE)
  }else{
    Segundo_producto<-as.numeric(names(which.max(table(sort(Segundo_producto$Datos.Product.Description)))));
    
    Tercero_producto<-(filter(ClusterImportante, Datos.Product.Description!=Segundo_producto & Datos.Product.Description!=Productos_cluster));
    Tercero_producto<-as.numeric(names(which.max(table(sort(Tercero_producto$Datos.Product.Description)))));
    
    
    
    
    #Cross-selling
    
    #comprobar que empresas del producto m�s poblado no tienen los dos siguientes
    Clientes_Potenciales_P2<-filter(ClusterImportante, Datos.Product.Description!=Productos_cluster & Datos.Product.Description==Segundo_producto);
    Clientes_Potenciales_P3<-filter(ClusterImportante, Datos.Product.Description!=Productos_cluster & Datos.Product.Description==Tercero_producto);
    #clientes que ya tienen el primero hay que quitarlos de la lista
    
    
    #mostrar� los dos productos siguientes ha recomendar en un arxivo xlm
    #2
    Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster)
    Spread=Clientes_Potenciales_P2$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate
    
    
    excel2<-data.frame(Clientes_Potenciales_P2)
    colnames(excel2)=c('Pa�s','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual', 'Spread Rate Nominal Futuro')
    
    pab1 <- "V1Clientes C-S"
    pab2 <- file
    pab3 <- "Con producto2"
    union1 <- paste(pab1, Pais, pab2, pab3,sep="_")
    write.csv(excel2, file=union1,na="NA", row.names=TRUE)
    pab1 <- "V1Producto para"
    pab2 <- file
    pab3 <- "Con producto2"
    union2 <- paste(pab1, Pais, pab2, pab3,sep="_")
    ProdMay<-data.frame(Producto_mayor,valorrep)
    write.csv(ProdMay, file=union2,na="NA", row.names=TRUE)
    
    #3
    Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster)
    Spread=Clientes_Potenciales_P3$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate
    
    
    excel3<-data.frame(Clientes_Potenciales_P3,Spread)
    
    colnames(excel3)=c('Pa�s','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual','Spread Rate Nominal Futuro')
    
    pab1 <- "V1Clientes C-S"
    pab2 <- file
    pab3 <- "Con producto3"
    union1 <- paste(pab1, Pais, pab2,pab3,sep="_")
    write.csv(excel3, file=union1,na="NA", row.names=TRUE)
    pab1 <- "V1Producto para"
    pab2 <- file
    pab3 <- "Con producto3"
    union2 <- paste(pab1, Pais, pab2, pab3,sep="_")
    ProdMay<-data.frame(Producto_mayor,valorrep)
    write.csv(ProdMay, file=union2,na="NA", row.names=TRUE)
  }

}

v1<-proc.time() - ptm

#----------------------------------------------------------------------------------------------------------v2

a<-1;
b<-1;
ptm <- proc.time()

for(i in 1:numcenters){
  file<-toString(i)
  
  Vectormayor<-nrow(data.frame(clients[(ClusterKmeans$cluster==i),]));
  ClusterImportante<-data.frame(clients[(ClusterKmeans$cluster==i),]);
  
  
  #Comprovamos que valor tiene el m�s reptido
  Productos_cluster<-(sort((ClusterImportante$Datos.Product.Description)));
  Productos_cluster<-as.numeric(names(which.max(table(Productos_cluster))));
  Quitar=Productos_cluster;
  
  Rep_num<-(filter(ClusterImportante, Datos.Product.Description==Productos_cluster));
  R=nrow(Rep_num)/nrow(ClusterImportante);
  
  Spread_Rate<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster);
  S=Spread_Rate$clients.Datos.Spread.Rate.Nominal;
  
  
  P=(R*(0.65*a)+S*(0.35*b));
  Producto=Quitar;
  pob_mej=1;
  valorrep<-nrow(Rep_num)
  
  #Hacemos lo mismo para los dem�s productos del vector
  
  
  t=0;
  Productos_cluster1<-data.frame(sort((ClusterImportante$Datos.Product.Description)));
  Longitud<-(sort(table(ClusterImportante$Datos.Product.Description)));
  if((!is.null(length(Longitud)) && length(Longitud)>1)){
    Long=length(Longitud)-1;
  }else{
    Long=0;
  }
  
  
  if(Long==0){
    pab1 <- "V2No es valido C-S"
    pab2 <- file
    union1 <- paste(pab1, Pais, pab2,sep="_")
    excel2<-data.frame(ClusterImportante,Spread_Rate)
    write.csv(excel2, file=union1,na="NA", row.names=TRUE)
  }else{
    
    while(Long>=1){
      
      Productos_cluster1<-filter(Productos_cluster1,Productos_cluster1$sort..ClusterImportante.Datos.Product.Description..!=Quitar)
      Productos_cluster2<-as.numeric(names(which.max(table(Productos_cluster1))));
      Quitar=Productos_cluster2;
      
      Rep_num<-(filter(ClusterImportante, Datos.Product.Description==Productos_cluster2));
      R=nrow(Rep_num)/nrow(ClusterImportante);
      
      Spread_Rate<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster2);
      S=Spread_Rate$clients.Datos.Spread.Rate.Nominal;
      
      P1=(R*(0.65*a)+S*(0.35*b));
      
      if(P1>=P){
        P=P1
        Producto=Quitar;
        pob_mej=t;
        valorrep<-nrow(Rep_num)
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
      
      colnames(excel2)=c('Pa�s','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual','Total Rate Nominal Actual', 'Spread Rate Nominal Futuro')
      
      pab1 <- "V2No es posible C-S"
      pab2 <- file
      union1 <- paste(pab1, Pais, pab2,sep="_")
      write.csv(excel2, file=union1,na="NA", row.names=TRUE)
      pab1 <- "V2Producto para"
      pab2 <- file
      union2 <- paste(pab1, Pais, pab2,sep="_")
      ProdMay<-data.frame(Producto_mayor,valorrep)
      write.csv(ProdMay, file=union1,na="NA", row.names=TRUE)
      
    }else{
      Clientes_Potenciales<-filter(ClusterImportante, Datos.Product.Description!=Producto & Datos.Product.Description==Productos_cluster);
      
      Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Producto)
      Spread=Clientes_Potenciales$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate
      
      
      excel2<-data.frame(Clientes_Potenciales,Spread)
      
      colnames(excel2)=c('Pa�s','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual','Total Rate Nominal Actual', 'Spread Rate Nominal Futuro')
      
      pab1 <- "V2Clientes C-S"
      pab2 <- file
      union1 <- paste(pab1, Pais, pab2,sep="_")
      write.csv(excel2, file=union1,na="NA", row.names=TRUE)
      pab1 <- "V2Producto para"
      pab2 <- file
      union2 <- paste(pab1, Pais, pab2,sep="_")
      ProdMay<-data.frame(Producto_mayor,valorrep)
      write.csv(ProdMay, file=union2,na="NA", row.names=TRUE)
    }
  }
  
}

v2<-proc.time() - ptm

#-----------------------------------------------------------------------------------------------------------v3
ptm <- proc.time()


#Bucle para determinar el C-S para cada uno de los Cluster
for(i in 1:numcenters){
  normaplot<-matrix(data=NA, ncol = 2, nrow=50);
  k=1;
  
  file<-toString(i)
  
  Vectormayor<-nrow(data.frame(clients[(ClusterKmeans$cluster==i),]));
  ClusterImportante<-data.frame(clients[(ClusterKmeans$cluster==i),]);
  
  
  #Comprovamos que valor tiene el m�s reptido
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
    normaplot[k,1]<-norma[1];
    normaplot[k,2]<-norma[2];
    valorrep<-nrow(Rep_num)
  }
  
  
  
  #Hacemos lo mismo para los dem�s productos del vector
  
  
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
      Freq=1/nrow(Rep_num);
      
      Spread_Rate<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster2);
      S=1/Spread_Rate$clients.Datos.Spread.Rate.Nominal;
      
      norma<-c(Freq,S);
      
      
      if(norma[2]==Inf){
        P1=1000;
      }else{
        P1=norm(norma, type="2");
        normaplot[k,1]<-norma[1];
        normaplot[k,2]<-norma[2];
        k=k+1;
      }
      
      
      if(P1<=P){
        P=P1
        Producto=Quitar;
        pob_mej=t;
        valorrep<-nrow(Rep_num)
      }
      
      Long=Long-1;
      t=t+1;
      
      
    }
    
    
    pab1 <- "OptMultiobj"
    pab2 <- file
    pab3 <- ".png"
    plot <- paste(pab1, Pais, pab2, pab3,sep="_")
    
    pab1 <- "Optimizaci�n Multiobjeto"
    pab2 <- file
    pab4 <-"cluster"
    plottitu <- paste(pab1, Pais, pab4,pab2, pab3,sep="_")
    
    png(plot)
    plot(normaplot, xlab="1/(Frecuencia en el Cluster)", ylab="1/(Spread Rate)", col="blue" ,main=plottitu)
    dev.off();
    #Generamos fichero
    
    if(pob_mej==1){
      Segundo_producto<-(filter(ClusterImportante, Datos.Product.Description!=Productos_cluster));
      Segundo_producto<-as.numeric(names(which.max(table(sort(Segundo_producto$Datos.Product.Description)))));
      
      Clientes_Potenciales<-filter(ClusterImportante, Datos.Product.Description!=Producto & Datos.Product.Description==Segundo_producto);
      
      Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Producto)
      Spread=Clientes_Potenciales$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate
      
      
      excel2<-data.frame(Clientes_Potenciales,Spread)
      
      colnames(excel2)=c('Pa�s','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual','Total Rate Nominal Actual', 'Spread Rate Nominal Futuro')
      
      pab1 <- "V3No es posible C-S"
      pab2 <- file
      union1 <- paste(pab1, Pais, pab2,sep="_")
      write.csv(excel2, file=union1,na="NA", row.names=TRUE)
      pab1 <- "V3Producto para"
      pab2 <- file
      union2 <- paste(pab1, Pais, pab2,sep="_")
      ProdMay<-data.frame(Producto_mayor,valorrep)
      write.csv(ProdMay, file=union2,na="NA", row.names=TRUE)
      
    }else{
      Clientes_Potenciales<-filter(ClusterImportante, Datos.Product.Description!=Producto & Datos.Product.Description==Productos_cluster);
      
      Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Producto)
      Spread=Clientes_Potenciales$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate
      
      
      excel2<-data.frame(Clientes_Potenciales,Spread)
      
      colnames(excel2)=c('Pa�s','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual','Total Rate Nominal Actual', 'Spread Rate Nominal Futuro')
      
      pab1 <- "V3Clientes C-S"
      pab2 <- file
      union1 <- paste(pab1, Pais, pab2,sep="_")
      write.csv(excel2, file=union1,na="NA", row.names=TRUE)
      pab1 <- "V3Producto para"
      pab2 <- file
      union2 <- paste(pab1, Pais, pab2,sep="_")
      ProdMay<-data.frame(Producto_mayor,valorrep)
      write.csv(ProdMay, file=union2,na="NA", row.names=TRUE)
    }
  }
  
  
  
}

v3<-proc.time() - ptm
 