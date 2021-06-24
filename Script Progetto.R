##### PUNTO 1: ESTRAZIONE DEL CAMPIONE CASUALE E SALVATAGGIO #####

library(readxl)
library(tidyverse)

table<-read_xlsx("tvprices.xlsx",sheet=4)
sample<-sample(1:40, size=15)
dati<-table[sample,]
write_csv(dati,"campione.csv")

# Si poteva mettere un set.seed piuttosto di salvare il file con il campione utilizzato per l'analisi

##### PUNTO 2: APPLICAZIONE DEL METODO MDS NON METRICO #####

library(tidyverse)

# Lettura dei dati da file contente il campione estratto precedentemente
dati_orig<-read.csv("campione.csv",sep=",",dec=".",header=T)

# Estrapoliamo i dati di interesse per tale analisi
mat_dati<-dati_orig[,3:7]

# Trasformiamo la variabile NatRes per renderla politoma, cambiando anche etichetta
mat_dati<- mat_dati %>%
  mutate(NatRes=
           recode(NatRes,
                  "720"="HD",
                  "1080"="Full HD"
           )
  )
mat_dati[,4]<-as.factor(mat_dati[,4])

# Calcoliamo la matrice contenente la misura di prossimità tra i differenti modelli di televisione 
library(StatMatch)
dist<-gower.dist(mat_dati) # Si è scelto di utilizzare la distanza di Gower
write_csv(as.data.frame(dist),"DistanzaGower.csv") 
# Abbiamo creato un file con tale matrice di dissimilarità per utilizzarla su SPSS

# Verifichiamo se è rispettata la disuguaglianza euclidea all'interno di tale matrice
# Non dovrebbe essere rispettata in quanto la distanza di Gower è una misura di dissimilarità
library(ade4)
is.euclid(as.dist(dist))

library(MASS)
MDS<-isoMDS(dist) #Configurazione iniziale: coordinate da scaling metrico
plot(MDS$points,type="n",xlab="Dimensione 1",ylab="Dimensione 2")
abline(h=0,v=0)
text(MDS$points,labels=dati_orig[,1])

set.seed(12345)
mat<-matrix(rnorm(30,0,1),15,2)
MDS_r1<-isoMDS(dist,y=mat) #Configurazione iniziale: coordinate randomiche
plot(MDS_r1$points,type="n",xlab="Dimensione 1",ylab="Dimensione 2")
abline(h=0,v=0)
text(MDS_r1$points,labels=dati_orig[,1])

set.seed(3.14)
mat<-matrix(rnorm(30,0,1),15,2)
MDS_r2<-isoMDS(dist,y=mat) #Configurazione iniziale: coordinate randomiche
plot(MDS_r2$points,type="n",xlab="Dimensione 1",ylab="Dimensione 2")
abline(h=0,v=0)
text(MDS_r2$points,labels=dati_orig[,1])

library(smacof)
MDS_smacof<-mds(dist); MDS_smacof #Configurazione iniziale: coordinate da scaling metrico
plot(MDS_smacof$conf,type="n",xlab="Dimensione 1",ylab="Dimensione 2")
abline(h=0,v=0)
text(MDS_smacof$conf,labels=dati_orig[,1])

she<-Shepard(as.dist(dist),MDS$points,2)
plot(she$yf,she$y)
cor(she$yf,she$y)^2

she2<-Shepard(as.dist(dist),MDS_r1$points,2)
plot(she2$yf,she2$y)
cor(she2$yf,she2$y)^2

she3<-Shepard(as.dist(dist),MDS_r2$points,2)
plot(she3$yf,she3$y)
cor(she3$yf,she3$y)^2

she4<-Shepard(as.dist(dist),MDS_smacof$conf,2)
plot(she4$yf,she4$y)
cor(she4$yf,she4$y)^2

# Mappa percettiva a tre dimensioni
MDS_3d<-isoMDS(dist,k=3)
library(rgl)
plot3d(MDS_3d$points,xlab="dim1",ylab="dim2",zlab="dim3")
grid3d(side="x",at=list(x=0))
grid3d(side="z",at=list(z=0))
text3d(MDS_3d$points,text=dati_orig[,1],cex=1)

##### SCRIPT PER GRAFICI SLIDE #####

library(ggplot2)
library(ggrepel)
library(ggthemes)

attach(dati_orig)

coord<-as.data.frame(MDS$points)
labels<-paste("Modello: ",dati_orig[,1],"\n",Screen," pollici","\nDCR= ",DCR,"\nLCD.Hz= ",
              LCD.Hz,"\nRisoluzione: ",NatRes,"\nPrezzo: ",Price,sep="")

# Mappa per slide numero 6 e 7
perc_map<-ggplot(coord,aes(x=V1,y=V2))+
  geom_hline(yintercept=0,color="#E68E35",size=2)+
  geom_vline(xintercept = 0,color="#E68E35",size=2)+
  geom_point(colour="#7A98EE",size=4)+
  geom_label_repel(aes(label=labels),color="#DD517E",label.padding=0.5,label.size=1.1,size=2.2,segment.color="#7A98EE",segment.size=1.5,force=30,box.padding=0.5)+
  labs(title="\nMappa percettiva delle televisioni in commercio\n",x="\nDimensione 1",y="Dimensione 2\n")+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5,size=30,colour="white"),
        axis.title=element_text(size=20,colour="white"),
        axis.line=element_line(color="white"),
        axis.text=element_text(size=10,color="white"),
        plot.background=element_rect(fill="#481D52"),
        panel.grid.minor=element_line(colour="#481D52"),
  )
perc_map
ggsave(plot=perc_map,"Mappa percettiva.png",width=33,height=19,units="cm",dpi=300)

Vi<-"#481D52"
Fu<-"#DD517E"
Az<-"#7A98EE"
Ar<-"#E68E35"

# mappa slide numero 8
palette<-c(Ar,Ar,Fu,Az,Ar,Fu,Fu,Ar,Fu,Fu,Az,Az,Fu,Fu,Ar)
perc_map2<-ggplot(coord,aes(x=V1,y=V2))+
  geom_hline(yintercept=0,color="#E68E35",size=2)+
  geom_vline(xintercept = 0,color="#E68E35",size=2)+
  geom_label_repel(aes(label=labels),color=palette,label.padding=0.5,label.size=1.1,size=2.2,segment.color="white",segment.size=1.5,force=30,box.padding=0.5)+
  labs(title="\nMappa percettiva delle televisioni in commercio\n",x="\nDimensione 1",y="Dimensione 2\n")+
  geom_point(colour=palette,size=4)+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5,size=30,colour="white"),
        axis.title=element_text(size=20,colour="white"),
        axis.line=element_line(color="white"),
        axis.text=element_text(size=10,color="white"),
        plot.background=element_rect(fill="#481D52"),
        panel.grid.minor=element_line(colour="#481D52"),
  )
perc_map2
ggsave(plot=perc_map2,"Mappa percettiva2.png",width=33,height=19,units="cm",dpi=300)

labels2<-paste("Modello: ",dati_orig[,1],"\n\nQualità dell'immagine\n percipita sul prezzo:\n",round(((DCR*LCD.Hz)/Screen)/Price,4))

# mappa slide numero 9
perc_map3<-ggplot(coord,aes(x=V1,y=V2))+
  geom_hline(yintercept=0,color="#E68E35",size=2)+
  geom_vline(xintercept = 0,color="#E68E35",size=2)+
  geom_point(colour="#7A98EE",size=4)+
  geom_label_repel(aes(label=labels2),color="#DD517E",label.padding=0.5,label.size=1.3,size=2.8,segment.color="#7A98EE",force=10,segment.size=1.5,box.padding=0.5)+
  labs(title="\nMappa percettiva delle televisioni in commercio\n",x="\nFascia del televisore\n",y=bquote("\n"~frac(frac(DCR*"\u00D7"*LCD.Hz,Screen),Price)))+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5,size=25,colour="white"),
        axis.title=element_text(size=15,colour="white"),
        axis.line=element_line(color="white"),
        axis.text=element_text(size=8,color="white"),
        plot.background=element_rect(fill="#481D52"),
        panel.grid.minor=element_line(colour="#481D52"),
  )
perc_map3
ggsave(plot=perc_map3,"Mappa percettiva3.png",width=33,height=19,units="cm",dpi=600)

##### PUNTO 3: APPLICAZIONE DEL METODO DELL'ANALISI DELLE CORRISPONDENZE #####

library(tidyverse)
library(readxl)
d_orig<-read_xlsx("tvprices.xlsx",sheet=4)

summary(filter(d_orig,Categoria==1))
summary(filter(d_orig,Categoria==2))
summary(filter(d_orig,Categoria==3))
summary(filter(d_orig,Categoria==4))


dati_orig<-read.csv("campione.csv",sep=",",dec=".",header=T)
dati<-dati_orig[,c(2,8:14)]
count_table<-aggregate(dati[-1],list(dati[,1]),sum)
count_table<-count_table[,-1]
colnames(count_table)<-c("Maschio","Femmina","Eta<35","Eta35_50","Eta>50","RedditoAlto","RedditoBasso")
rownames(count_table)<-c("cat1","cat2","cat3","cat4")

library(ca)
an_co<-ca(count_table)
an_co
summary(an_co)
plot(an_co)

attach(dati_orig)

Vi<-"#481D52"
Fu<-"#DD517E"
Az<-"#7A98EE"
Ar<-"#E68E35"
Bl<-"#556CC9"
l.col<-c("Maschio","Femmina","Età <35","Età 35-50","Età >50","Reddito alto","Reddito basso")
c.row<-data.frame(an_co$rowcoord[,1:2],"lab"=rownames(count_table))
c.col<-data.frame(an_co$colcoord[,1:2],"lab"=l.col)


library(ggplot2)
library(ggrepel)
library(ggthemes)

map<-ggplot()+
  geom_hline(yintercept=0,color="white",size=2.5)+
  geom_vline(xintercept=0,color="white",size=2.5)+
  geom_label(aes(x=c.col$Dim1,y=c.col$Dim2,label=l.col),fill=Fu,size=8,label.size=0,color="white")+
  geom_label(aes(x=c.row$Dim1,y=c.row$Dim2,label=c.row$lab),fill=Ar,size=8,label.size=0,color="white")+
  #geom_point(aes(c.row$Dim1,c.row$Dim2),color=Az,size=5,shape=19)+
  labs(title="\nRappresentazione congiunta delle categorie di televisioni\n e delle caratteristiche demo-economico\n",
       x="\nDimensione 1, inerzia 87,2%",
       y="\nDimensione 2, inerzia 10%\n")+
  theme(plot.background=element_rect(fill=Vi,color=Vi),
        panel.background=element_rect(fill=Vi,color=Vi),
        plot.title=element_text(hjust=0.5,size=30,color="white"),
        axis.title=element_text(hjust=0.5,size=20,color="white"),
        axis.text=element_text(hjust=0.5,size=10,color="white"),
        panel.grid.minor=element_line(color=Vi))
  
map  
ggsave("Ac map.png",plot=map,width=16,height=9,dpi=600)




##### GRAFICI SLIDE #####

library(ggplot2)

for (i in 1:4){
freq<-data.frame(x=c("Maschio","Femmina","Età <35","Età 35-50","Età >50","Reddito\n alto","Reddito\n basso"),
                 t(count_table[i,]),row.names=NULL)
y.lab<-rownames(count_table[i,])

radar<-ggplot(data=freq)+
  geom_col(aes(x=x,y=freq[,2]),fill=c(Fu,Fu,Ar,Ar,Ar,Bl,Bl),alpha=c(1,1,0.7,0.7,0.7,1,1), width=1)+
  geom_hline(yintercept = seq(0, 16, by = 1),color =Az, size = 1,alpha=0.75) +
  geom_vline(xintercept = seq(0.5, 6.5, by = 1),color=Az, size = 1,alpha=0.75)+
  geom_text(aes(x=x,y=14,label=x),color="white",size=20)+
  coord_polar()+
  theme_minimal()+
  labs(title=paste("\n",y.lab,sep=""),x="",y="")+
  theme(
    plot.background=element_rect(fill=Vi),
    panel.background=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.line=element_line(color=Vi),
    panel.grid=element_line(color=Vi),
    panel.border=element_blank(),
    plot.title=element_text(hjust=0.5,color="white",size=65)
  )
#radar
ggsave(plot=radar,file=paste("Modello",i,".png"),dpi=300,width=15,height=16)
}


MDS_m<-cmdscale(dist)
coord<-as.data.frame(MDS$points)
labels<-paste("Modello: ",dati_orig[,1])

# Mappa per slide numero 6 e 7
perc_map<-ggplot(coord,aes(x=V1,y=V2))+
  geom_hline(yintercept=0,color="#E68E35",size=2)+
  geom_vline(xintercept = 0,color="#E68E35",size=2)+
  geom_point(colour=Fu,size=10)+
  #geom_label(aes(label=labels),color="#DD517E",label.padding=0.5,label.size=1.1,size=2.2,segment.color="#7A98EE",segment.size=1.5,force=30,box.padding=0.5)+
  labs(x="\nDimensione 1",y="Dimensione 2\n")+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5,size=30,colour=Az),
        axis.title=element_text(size=20,colour=Az),
        axis.line=element_line(color=Az),
        axis.text=element_text(size=10,color=Az),
        plot.background=element_rect(fill="white"),
        panel.grid.minor=element_line(colour="white"),
        panel.grid.major=element_line(colour=Az)
  )
perc_map
ggsave(plot=perc_map,"Mappa percettiva.png",width=33,height=19,units="cm",dpi=300)
