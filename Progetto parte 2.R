#########################################
#####                               #####
#####     SVILUPPO DEL PUNTO 1      #####
#####                               #####
#########################################

##### CARICHIAMO I DATI #####

library(readxl)
data<-read_xlsx("tvprices.xlsx",sheet=4)

##### OPERAZIONI INIZIALI SULLE VARIABILI #####

library(tidyverse)

data<- data %>%
  column_to_rownames(var="Model")
data<-data[1:6]
data<-data %>%
  mutate(NatRes=
           recode(NatRes,
                  "720"="HD",
                  "1080"="Full HD"
           )
  )
data[,1]<-as.factor(data[,1]) #Categoria as.factor
data[,2]<-ifelse(data[,2]<=mean(data[,2]),"<=38 pollici",">38 pollici")
data[,2]<-as.factor(data[,2])
data[,3]<-ifelse(data[,3]<=mean(data[,3]),"15-60","70-150")
data[,3]<-as.factor(data[,3])
data[,4]<-as.factor(data[,4])
data[,5]<-as.factor(data[,5])
data[,6]<-ifelse(data[,6]<=mean(data[,6]),"<=1272,99",">1272,99")
data[,6]<-as.factor(data[,6])

##### COSTRUZIONE DELL'ALBERO CON ALGORITMO CHAID #####

library(partykit)
library(CHAID)

attach(data)
formula<-Categoria~NatRes+Price+Screen+DCR+`LCD Hz`

ctrl<-chaid_control(alpha2=0.05,alpha4=0.05,minsplit=10,minbucket=5,maxheight=4)
albero<-chaid(formula=formula,data=data,control=ctrl)
plot(albero)

##### COSTRUZIONE DELL'ALBERO CON ALGORITMO CHAID MANUALE #####
v<-vector()
for(i in 2:6){
  v[i-1]<-colnames(data)[i]
  v[(i-1)+5]<-chisq.test(table(data[,i],data[,1]))[["statistic"]][["X-squared"]]
  v[(i-1)+10]<-chisq.test(table(data[,i],data[,1]))$p.value
  print(table(data[,i],data[,1]))}
matrix(v,nrow=5,ncol=3,byrow=F)

t<-chisq.test(table(data[,2],data[,1]))
nodo2<-data %>%
  filter(NatRes=="HD")
nodo3<-data %>%
  filter(NatRes=="Full HD")

table(nodo2[,1])

temp<-nodo3 %>% filter(`LCD Hz`!=240)
table(temp[,4],temp[,1])
temp<-nodo3 %>% filter(`LCD Hz`!=60)
table(temp[,4],temp[,1])

v<-vector()
for(i in 2:6){
  v[i-1]<-colnames(data)[i]
  v[(i-1)+5]<-chisq.test(table(nodo3[,i],nodo3[,1])[,-1])[["statistic"]][["X-squared"]]
  v[(i-1)+10]<-chisq.test(table(nodo3[,i],nodo3[,1])[,-1])$p.value
  print(table(nodo3[,i],nodo3[,1])[,1])}
matrix(v,nrow=5,ncol=3,byrow=F)

nodo4<-nodo3 %>%
  filter(Price=="<=1272,99")
nodo5<-nodo3 %>%
  filter(Price==">1272,99")

temp<-nodo5 %>% filter(`LCD Hz`!=240)
chisq.test(table(temp[,4],temp[,1])[-3,-1])
temp<-nodo5 %>% filter(`LCD Hz`!=60)
chisq.test(table(temp[,4],temp[,1])[-1,-1])

nodo5[,4]<-ifelse(nodo5[,4]=="60","60","120-240")
nodo5[,4]<-as.factor(nodo5[,4])

v<-vector()
for(i in 2:6){
  v[i-1]<-colnames(data)[i]
  v[(i-1)+5]<-chisq.test(table(nodo5[,i],nodo5[,1])[,-1])[["statistic"]][["X-squared"]]
  v[(i-1)+10]<-chisq.test(table(nodo5[,i],nodo5[,1])[,-1])$p.value
  print(table(nodo5[,i],nodo5[,1])[,-1])}
matrix(v,nrow=5,ncol=3,byrow=F)

##### COSTRUZIONE ALBERO FINALE CHAID #####
ctrl_f<-chaid_control(alpha2=0.05,alpha4=0.05,minsplit=3,minbucket=1,maxheight=4)
albero_f<-chaid(formula=formula,data=data,control=ctrl_f)
plot(albero_f)

##### STIMA DEL TASSO DI ERRATA CLASSIFICAZIONE #####
table(predict(albero_f,data)==albero_f$fitted[,2])["FALSE"]/nrow(data)

# Ordiniamo in maniera casuale il campione
set.seed(123)
nrow<-sample(nrow(data))
camp<-data[nrow,]

ctrl<-chaid_control(alpha2=0.05,alpha4=0.05,minsplit=3,minbucket=1,maxheight=4)

vett<-numeric()
for(i in 1:10){
  left<-40-i*4+1
  right<-left+3
  add<-camp[-(left:right),]
  test<-camp[left:right,]
  albero_t<-chaid(formula,data=add,control=ctrl)
  print(albero_t)
  vett[i]<-length((predict(albero_t,test)==test[,1])[(predict(albero_t,camp[left:right,])==test[,1])==FALSE])/4
}
print(mean(vett))

##### SCRIPT PER GRAFICO SLIDE ALBERO CHAID #####

library(networkD3)
nodes<-data.frame("Splitvar"=c("","","","","")); nodes
links<-as.data.frame(matrix(c(
                      0,1,7,#cat4
                      0,1,11,#cat3
                      0,1,11,#cat2
                      0,2,11,#cat1
                      1,3,7,#cat4
                      1,3,11,#cat3
                      1,3,1,#cat2
                      1,4,10#cat2
                    ),byrow=T,nrow=8))
colnames(links)<-c("Padre","Figlio","Quantità")

# Creiamo i gruppi per i colori
links$gruppo<-as.factor(c("cat4","cat3","cat2","cat1","cat4","cat3","cat2","cat2"))
nodes$gruppo<-as.factor(c("neu","neu","1","2","3"))

colourScale<-'d3.scaleOrdinal() .domain(["cat1","cat2","cat3","cat4","neu","1","2","3"]) .range(["#FF296D","#05D8E8","#FFE66D","#ED7D31","white","white","white","white"]) '

graf<-sankeyNetwork(Links=links, Nodes=nodes, Source="Padre", Target="Figlio", Value="Quantità",NodeID="Splitvar",margin=list(top=10,bottom=1,right=1,left=1),
              fontSize=20, nodeWidth=10, nodePadding=140, colourScale=colourScale, LinkGroup="gruppo", NodeGroup="gruppo", width=800, height=400)
graf

#####

#########################################
#####                               #####
#####     SVILUPPO DEL PUNTO 2      #####
#####                               #####
#########################################

##### LETTURA DEI DATI E TRATTAMENTO PRELIMINARE DELLE VARIABILI #####

library(readxl)
library(tidyverse)

dati<-read_xlsx("tvprices.xlsx",sheet=4)
dati<- dati %>%
  column_to_rownames(var="Model")
dati<-dati[1:6]

dati<-dati %>%
  mutate(NatRes=
           recode(NatRes,
                  "720"="HD",
                  "1080"="Full HD"
           )
  )
dati[,1]<-as.factor(dati[,1])
dati[,5]<-as.factor(dati[,5])

attach(dati)

##### COSTRUZIONE DELL'ALBERO CON ALGORITMO CART #####

library(rpart)
formula<-Categoria~Screen+DCR+`LCD Hz`+NatRes+Price
ctrl<-rpart.control(minsplit=2,cp=0.0000000000000001)
tree<-rpart(formula,data=dati,method="class",control=ctrl)

plot(tree,uniform=T,margin=0.1) 
text(tree,use.n=T,cex=0.8,all=T)

tree
summary(tree)

##### cambiamo cp
formula<-Categoria~Screen+DCR+`LCD Hz`+NatRes+Price
ctrl<-rpart.control(minsplit=2,cp=0.01)
set.seed(934)
tree<-rpart(formula,data=dati,method="class",control=ctrl)

plot(tree,uniform=T,margin=0.1,branch=1) 
text(tree,use.n=T,cex=0.8,all=T,splits=T)

tree
summary(tree)

##### GRAFICO ALBERO CART ####

library(rpart.plot)
blu_notte<-"#01012A"
azz<-"#05D8E8"
cel<-"#D1F9FF"
fuc<-"#FF296D"
par(bg=blu_notte)
rpart.plot(tree,type=4,extra=1,fallen.leaves=F,varlen=0,faclen=0,tweak=1.1,
           clip.facs=T,branch=1,branch.col="white",split.col="white")

prp(tree,type=5,extra=1,cex=1.2,under=T,
    col=cel,split.col=azz,branch.col=fuc,
    split.cex=0.7,branch.lwd=5, split.border.col = fuc,#split.shadow.col=cel,
    #split.fun =
    split.space = 0.5,split.yspace = 0.8, 
    uniform=T,#space=0.2,gap=0.5, #split.shadow.offset =T
    branch = 1, under.col = cel, varlen=0, faclen=0, border.col=blu_notte,
    under.cex = 0.7, boxes.include.gap=T, compress=T
    )

library(networkD3)
nodes<-data.frame("Splitvar"=c(1,2,3,4,5,6,7,8,9,10,11)); nodes
links<-as.data.frame(matrix(c(
  0,1,11,#cat1
  0,2,11,#cat2
  0,2,11,#cat3
  0,2,7,#cat4
  2,4,10,#cat2
  2,5,1,#cat2
  2,5,11,#cat3
  2,5,7,#cat4
  5,6,1,#cat2
  5,6,11,#cat3
  5,7,7,#cat4
  6,8,1,#morto
  6,8,2,#morto
  6,9,9,#morto
  8,10,1,#morto
  8,11,2#morto
),byrow=T,nrow=16))
colnames(links)<-c("Padre","Figlio","Quantità")

# Creiamo i gruppi per i colori
links$gruppo<-as.factor(c("cat1","cat2","cat3","cat4","cat2","cat2","cat3","cat4","cat2","cat3","cat4","morto","morto","morto","morto","morto"))
nodes$gruppo<-as.factor(c("neu","neu","1","2","3","neu","neu","neu","neu","neu","neu"))

colourScale<-'d3.scaleOrdinal() .domain(["cat1","cat2","cat3","cat4","morto","neu","1","2","3"]) .range(["#FF296D","#05D8E8","#FFE66D","#ED7D31","black","white","white","white","white"]) '

sankeyNetwork(Links=links, Nodes=nodes, Source="Padre", Target="Figlio", Value="Quantità",NodeID="Splitvar",
                    fontSize=20, nodeWidth=10, nodePadding=140, colourScale=colourScale, LinkGroup="gruppo", NodeGroup="gruppo")

par(bg="white")

##### PRUNING DELL'ALBERO FINALE #####

opt<-tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree_p<-prune(tree,cp=opt)
summary(tree_p)
plot(tree_p,uniform=T,margin=0.1) 
text(tree_p,use.n=T,cex=0.8,all=T)

##### CROSS VALIDATION SU ALGORITMO CART ####

formula<-Categoria~Screen+DCR+`LCD Hz`+NatRes+Price
ctrl<-rpart.control(minsplit=2,cp=0.01)
set.seed(12345)
nrow<-sample(nrow(dati))
camp<-dati[nrow,]

vett<-numeric()
log<-logical()
for(i in 1:10){
  left<-40-i*4+1
  right<-left+3
  add<-camp[-(left:right),]
  test<-camp[left:right,]
  set.seed(934)
  tree<-rpart(formula,data=add,method="class",control=ctrl)
  opt<-tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
  albero_p<-prune(tree,cp=opt)
  print(albero_p)
  previsioni<-predict(albero_p,test)
  previsioni[,2]<-previsioni[,2]*2
  previsioni[,3]<-previsioni[,3]*3
  previsioni[,4]<-previsioni[,4]*4
  for(j in 1:4){
    log[j]<-which.max(previsioni[j,])==test[j,1]
  }
  vett[i]<-1-table(log)["TRUE"]/4  
  }
print(mean(vett))
