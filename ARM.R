library(arulesViz)
library(arules)
library(readxl)

#PRAPROSES DATA
Dataset1<-Dataset[,14:15]
names(Dataset1)<-c("Pembimbing_1","Pembimbing_2")
Keyword<-Dataset[,4:13]
names(Keyword)<-c("keyword_1","keyword_2","keyword_3","keyword_4","keyword_5","keyword_6","keyword_7","keyword_8","keyword_9","keyword_10")
Dataset2<-cbind(Dataset1,Keyword)

#Merubah dataset menjadi factor
Dataset1$Pembimbing_1<-as.factor(Dataset1$Pembimbing_1);
Dataset1$Pembimbing_2<-as.factor(Dataset1$Pembimbing_2);

Dataset2$keyword_1<-as.factor(Dataset2$keyword_1);
Dataset2$keyword_2<-as.factor(Dataset2$keyword_2);
Dataset2$keyword_3<-as.factor(Dataset2$keyword_3);
Dataset2$keyword_4<-as.factor(Dataset2$keyword_4);
Dataset2$keyword_5<-as.factor(Dataset2$keyword_5);
Dataset2$keyword_6<-as.factor(Dataset2$keyword_6);
Dataset2$keyword_7<-as.factor(Dataset2$keyword_7);
Dataset2$keyword_8<-as.factor(Dataset2$keyword_8);
Dataset2$keyword_9<-as.factor(Dataset2$keyword_9);
Dataset2$keyword_10<-as.factor(Dataset2$keyword_10);

#Set Kata yang akan dibuang
Kata_Dibuang <- c("Bogor Agricultural University","IPB","Institut Pertanian Bogor","Bogor Agicultural University")

#Membuang Kata yang tidak diperlukan, yaitu pada "Kata_Dibuang"
D2_Clean <- as.data.frame(sapply(Dataset2, function(x) 
  gsub(paste(Kata_Dibuang, collapse = '|'), '', x)))

#Menyimpan Cut_keyword dari daraset 2
write.csv(D2_Clean,file = "Dataset2.csv")

#merubah empty menjadi NA
Key_NA <- read.csv("Dataset2.csv", header = T, na.strings = c("","NA"))
Dataset2<- Key_NA[, -1]

##TRANSAKSI
#Dataset1
T_D1<- as(Dataset1, "transactions");
T_D2<- as(Dataset2, "transactions");

##RULES

#2 Frequent Itemset, core dari Pembimbing 1
rules <- apriori(T_D1,parameter = list(supp = 0.004, conf = 0.01, minlen=1, target = "rules"))
top.support <- sort(subset(rules, subset= lhs %pin% "Pembimbing_1=" ), decreasing = T, na.last = NA, by = "support")
(Hasil<-as.data.frame(inspect(top.support)))
plot(rules, method="graph", control=list(type="items"))

#2 Frequent Itemset, core dari Pembimbing 1 dan 2
  rules <- apriori(T_D2,parameter = list(supp = 0.0012, conf = 0.01, minlen=2, target = "rules"))
  top.support <- sort(subset(rules, subset= (lhs %pin% "Pembimbing_1=" & lhs %pin% "Pembimbing_2=" )), decreasing = T, na.last = NA, by = "support")
  (Hasil<-inspect(top.support))
  plot(top.support, method="graph", control=list(type="items"))