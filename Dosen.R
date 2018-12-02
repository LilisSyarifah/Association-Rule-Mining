library(readxl)

#Load dataset 
Dataset <- read_excel("D:/Lilis Syarifah's File Master/S2/Materi Kuliah/semester 3/Tesis/Program/Dataset.xlsx", 
                      +     col_names = FALSE)

#PRAPROSES DATA
Dataset1<-Dataset[,14:15]
names(Dataset1)<-c("Pembimbing_1","Pembimbing_2")
dosen<-rbind(as.matrix(Dataset1[,1]),as.matrix(Dataset1[,2]))
dosen<-dosen[!duplicated(dosen), ]
write.csv(dosen,file = "dosen.csv")
dosen
View(dosen)

#Dosen 1
D1_P1<-Dataset1[,1]
dosen1<-D1_P1[!duplicated(D1_P1), ]
write.csv(D1_P1,file = "Dosen1.csv")

#Dosen 2
D1_P2<-Dataset1[,2]
dosen1<-D1_P2[!duplicated(D1_P2), ]
write.csv(D1_P2,file = "Dosen2.csv")
