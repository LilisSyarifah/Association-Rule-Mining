setwd("D:/S2/Materi Kuliah/semester 3/Tesis/Program/Data/")

library(readxl)
#Memanggil data
Repo_faperta_english_abstrak <- read_excel("D:/S2/Materi Kuliah/semester 3/Tesis/Program/Data/Repo_faperta_english_abstrak.xlsx")
View(Repo_faperta_english_abstrak)
#Mengambil data dari kolom tertentu (kolom 6)
abstrak_eng<-data.frame(Repo_faperta_english_abstrak[,6])
#abstrak<-data.frame(English[,3])
# Create a list of n data frames
n<-nrow(abstrak_eng)
#names(abstrak)<-"abstrak"
my_list <- lapply(1:n, function(i) abstrak_eng[i,1])

# name the data frames
names(my_list) <- 1:n

# save each new data frame as an individual .csv file based on its name
lapply(1:length(my_list), function(i) write.table(my_list[[i]],
                                                file = paste0(names(my_list[i]), ".txt"),
                                                row.names = FALSE, col.names=FALSE))
