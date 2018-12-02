
library(arules)
library(tm)
library(SnowballC)
library(wordcloud)
library(arulesViz)
cname<- file.path("D:","Lilis Syarifah's File Master","S2","Materi Kuliah","semester 3","Tesis","Program","Data")
#cname<- file.path("C:","Users","Apriliantono","Desktop","SIDANG","Program","Data")
docs <- Corpus(DirSource(cname))
#make texts document as one
docs <- tm_map(docs,PlainTextDocument)
docs <- Corpus(VectorSource(docs$content))
#change the word to lower style
docs <- tm_map(docs, tolower)
#remove punctuation in the text
docs <- tm_map(docs, removePunctuation)
#remove number
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove blank space
docs <- tm_map(docs, stemDocument)
#remove spesific word
myStopWord <- c('alternative','although','along','ability','aim','aimed','areas','aims','activ','activity',"activities",'addition','among','and','analys','analysis','are','analyses','analysi','analyze','analyzed','analyz','also','accuracy','algorithm','available',"applied",'application','alternatives','base','based','bogor','browser',"best",'become','can','calculate',"characteristics",'complete',"consisted",'consist','consists','combination',"compared",'considered', "condition",'caus',"cause",'causes',"caused",'containing','code','current','compile','computer','conduct','conducted',"character",'criteria',"characters",'content','determine','determin','design','dapat','dari',"days", "done",'due','found','data','dan','different','differ','environment',"evaluate",'evalu',"existing",'experiment','effect','execute', 'except','experiment','experi','especially','effect','effects','four','for','from','file','factor','find','format','formula','function','first','good','get','grow','growth','generate',"highest","however",'howev',"higher",'high','height','ipb','identifi','identification',"information",'implement','identify','important','import','input',"increase",'increas', "increased",'installation','included','includ',"index",'increasing',"indicated",'known','know',"level",'levels','like',"management","medium",'meet','mainly', 'may','measure','method','model','mainwhile','make',"methods",'must',"many","namely", 'need','non','needed','necessary','number','new','obtain','open','observ','object','objectives',"obtained",'one','operate','operation','order','optimal','output','plants','perform','power','previous',"per",'problem','program','produced','produce','produc','provide','purpose','potentially','product','production','productivity',"period",'purposes','preferences',"quality",'rate','regarding','require','research','result','resulted',"results","randomized",'reduced','reduc','recommendation','requires',"respectively",'set','second','shown','shows','status',"several",'signific',"significantly",'studied','significant',"selected",'study','spesific','still','show','sustanable','system','showed','studi','source','summary','system','standard','types','technical','type','the','that','technique','technology',"time",'times','tool','total','two','three','transfer',"therefore","therefor", "test", 'unit','utility',"value",'valu',"whereas", "without","well", 'which','will','was','were','with','yang')
docs <- tm_map(docs, removeWords, myStopWord)
docs <- tm_map(docs, stripWhitespace)
dtm <- DocumentTermMatrix(docs)
dtm
dt <- removeSparseTerms(dtm, 0.95)
dt
mdt<-as.matrix(dt)
write.csv(mdt,file = "dt.csv")
trans <- as(mdt, "transactions")
frequentitemset <- apriori(trans, parameter = list(sup=0.022,minlen=4),
                           appearance = list(default="rhs",lhs=c("land","seed","fertil","soil","rice","genotyp","forest","fruit",'water','leaf','oil',"palm",'farm',"soybean",'natur','farmer','flower','nutrient','plantat','watersh','seedl','germin','pest','veget','paddi','root','morpholog','gene','physiolog')))
top.support <- sort(frequentitemset, decreasing = TRUE, na.last = NA, by = "support")

items(top.support)
support<-quality(top.support)
support
write.csv(support,file = "support.csv")

frequent_l<- as(items(top.support), "list")
frequent_l

plot(top.support, method="graph")
##

tdm <- TermDocumentMatrix(docs)
td <- removeSparseTerms(tdm, 0.95)
mtd<-as.matrix(td)


View(mtd)
write.csv(mtd,file = "td.csv")
#menghitung jumlah kata dalam setiap satu dokumen
freq <- sort(colSums(as.matrix(mtd)), decreasing=TRUE)
freq
write.csv(freq,file = "freq.csv")
head(freq, 50)
#menghitung frekuensi kata dalam banyak dokumen yang diurutkan dari kemunculan tertinggi
dm <- sort(rowSums(mtd),decreasing=TRUE)
#merubah matrik kedalam data fram dengan memberikan informasi header
dm <- data.frame(word = names(dm),freq=dm)
head(dm,200)
#wordcloud(docs, max.words = 100, random.order = FALSE)
set.seed(142)
wordcloud(words = dm$word, freq = dm$freq, min.freq = 100,
          max.words=, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


