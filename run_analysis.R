#setwd('C:\\Users\\Roberto\\GCD_Project')

unzip('getdata_projectfiles_UCI HAR Dataset.zip')

#load some utility data.frame
actLabel<-read.table(".\\UCI HAR Dataset\\activity_labels.txt",sep=" ")
names(actLabel)<-c("idAct","ActLabel")
feat<-read.table(".\\UCI HAR Dataset\\features.txt",header=FALSE)

#Clear and Load UCI HAR Dataset\train\X_train.txt
f1<-file('.\\UCI HAR Dataset\\train\\X_train.txt')
testo1<-readLines(f1)
l<-strsplit(testo1,"\n")
l1<-lapply(l,f<-function(a,b,c,...) return(sub(b,c,a,...)),"^ ","")
l2<-lapply(l1,f<-function(a,b,c,...) return(sub(b,c,a,...)),"^  ","")
l3<-lapply(l2,f<-function(a,b,c,...) return(gsub(b,c,a,...)),"  "," ")
l4<-lapply(l3,f<-function(a,b,c,...) return(gsub(b,c,a,...))," ",",")
l5<-lapply(l4,f<-function(a,b,c,...) return(gsub(b,c,a,...)),"^,","")
lapply(l5,f<-function(a,...) cat(paste(a,"\n",sep=""),...),file=".\\UCI HAR Dataset\\train\\X_clear.txt",append=TRUE)
close(f1)
#rm(list("testo1","l","l2","l3","l4"))

dt1<-read.csv(".\\UCI HAR Dataset\\train\\X_clear.txt",header=FALSE)
unlink(".\\UCI HAR Dataset\\train\\X_clear.txt");
act1<-read.csv(".\\UCI HAR Dataset\\train\\y_train.txt",header=FALSE)
names(act1)<-"idAct"
act1<-merge(act1,actLabel)

subj1<-read.csv(".\\UCI HAR Dataset\\train\\subject_train.txt",header=FALSE)
dt1$idActivity<-act1[,2]
dt1$subject<-subj1[,1]

#Load UCI HAR Dataset\test\X_test.txt 
f1<-file('.\\UCI HAR Dataset\\test\\X_test.txt')
testo1<-readLines(f1)
l<-strsplit(testo1,"\n")
l1<-lapply(l,f<-function(a,b,c,...) return(sub(b,c,a,...)),"^ ","")
l2<-lapply(l1,f<-function(a,b,c,...) return(sub(b,c,a,...)),"^  ","")
l3<-lapply(l2,f<-function(a,b,c,...) return(gsub(b,c,a,...)),"  "," ")
l4<-lapply(l3,f<-function(a,b,c,...) return(gsub(b,c,a,...))," ",",")
l5<-lapply(l4,f<-function(a,b,c,...) return(gsub(b,c,a,...)),"^,","")
lapply(l5,f<-function(a,...) cat(paste(a,"\n",sep=""),...),file=".\\UCI HAR Dataset\\test\\X_clear.txt",append=TRUE)
close(f1)
#rm(list("testo1","l","l2","l3","l4"))

dt2<-read.csv(".\\UCI HAR Dataset\\test\\X_clear.txt",header=FALSE)
unlink(".\\UCI HAR Dataset\\test\\X_clear.txt");
act2<-read.csv(".\\UCI HAR Dataset\\test\\y_test.txt",header=FALSE)
names(act2)<-"idAct"
act2<-merge(act2,actLabel)

subj2<-read.csv(".\\UCI HAR Dataset\\test\\subject_test.txt",header=FALSE)
dt2$idActivity<-act2[,2]
dt2$subject<-subj2[,1]

#merge train with test 
library(plyr)
dtFinal<-rbind.fill(dt1,dt2)

#load feature column name
colNames<-c(as.vector(feat[,2]),"Activity","Subject")
names(dtFinal)<-colNames
selCol<-c(dim(dtFinal)[2],dim(dtFinal)[2]-1,grep("mean()|std()",colNames))
dtFinal<-dtFinal[,selCol]

#Appropriately labels the data set with descriptive variable names
#TODO


#Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyDt<-ddply(dtFinal,.(Subject,Activity),function(df) colMeans(df[,3:dim(df)[2]]))

