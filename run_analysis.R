#Run_analysis reads the Test and Train Data sets, extracts the variables that measure
#the mean and STD only. It then merges the two data sets, and calculates the mean
#of each variable. Finally, it produces a tidy data set which descriptively defines
#the activities and the measured variables.
#Further details in the codebook and README files
activitylabel<-read.table("activity_labels.txt",stringsAsFactor=F)
featurelabel<-read.table("features.txt",stringsAsFactors=F)
#accessing the Training data set
xtrain<-read.table("train/X_train.txt",stringsAsFactor=F)#the measured variables
ytrain<-read.table("train/y_train.txt",stringsAsFactor=F)#the activity identifier
subjectID<-read.table("train/subject_train.txt",stringsAsFactor=F)#the subject identifier
header<-c(subjectID,ytrain)
names(xtrain)<-as.character(featurelabel[,2])
names(header)<-c("subject.id","activity.id")
train.set<-cbind(header,xtrain)
names(train.set)<-tolower(names(train.set))#changing to lower case the names of train.set in order to aid the find and replace statement
ind0<-c(1,2)
ind1<-grep("mean",names(train.set))
ind2<-grep("std",names(train.set))#extracting the indices of the columns which contain the mean and std of the variable
ind<-c(ind0,ind1,ind2)
ind<-ind[order(ind)]
train.data<-train.set[,ind] #extracting only the columns of the variables which contain std and mean
###The Test Data Set
subjectIDTest<-read.table("test/subject_test.txt",stringsAsFactor=F)
xtest<-read.table("test/X_test.txt",stringsAsFactors=F,header=F)
ytest<-read.table("test/y_test.txt",stringsAsFactors=F,header=F)
headertest<-c(subjectIDTest,ytest)
names(xtest)<-as.character(featurelabel[,2])
names(headertest)<-c("subject.id","activity.id")
test.set<-cbind(headertest,xtest)
names(test.set)<-tolower(names(test.set))
ind0<-c(1,2)
ind1<-grep("mean",names(test.set))
ind2<-grep("std",names(test.set))
ind<-c(ind0,ind1,ind2)
ind<-ind[order(ind)]
test.data<-test.set[,ind]
##The Part where Test and Train are Merged
merge.data<-merge(train.data,test.data,all=TRUE)
library(plyr);library(reshape2)
nmean<-names(merge.data)
melt.data<-melt(merge.data,id=c("subject.id","activity.id"),measure.vars=nmean[3:length(nmean)])
data<-dcast(melt.data, subject.id + activity.id ~ variable,mean)
data$activity.id<-mapply(gsub,c(1,2,3,4,5,6),activitylabel[,2],data$activity.id)
ndata<-read.table("ndata.txt",stringsAsFactor=F,header=F)#prdefined header information
names(data)<-ndata[,1]
write.csv(data,"tidydata.csv")