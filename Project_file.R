library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)  
library(graphics)
library(plotrix)
library(corrplot)

setwd("c:/Users/lalit/Desktop/student-alcohol-consumption/")
student_mat=read.table("student-mat.csv",sep=",",header=TRUE)
student_por=read.table("student-por.csv",sep=",",header=TRUE)

merged<- rbind(student_mat,student_por)
merged_final <- merged %>% distinct(school,sex,age,address,famsize,Pstatus,
                                    Medu,Fedu,Mjob,Fjob,reason,
                                    guardian,traveltime,studytime,failures,
                                    schoolsup, famsup,activities,nursery,higher,internet,
                                    romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)

write.csv(merged_final,file="merged_final.csv")
merged_final$G1 <- NULL
merged_final$G2 <- NULL
nrow(merged_final)

summary(merged_final)

merged_final$Dalc <- as.factor(merged_final$Dalc)
merged_final$Walc <- as.factor(merged_final$Walc)




lbls <- c("Very Low","Low ","Moderate ","High ","Very High")

mytable <- table(merged_final$Dalc)

#code for pie chart Start
lbls <- c("Very Low","Low ","Moderate ","High ","Very High")
mytable <- table(merged_final$Dalc)

mytable1 <- table(merged_final$Walc)

pct <- round(mytable/sum(mytable)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 

pie(mytable, labels = lbls, main = "Pie Chart of Alcohol consumption over weekdays",col = rainbow(length(mytable)))
legend("topright", lbls, cex = 0.8,
       fill = rainbow(length(mytable)))

lbls <- c("Very Low","Low ","Moderate ","High ","Very High")
pct <- round(mytable1/sum(mytable1)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 

pie(mytable1, labels = lbls, main = "Pie Chart of Alcohol consumption on weekend",col = rainbow(length(mytable1)))
legend("topright", lbls, cex = 0.8,
       fill = rainbow(length(mytable1)))

#code for pie char end

#code for Normal distribution of final grades
x <- merged_final$G3 
h<-hist(x, breaks=10, col=c("lightblue","grey","lightgreen","lightyellow"), xlab="Final Grades",
        col.bars="darkblue", col.border="lightsteelblue4", col.bg="ivory",
        col.grid="darkgray",
        main="Normal Distribution of final grades") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
#end


#code for  Age vs health
ggplot(merged_final, aes(x=(as.numeric(age))))+ geom_histogram(fill="cornflowerblue", colour="black",binwidth=1) +
  facet_grid(health ~ .)+ggtitle("Distribution based on Age and Health")+xlab("Age")
#end

#graph for romance and Final Grade
ggplot(merged_final, aes(x=G3,fill=romantic ) )+
  facet_grid(~romantic)+
  geom_histogram(position="identity", alpha=0.45,binwidth=1.0) + xlab("Final Grade")
#end

#Correaltion matrix
matrixcor  <- cor(merged_final[,unlist(lapply(merged_final, is.numeric))])
matrixcor
corrplot(matrixcor, method="number")
#end


# Weekday alcohol vs final grades
c1 <-ggplot(merged_final, aes(x=Dalc, y=G3, fill=Dalc)) + 
  geom_boxplot(alpha=0.3,outlier.colour="black",outlier.fill="grey",outlier.size=2) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")+
  xlab("Alcohol consumption")+
  ylab("Final Grade")+
  ggtitle(" Weekday Alcohol Consumption vs Final Grade")


# Weekend alcohol vs grades
c2<-ggplot(merged_final, aes(x=Walc, y=G3, fill=Walc)) + 
  geom_boxplot(alpha=0.3,outlier.colour="black",outlier.fill="red",outlier.size=3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")+
  xlab("Alcohol consumption")+
  ylab("Final Grade")+
  ggtitle("Weekend Alcohol Consumption vs Final Grade")

grid.arrange(c1,c2,ncol=2)

#frequency distribution of School
ggp <- ggplot(data.frame(merged_final),aes(x=school))
# counts
ggp + geom_histogram(fill="lightblue",stat = "count")+
  xlab("School")+
  ylab("Number of Students")+
  ggtitle(" Frequency Distribution of Students")


#freq distribution of age
ggpage <- ggplot(data.frame(merged_final),aes(x=age,fill=school))
# counts
ggpage + geom_histogram(stat="count")+
  xlab("Age")+
  ylab("Count")+
  ggtitle("Frequency Distribution of Age by School")

#frequency Distribution of absences
ggplot(merged_final, aes(x=absences, fill=school)) +
  geom_histogram(binwidth=.5, alpha=.6, position="identity")

#weekday alcohol consumption by age
a1 <- ggplot(merged_final, aes(x=age, fill=Dalc))+
  geom_histogram(binwidth=1, colour="black")+
  facet_grid(~Dalc)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Weekday alcohol consumption by age")+
  xlab("Student's age")  

#weekend alcohol consumption by age
a2 <- ggplot(merged_final, aes(x=age, fill=Walc))+
  geom_histogram(binwidth=1, colour="black")+
  facet_grid(~Walc)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Weekend alcohol consumption by age")+
  xlab("Student's age")  

grid.arrange(a1,a2,ncol=1)


#weekday alcohol consumption by reason
r1 <- ggplot(merged_final, aes(x=reason, fill=Dalc))+
  geom_histogram(binwidth=1, colour="black",stat="count")+
  facet_grid(~Dalc)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Weekday alcohol consumption by Reason")+
  xlab("Reason")  
#weekend alcohol consumption by reason
r2<-ggplot(merged_final, aes(x=reason, fill=Walc))+
  geom_histogram(binwidth=1, colour="black",stat="count")+
  facet_grid(~Walc)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Weekend alcohol consumption by Reason")+
  xlab("Reason")  

grid.arrange(r1,r2,ncol=1)

s1 <- ggplot(merged_final, aes(x=Walc, y=G3, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#00897B", "#E64A19"))+
  facet_grid(~higher)+
  theme_bw()+
  xlab("Weekday alcohol consumption")+
  ylab("Grades")+
  ggtitle("Weekday alcohol consumption vs Final Grades by sex and higher education")

s2 <- ggplot(merged_final, aes(x=Dalc, y=G3, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#00897B", "#E64A19"))+
  facet_grid(~higher)+
  theme_bw()+
  xlab("Weekend alcohol consumption")+
  ylab("Grades")+
  ggtitle("Weekend alcohol consumption vs  Final Grades by sex and higher education")

grid.arrange(s1,s2, nrow=2)

#missing values 
sum(!complete.cases(merged_final))

ggplot(merged_final, aes(x=Dalc, y=absences, color=sex))+
  geom_boxplot(outlier.color = "black")+
  scale_fill_manual(values=c("#00897B", "#E64A19"))+
  theme_bw()+
  xlab("Weekday alcohol consumption")+
  ylab("Absences")+
  ggtitle("Weekday alcohol consumption vs Absences by sex")



length(which(merged_final$school=="MS"))

#setting the working directory
setwd("c:/Users/lalit/Desktop/student-alcohol-consumption/")

#read the csv files
student_mat=read.table("student-mat.csv",sep=",",header=TRUE)

#Integrating the data: merging student_mat.csv and student_por.csv 
#a.
merged<- rbind(student_mat,student_por)

#b.
#Removing duplicates, getting distinct instances only
merged_final <- merged %>% distinct(school,sex,age,address,famsize,Pstatus,
                                    Medu,Fedu,Mjob,Fjob,reason,
                                    guardian,traveltime,studytime,failures,
                                    schoolsup, famsup,activities,nursery,higher,internet,
                                    romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)

View(merged_final)
str(merged_final)

#constructing new variables
#a.
merged_final$avg_grade <- rowMeans(subset(merged_final,select = c(G1,G2,G3)), na.rm = TRUE)
merged_final$G1 <- NULL
merged_final$G2 <- NULL
merged_final$G3 <- NULL

#b.
#new variable for decision tree modeling
merged_final$result[merged_final$avg_grade>= 11.5 ] <-"pass"
merged_final$result[merged_final$avg_grade< 11.5 ] <-"fail"   


#formatting the data
#a.
#rounding the average grade to three digits.
merged_final$avg_grade <- round(merged_final$avg_grade,3)

#New Variable Using Only the final Grade
#Working with our merged_final dataset and using just the final grade
avg_grade1<-merged_final$avg_grade
#To print our our observations under the G3 attribute
avg_grade1
#To create new bins in preparation for our new variable
cut(avg_grade1, 5)
#Next we name the bins inorder to properly assess the final grades.
#Levels: (0,4] (4,8] (8,12] (12,16] (16,20]
cut(avg_grade1,5, labels = c("very low","low","Average","high","very high"))
#To append the new column with our new variable of final exam ranges:bins
merged_final$bins<-cut(avg_grade1,5,labels = c("very low","low","Average","high","very high"))

#To print out our new data_set with the attached new variable
merged_final
Code

#RANDOM FOREST CLASSIFICATION
library(dplyr)

#read the csv files
student_mat=read.table("student-mat.csv",sep=",",header=TRUE)
student_por=read.table("student-por.csv",sep=",",header=TRUE)

#Integrating the data: merging student_mat.csv and student_por.csv 
#a.
merged<- rbind(student_mat,student_por)

#b.
#Removing duplicates, getting distinct instances only
merged_final<-merged%>%distinct(school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,guardian,traveltime,studytime,failures,schoolsup,famsup,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)

#viewing merged_final
View(merged_final)
#examining the structure of merged_final
str(merged_final)

#constructing new variables
#a.
merged_final$avg_grade <- rowMeans(subset(merged_final,select = c(G1,G2,G3)), na.rm = TRUE)
merged_final$G1 <- NULL
merged_final$G2 <- NULL
merged_final$G3 <- NULL

merged_final$result[merged_final$avg_grade>= 11.5 ] <-"pass"
merged_final$result[merged_final$avg_grade< 11.5 ] <-"fail"   

#Classification:Random Forest
require(randomForest) 
set.seed(7777)
rf<- randomForest(avg_grade~., data=merged_final[,-32], ntree=1000, importance=T)
varImpPlot(rf,type = 1)

#Classification: Decision Trees

#selecting predictors that contribute most to the avg_grade
merged1 <- merged_final[,c(7,8,9,14,15,16,21,27,28,30,32)]
merged1$result <- as.factor(merged1$result)
str(merged1)

require(caTools)
#reproducibility
set.seed(9234) 
#splitting dataset merged1 into train and test data
sample = sample.split(merged1$result, SplitRatio = .75)
train = subset(merged1, sample == TRUE)
test  = subset(merged1, sample == FALSE)

#Build the classifier
m <- C5.0(x = train[,-11], y = train$result  ) 
#  result is the class variable so we need to exclude it from training
summary(m)

#predicting using test data
p <- predict(m, test)
#proportion table 
CrossTable(test$result, p, prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE, dnn = c("actual pass",
                                                   "predicted pass"))

#plotting decision tree
m1 <- rpart(train$result ~. , data = train, method = 'class')
#Plot it
prp(m1,varlen = 4, extra = 2,type = 4,faclen = 0,box.palette="RdYlGn",tweak = 1.3 )

#ASSOCIATION RULE MINING
#preparing data for association
new_merged_final <- data.frame(merged_final$failures, merged_final$higher,           merged_final$schoolsup,merged_final$Medu,merged_final$studytime,merged_final$Mjob,
                               merged_final$reason,merged_final$Fedu,merged_final$Walc,merged_final$Dalc,merged_final$result) 

#naming the columns of the data frame new_merged_final
names(new_merged_final)<-c("failures","higher","schoolsup","Medu","studytime","Mjob","reason",
                           "Fedu","Walc","Dalc","result")

#viewing new_merged_final
View(new_merged_final)

#changing datatype of necessary columns to type factor
new_merged_final <- data.frame(lapply(new_merged_final,as.factor))

#structure of new_merged_final
str(new_merged_final)

#install.packages( "arulesViz",dependencies = T) and its dependencies
library(arules)
library(arulesViz)

#running apriori for the subset new_merged_final
rules <- apriori(new_merged_final)

#examining summary of apriori
summary(rules)

#running the apriori again
#since rule length for 1 has only 3 instances, setting minlen to 2
#setting the support as .3
rules <- apriori(new_merged_final,parameter = list(minlen =2,supp =.3))
inspect(rules)

#decreasing the number of rules by setting minlen to 2, confidence to .6 and rhs only to pass
rules_pass <- apriori(new_merged_final,parameter = list(minlen =2, conf = .6),appearance = list(rhs=c("result=pass"),default = "lhs"))
inspect(rules_pass)

#ordering by descending order of lift
rules_pass.pruned <- sort(rules_pass,by="lift")

#examining the rules for rhs pass
inspect(rules_pass.pruned)

#plotting scatterplot for rhs PASS
plot(rules_pass)

#plotting group matrix for rules of rhs pass
plot(rules_pass, method ="grouped")

#plotting graph for rules of rhs pass
plot(rules_pass,method = "graph",control = list(type="items"))

#decreasing the number of rules by setting minlen to 2, confidence to .6 and rhs only to fail
rules_fail <- apriori(new_merged_final,parameter = list(minlen =2, conf = .6),appearance = list(rhs=c("result=fail"),default = "lhs"))
inspect(rules_fail)

#ordering by descending order of lift
rules_fail.pruned <- sort(rules_fail,by="lift")

#examining the rules for rhs fail
inspect(rules_fail.pruned)

#plotting scatterplot for rhs PASS
plot(rules_fail)

#plotting group matrix for rules of rhs fail
plot(rules_fail, method ="grouped")

##plotting graph for rules of rhs fail
plot(rules_fail,method = "graph",control = list(type="items"))

#Clustering 



new_merged_clustering <- data.frame(merged_final$age, merged_final$Medu, merged_final$Fedu ,merged_final$studytime,merged_final$traveltime,
                                    merged_final$absences,
                                    merged_final$failures,
                                    merged_final$Walc,
                                    merged_final$Dalc,
                                    merged_final$health,
                                    merged_final$avg_grade
                                    ,merged_final$famrel,
                                    merged_final$freetime
                                    ,merged_final$goout) 

#naming the columns of the data frame new_merged_final
names(new_merged_clustering)<-c("Age","Medu","Fedu","studytime","traveltime","absences",
                                "failures","Walc","Dalc","health","avg_grade","famrel","freetime","goout")



View(new_merged_clustering)

# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:20) {
  km.out <- kmeans(new_merged_clustering, centers = i, nstart = 9)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:20, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

#hence the elbow point is 5 we take number of centers for cluster as 5

km.out <- kmeans(new_merged_clustering,centers = 5 , nstart = 30)
km.out
km.out$cluster
km.out$size

# Plot of Student Absences vs. Avg_grades by Cluster Membership
plot(new_merged_clustering[,c("absences","avg_grade")],col = km.out$cluster,
     main = paste("K-Means clustering for Student Abscences and Average-grades with", 5, "clusters"),
     xlab = "Absences", ylab = "average grades")

library(dplyr)
#setting the working directory
setwd("c:/Users/lalit/Desktop/student-alcohol-consumption/")

#read the csv files
student_mat=read.table("student-mat.csv",sep=",",header=TRUE)
student_por=read.table("student-por.csv",sep=",",header=TRUE)

#Integrating the data: merging student_mat.csv and student_por.csv 
#a.
merged<- rbind(student_mat,student_por)

#b.
#Removing duplicates, getting distinct instances only
merged_final <- merged %>% distinct(school,sex,age,address,famsize,Pstatus,
                                    Medu,Fedu,Mjob,Fjob,reason,
                                    guardian,traveltime,studytime,failures,
                                    schoolsup, famsup,activities,nursery,higher,internet,
                                    romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)

View(merged_final)
str(merged_final)

#constructing new variables
#a.
merged_final$avg_grade <- rowMeans(subset(merged_final,select = c(G1,G2,G3)), na.rm = TRUE)
merged_final$G1 <- NULL
merged_final$G2 <- NULL
merged_final$G3 <- NULL

merged_final$result[merged_final$avg_grade>= 11.5 ] <-"pass"
merged_final$result[merged_final$avg_grade< 11.5 ] <-"fail"   

#Classification:Random Forest
require(randomForest) 
set.seed(7777)
rf<- randomForest(avg_grade~., data=merged_final[,-32], ntree=1000, importance=T)
varImpPlot(rf,type = 1)

#Classification: Decision Trees
library(plyr)
library(C50)
library(gmodels)
library(rpart.plot)

merged1 <- merged_final[,c(7,8,9,14,15,16,21,27,28,30,32)]
attach(merged1)
merged1$Medu<- as.factor(Medu)
merged1$Fedu<- as.factor(Fedu)
merged1$studytime<- as.factor(studytime)
merged1$failures <- as.factor(failures)
merged1$Dalc<- as.factor(Dalc)
merged1$Walc<- as.factor(Walc)
merged1$result <- as.factor(result)
detach()

str(merged1)

View(merged1)

set.seed(9234) 
sample = sample.split(merged1$result, SplitRatio = .75)
train = subset(merged1, sample == TRUE)
test  = subset(merged1, sample == FALSE)

#Build the classifier
m <- C5.0(x = train[,-11], y = train$result  ) 
#  result is the class variable so we need to exclude it from training
summary(m)

p <- predict(m, test)
CrossTable(test$result, p, prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE, dnn = c("actual pass",
                                                   "predicted pass"))

#plotting decision tree
m1 <- rpart(train$result ~. , data = train, method = 'class')
#Plot it
prp(m1,varlen = 5, extra = 2,type = 4,faclen = 5,box.palette="RdYlGn",tweak =1.3 )

#evaluation techniques for Decision Tree

accuracy <- (66+47)/186
accuracy

error_rate <- 1-accuracy
error_rate

false_positive_rate <- 39/86
false_positive_rate

true_positive_rate <- 47/81
true_positive_rate  

sensitivity<- 47/81
sensitivity

specificity <- 66/105
specificity

