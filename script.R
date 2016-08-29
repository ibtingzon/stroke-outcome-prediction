#load library
library(C50)
library(rpart)

#Load data and label features
istdb <- read.csv("C:/Users/issa/Dropbox/CS 176/Mini_Project/IST_DATABASE.csv")
istdata <- istdb[c("SEX", "AGE", "DASPLT", "RVISINF", "RSBP", "RDEF1","RDEF2","RDEF3","RDEF3","RDEF4","RDEF5","RDEF6","RDEF7", "STYPE", "DDIAGISC", "DDIAGHA", "DDIAGUN", "DNOSTRK")]

#Clean up data; set blank answers to U or unknown
levels(istdata$DNOSTRK )[1] = "U"
levels(istdata$DDIAGUN)[1] = "U"
levels(istdata$DDIAGHA)[1] = "U"
levels(istdata$DDIAGISC)[1] = "U"
levels(istdata$DASPLT)[1] = "U"

#Assign DIAG attributes as either DDEAD, FDEAD, DALIVE, FRECOVER, FDENNIS, UNKNOWN
PROG <- matrix(0, ncol = 1, nrow = nrow(istdb))
istdb.PROG <- data.frame(PROG)
levels(istdb.PROG$PROG) <- c("DDEAD", "FDEAD", "DALIVE", "FRECOVER", "FDENNIS", "UNKNOWN")
for (i in 1:nrow(istdb)){
  if (istdb$DDEAD[i] == "Y"){
    istdb.PROG$PROG[i] <- "DDEAD"
  } else if (istdb$FDEAD[i] == "Y"){
    istdb.PROG$PROG[i] <- "DDEAD"
  } else if (istdb$DALIVE[i] == "Y"){
    istdb.PROG$PROG[i] <- "DALIVE"
  } else if (istdb$FRECOVER[i] == "Y"){
    istdb.PROG$PROG[i] <- "DALIVE"
  } else if (istdb$FDENNIS[i] == "Y"){
    istdb.PROG$PROG[i] <- "FDENNIS"
  } else {
    istdb.PROG$PROG[i] <- "UNKNOWN"
  }
}

#Class Labels
istdata["PROG"] <- istdb.PROG
levels(istdata$PROG) <- c("DDEAD", "FDEAD", "DALIVE", "FRECOVER", "FDENNIS", "UNKNOWN")
istdata$PROG <- factor(istdata$PROG)

testingSet <- istdata[15549:19435,]
trainingSet <- istdata[1:15548,]

X<-trainingSet[,1:18]
y<-trainingSet$PROG

X1<-testingSet[,1:18]
y1<-testingSet$PROG

#Generate decision tree model
treeModel <- C5.0(PROG ~ ., data=trainingSet) 

summary(treeModel)
sink(file = "ISDB")
summary(rules)
sink(NULL)

pred <- predict(treeModel, X1)
sum(pred == y1) / length(pred)

boostTreeModel <- C5.0(PROG ~ ., data = trainingSet, trials = 5)
pred <- predict(boostTreeModel, X1)
summary(pred)
summary(boostTreeModel)
sum(pred == y1) / length(pred)

#Generate rules
rules <- C5.0(PROG ~ ., data=istdata, rules = TRUE)
summary(rules)

#creates a file called ISDB containing all the rules
sink(file = "ISDB")
summary(rules)
sink(NULL)

#CART Model <-- is really mess so just ignore it. :P
form <- as.formula(PROG ~ .)
fit <- rpart(form,data=istdata,control=rpart.control(minsplit=20,cp=0))
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
plot(pfit, uniform=TRUE)
text(pfit, use.n=TRUE, all=TRUE, cex=.5)
