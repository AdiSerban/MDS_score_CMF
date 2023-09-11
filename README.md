# MDS_score_CMF
Score for Myelodyspastic sindrome diagnosis trough flow cytomery
rm(list=ls())
library(readxl)
library(rpart)
library(rpart.plot)
library("ggplot2")
library(tidyr)
library(datasets)
library(caret)
library(randomForest)

All_data <- read_excel("~/R Codes/All_data.xlsx", 
                         +     col_types = c("text", "numeric", "numeric", 
                                                   +  "numeric", "numeric", "numeric", 
                                                    + "numeric", "numeric", "numeric", 
                                                    +  "numeric", "numeric", "numeric", 
                                                   +  "numeric", "numeric", "numeric", 
                                                    + "numeric", "numeric", "numeric", 
                                                   +  "numeric", "numeric", "numeric", 
                                                    + "numeric", "numeric", "numeric", 
                                                    +"numeric"))
View(All_data)                                                                            
# Load the necessary packages

# Load the data
data <- Final_SMDdatamining


All_data<-as.data.frame(All_data)
Final_SMDdatamining <- na.omit(Final_SMDdatamining)
# Split the data into training and testing sets
set.seed(123)
train_index <- sample(nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Build the decision tree model
model <- rpart(Final_SMDdatamining$`Patient-ID` ~ ., data = data)
# Plot the decision tree
rpart.plot(model)
predictions <- predict(model, newdata =data)
plotcp(model)

#regresiias
All_data<-as.data.frame(All_data)
All_data<-as.numeric(All_data)
Names<-colnames(All_data)

#data to graph

#Bucla
i<-2
j<-2
for (i in 2:(25))
{
  xgraf<-All_data[,i]
  
  for (j in 2:(25))
  {
    ygraf<-All_data[,j]
    
    #calcul of plot
    cinqx<-xgraf[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)]
    cinqy<-ygraf[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)]
    septqx<-xgraf[c(39,40,41,42,43,44,45,46,47,48,49,50,51)]
    septqy<-ygraf[c(39,40,41,42,43,44,45,46,47,48,49,50,51)]
    BMTx<-xgraf[c(52,53,54,55,56,57,58,59,60,61,62,63,64)]
    BMTy<-ygraf[c(52,53,54,55,56,57,58,59,60,61,62,63,64)] 
    CNx<-xgraf[c(65,66,67,68,69,70,71,72,73,74,75,76,77,78,79)]
    CNy<-ygraf[c(65,66,67,68,69,70,71,72,73,74,75,76,77,78,79)]
    DAx<-xgraf[c(80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108)]
    DAy<-ygraf[c(80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108)]
    Delx<-xgraf[c(109,110,111,112,113,114,115,116,117,118,119,120,121,122,123)]
    Dely<-ygraf[c(109,110,111,112,113,114,115,116,117,118,119,120,121,122,123)]
    HAx<-xgraf[c(124,125,126,127,128,129,130,131,132,133,134,135)]
    HAy<-ygraf[c(124,125,126,127,128,129,130,131,132,133,134,135)]
    HDx<-xgraf[c(136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159)]
    HDy<-ygraf[c(136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159)]
    IBMx<-xgraf[c(160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182)]
    IBMy<-ygraf[c(160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182)]
    ITPx<-xgraf[c(183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203)]
    ITPy<-ygraf[c(183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203)]
    MDSx<-xgraf[c(204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241)]
    MDSy<-ygraf[c(204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241)]
    MDSTx<-xgraf[c(242,243,244,245,246,247,248,249,250,251,252,253,254)]
    MDSTy<-ygraf[c(242,243,244,245,246,247,248,249,250,251,252,253,254)]
    
    Ax<-Names[i]
    Ay<-Names[j]
    pdf(file=paste(Ax,Ay,".pdf", sep="_", collapse = NULL), height=9, width=16)
    
    plot(xgraf,ygraf, type="n", main= paste(Ax, Ay, sep= "/"), xlab = Ax, ylab = Ay)
    points(cinqx,cinqy, pch=19, col= "#00D3CE" )
    points(septqx,septqy, pch=19, col="#C8D300"  )
    points(BMTx,BMTy, pch=19, col= "#DD00C8" )
    points(CNx,CNy, pch=19, col= "#006968" )
    points(DAx,DAy, pch=19, col= "#FF9A00" )
    points(Delx,Dely, pch=19, col= "#8B0000" )
    points(HAx,HAy, pch=19, col= "#FF0000")
    points(HDx,HDy, pch=19, col= "#1D0083" )
    points(IBMx,IBMy, pch=19, col= "#00FF01" )
    points(ITPx,ITPy, pch=19, col= "#008905" )
    points(MDSx,MDSy, pch=19, col="yellow")
    points(MDSTx,MDSTy, pch=19, col= "#61646E" )
    dev.off()
    j<-j+1
  }
  i<-i+1
}

library(datasets)
library(caret)


train_index <- sample(nrow(All_data), 0.8 * nrow(All_data))
train_data <- All_data[train_index, ]
test_data <- All_data[-train_index, ]

All_data$Patient<-as.factor(All_data$Patient) 
rf<-randomForest(All_data$'Patient'~.,data=All_data, mtry=15, ntree=201, importance= TRUE)

rf
varImpPlot(rf)
importance(rf)
pred_test <- predict(rf, newdata = test_data, type= "class")
confusionMatrix(table(pred_test,test_data$Patient))

All_data<-All_data[,-c(1)]

install.packages("rattle")
library(rattle)
printRandomForests(rf)

# test avec separation colone Patient

View(All_data)                                                                            
DAtabase<-All_data
All_data$Patient<-as.factor(All_data$Patient) 
train_index <- sample(nrow(All_data), 0.8 * nrow(All_data))
train_data <- All_data[train_index, ]
test_data <- All_data[-train_index, ]
 Trainpatient<-train_data$Patient
 Testpatient<-test_data$Patient
 train_data<-train_data[,-c(1)]
 test_data<-test_data[,-c(1)]
 rf<-randomForest(Trainpatient~.,data=train_data, mtry=15, ntree=201, importance= TRUE)
 rf
pred_test <- predict(rf, newdata = test_data, type= "class")
 confusionMatrix(table(pred_test,Testpatient))
