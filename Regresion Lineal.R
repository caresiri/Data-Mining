library(ISLR); library(ggplot2); library(caret)

setwd(easycsv::choose_dir()) ## seleccione el directorio con el que va a trabajar
Energy <- read.csv(file="Energy Efficiency of Buildings.csv", sep = ";")
str(Energy)
#Energy$Orientation <- as.factor(Energy$Orientation)


inTrain <- createDataPartition(y=Energy$HeatingLoad, 
                               p = 1, list = FALSE) #Create training set
training <- Energy[inTrain,-9]; testing <- Energy[-inTrain,-9]
train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)
dim(training)
dim(testing)

featurePlot(x=training[,c("OverallHeight", "RoofArea", "Orientation")],
            y = training$HeatingLoad,
            plot = "pairs")

qplot(SurfaceArea, HeatingLoad, data=training)
qplot(SurfaceArea, HeatingLoad, colour=Orientation, data=training)

df2 = cor(training[,-9])
hc = findCorrelation(df2, cutoff=0.9) # putt any value as a "cutoff" 
hc = sort(hc)
reduced_Data = training[,-c(hc)]
print (reduced_Data)


modFit <- train(HeatingLoad ~ ., trControl=train_control, 
                method = "glm", data = reduced_Data)


finMod <- modFit$finalModel
print(modFit)

finMod

