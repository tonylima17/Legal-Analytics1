wine = read.csv("http://www.nd.edu/~mclark19/learn/data/goodwine/csv")
summary(wine)

## fixed.acidity    volatile.acidity  citric.acid    
## Min.   : 3.800   Min.   :0.0800   Min.   :0.0000  
## 1st Qu.: 6.400   1st Qu.:0.2300   1st Qu.:0.2500  
## Median : 7.000   Median :0.2900   Median :0.3100  
## Mean   : 7.215   Mean   :0.3397   Mean   :0.3186  
## 3rd Qu.: 7.700   3rd Qu.:0.4000   3rd Qu.:0.3900  
## Max.   :15.900   Max.   :1.5800   Max.   :1.6600  
## residual.sugar     chlorides       free.sulfur.dioxide
## Min.   : 0.600   Min.   :0.00900   Min.   :  1.00     
## 1st Qu.: 1.800   1st Qu.:0.03800   1st Qu.: 17.00     
## Median : 3.000   Median :0.04700   Median : 29.00     
## Mean   : 5.443   Mean   :0.05603   Mean   : 30.53     
## 3rd Qu.: 8.100   3rd Qu.:0.06500   3rd Qu.: 41.00     
## Max.   :65.800   Max.   :0.61100   Max.   :289.00     
## total.sulfur.dioxide    density             pH       
## Min.   :  6.0        Min.   :0.9871   Min.   :2.720  
## 1st Qu.: 77.0        1st Qu.:0.9923   1st Qu.:3.110  
## Median :118.0        Median :0.9949   Median :3.210  
## Mean   :115.7        Mean   :0.9947   Mean   :3.219  
## 3rd Qu.:156.0        3rd Qu.:0.9970   3rd Qu.:3.320  
## Max.   :440.0        Max.   :1.0390   Max.   :4.010  
## sulphates         alcohol         quality        color     
## Min.   :0.2200   Min.   : 8.00   Min.   :3.000   red  :1599  
## 1st Qu.:0.4300   1st Qu.: 9.50   1st Qu.:5.000   white:4898  
## Median :0.5100   Median :10.30   Median :6.000               
## Mean   :0.5313   Mean   :10.49   Mean   :5.818               
## 3rd Qu.:0.6000   3rd Qu.:11.30   3rd Qu.:6.000               
## Max.   :2.0000   Max.   :14.90   Max.   :9.000               
## white          good     
## Min.   :0.0000   Bad :2384  
## 1st Qu.:1.0000   Good:4113  
## Median :1.0000              
## Mean   :0.7539              
## 3rd Qu.:1.0000              
## Max.   :1.0000 

library(corrplot)
corrplot(cor(wine[, -c(13, 15)]), method = "number", tl.cex = 0.5)
install.packages('caret')
library(caret)
set.seed(1234)
trainIndices <- createDataPartition(wine$good, p = 0.8, list = F)
wanted = !colnames(wine) %in% c("free.sulfur.dioxide", "density", "quality", "color", "white")
wine_train <- wine[trainIndices, wanted]
wine_test <- wine[-trainIndices, wanted]
wine_trainplot <- predict(preProcess(wine_train[,-10], method="range"),wine_train[,-10])
featurePlot(wine_trainplot, wine_train$good, "box")
set.seed(1234)
cv_opts <- trainControl(method="cv", number=10)
knn_opts <- data.frame(.k=c(seq(3, 11, 2), 25, 51, 101))
results_knn <- train(good~., data=wine_train, method="knn", preProcess="range", trControl=cv_opts, tuneGrid = knn_opts)
install.packages("e1071")
results_knn
## k-Nearest Neighbors 

## 5199 samples
## 9 predictor
## 2 classes: 'Bad', 'Good' 

## Pre-processing: re-scaling to [0, 1] 
## Resampling: Cross-Validated (10 fold) 

## Summary of sample sizes: 4679, 4680, 4680, 4679, 4679, 4678, ... 

## Resampling results across tuning parameters:
  
##   k    Accuracy   Kappa    
## 3  0.7501380  0.4529401
## 5  0.7514853  0.4521043
## 7  0.7491780  0.4456167
## 9  0.7505208  0.4465035
## 11  0.7501362  0.4467934
## 25  0.7518766  0.4475483
## 51  0.7445637  0.4275101
## 101  0.7424476  0.4191436
## Accuracy SD  Kappa SD  
## 0.01773698   0.04122014
## 0.01975451   0.04778807
## 0.01783406   0.04275751
## 0.01804636   0.04191760
## 0.01660755   0.03845927
## 0.02064425   0.04929378
## 0.01734204   0.04066191
## 0.01334602   0.03422896

## Accuracy was used to select
## the optimal model using 
## the largest value.
## The final value used for the
## model was k = 25.

preds_knn <- predict(results_knn, wine_test[,-10])
confusionMatrix(preds_knn, wine_test[,10], positive='Good')

## Confusion Matrix and Statistics

## Reference
## Prediction Bad Good
## Bad  281  130
## Good 195  692

## Accuracy : 0.7496         
## 95% CI : (0.7251, 0.773)
## No Information Rate : 0.6333         
## P-Value [Acc > NIR] : < 2.2e-16      

## Kappa : 0.445          
## Mcnemar's Test P-Value : 0.0003851      

## Sensitivity : 0.8418         
## Specificity : 0.5903         
## Pos Pred Value : 0.7802         
## Neg Pred Value : 0.6837         
## Prevalence : 0.6333         
## Detection Rate : 0.5331         
## Detection Prevalence : 0.6834         
## Balanced Accuracy : 0.7161         

## 'Positive' Class : Good
dotPlot(varImp(results_knn))
