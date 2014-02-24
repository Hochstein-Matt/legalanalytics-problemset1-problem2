legalanalytics-problemset1-problem2
===================================
#Read in the wine data
> wine <- read.csv("http://www.nd.edu/~mclark19/learn/data/goodwine.csv")

> summary(wine)
 fixed.acidity    volatile.acidity  citric.acid     residual.sugar     chlorides       free.sulfur.dioxide
 Min.   : 3.800   Min.   :0.0800   Min.   :0.0000   Min.   : 0.600   Min.   :0.00900   Min.   :  1.00     
 1st Qu.: 6.400   1st Qu.:0.2300   1st Qu.:0.2500   1st Qu.: 1.800   1st Qu.:0.03800   1st Qu.: 17.00     
 Median : 7.000   Median :0.2900   Median :0.3100   Median : 3.000   Median :0.04700   Median : 29.00     
 Mean   : 7.215   Mean   :0.3397   Mean   :0.3186   Mean   : 5.443   Mean   :0.05603   Mean   : 30.53     
 3rd Qu.: 7.700   3rd Qu.:0.4000   3rd Qu.:0.3900   3rd Qu.: 8.100   3rd Qu.:0.06500   3rd Qu.: 41.00     
 Max.   :15.900   Max.   :1.5800   Max.   :1.6600   Max.   :65.800   Max.   :0.61100   Max.   :289.00     
 total.sulfur.dioxide    density             pH          sulphates         alcohol         quality     
 Min.   :  6.0        Min.   :0.9871   Min.   :2.720   Min.   :0.2200   Min.   : 8.00   Min.   :3.000  
 1st Qu.: 77.0        1st Qu.:0.9923   1st Qu.:3.110   1st Qu.:0.4300   1st Qu.: 9.50   1st Qu.:5.000  
 Median :118.0        Median :0.9949   Median :3.210   Median :0.5100   Median :10.30   Median :6.000  
 Mean   :115.7        Mean   :0.9947   Mean   :3.219   Mean   :0.5313   Mean   :10.49   Mean   :5.818  
 3rd Qu.:156.0        3rd Qu.:0.9970   3rd Qu.:3.320   3rd Qu.:0.6000   3rd Qu.:11.30   3rd Qu.:6.000  
 Max.   :440.0        Max.   :1.0390   Max.   :4.010   Max.   :2.0000   Max.   :14.90   Max.   :9.000  
   color          white          good     
 red  :1599   Min.   :0.0000   Bad :2384  
 white:4898   1st Qu.:1.0000   Good:4113  
              Median :1.0000              
              Mean   :0.7539              
              3rd Qu.:1.0000              
              Max.   :1.0000      

# Correlation Matrix
> corrplot(cor(wine[, -c(13, 15)]), method = "number", tl.ce = 0.5)

> set.seed(1234) #so that the indices will be the same when re-run
> trainIndices = createDataPartition (wine$good, p = 0.8, list = F)
> Wanted = !colnames(wine) %in% c("free.sulfur.dioxide", "density", "quality", "color", "white")
> wine_train = wine[trainIndices, Wanted] #remove quality and color, as well as density and others
> wine_test = wine[-trainIndices, Wanted]


> wine_trainplot = predict(preProcess(wine_train[,-10], method="range"), wine_train[, -10])
> featurePlot(wine_trainplot, wine_train$good, "box")

set.seed(1234)
> cv_opts = trainControl(method="cv", number=10)
> knn_opts = data.frame(.k=c(seq(3, 11, 2), 25, 51, 101)) #odd to avoid ties
> results_knn = train(good~., data=wine_train, method="knn",
+                     preProcess="range", trControl=cv_opts,
+                     tuneGrid = knn_opts)

> show(results_knn)
k-Nearest Neighbors 

5199 samples
   9 predictors
   2 classes: 'Bad', 'Good' 

Pre-processing: re-scaling to [0, 1] 
Resampling: Cross-Validated (10 fold) 

Summary of sample sizes: 4679, 4679, 4679, 4679, 4680, 4679, ... 

Resampling results across tuning parameters:

  k    Accuracy  Kappa  Accuracy SD  Kappa SD
  3    0.748     0.449  0.0177       0.0387  
  5    0.742     0.433  0.0224       0.0498  
  7    0.75      0.447  0.0194       0.0439  
  9    0.753     0.453  0.0166       0.0374  
  11   0.751     0.448  0.0205       0.0463  
  25   0.75      0.444  0.0228       0.055   
  51   0.747     0.435  0.0195       0.0463  
  101  0.745     0.425  0.0168       0.04    

Accuracy was used to select the optimal model using  the largest value.
The final value used for the model was k = 9. 

> pred_knn = predict(results_knn, wine_test[, -10])
> confusionMatrix(pred_knn, wine_test[, 10], positive='Good')
Confusion Matrix and Statistics

          Reference
Prediction Bad Good
      Bad  284  127
      Good 192  695
                                          
               Accuracy : 0.7542          
                 95% CI : (0.7299, 0.7774)
    No Information Rate : 0.6333          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.4552          
 Mcnemar's Test P-Value : 0.0003393       
                                          
            Sensitivity : 0.8455          
            Specificity : 0.5966          
         Pos Pred Value : 0.7835          
         Neg Pred Value : 0.6910          
             Prevalence : 0.6333          
         Detection Rate : 0.5354          
   Detection Prevalence : 0.6834          
      Balanced Accuracy : 0.7211          
                                          
       'Positive' Class : Good      

> dotPlot(varImp(results_knn))

