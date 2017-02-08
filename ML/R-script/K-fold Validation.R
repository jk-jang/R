test_idx <- createDataPartition(iris$Species, p=.1)$Resample1
iris.test <- iris[test_idx, ]
iris.train <- iris[-test_idx,]


create_ten_fold_cv <- function(){
  set.seed(137)
  lapply(createFolds(iris.train$Species, k=3), function(idx){
    return(list(train=iris.train[-idx, ],
                validation=iris.train[idx,]))
  })
}
x <- create_ten_fold_cv()
str(x)
####

###4. 10 folds 교차 검증(모델 적용)
# create_ten_fold_cv() 함수는 train에 훈련 데이터를 validation에 검증 데이터를 담은
#리스트의 리스트를 반환

library(foreach)
folds <- create_ten_fold_cv()
library(randomForest)
####랜덤포레스트 적용
rf_result <- foreach(f=folds) %do% {
  model_randomforest <- randomForest(Species~., data=f$train, ntree=400, mtry=3, importance=TRUE)
  predicted <- predict(model_randomforest, newdata=f$validation, type="class")
  return(list(actual=f$validation$Species, predicted=predicted))
}


#####변수 중요도 평가
#MeanDecreaseAccuracy, MeanDecreaseGini : 클 수록 중요한 변수
importance(model_randomforest)
library(caret)
varImp(model_randomforest)

# 5. 정확도 평가
#평가 함수
evaluation <- function(lst){
  accuracy <- sapply(lst, function(one_result){
    return(sum(one_result$predicted == one_result$actual) / NROW(one_result$actual))
  })
  print(sprintf("MEAN +/- SD: %.3f +/- %.3f", mean(accuracy), sd(accuracy)))
  return(accuracy)
}
#정확도 확인
(rf_accuracy <- evaluation(rf_result))

## 정확도: accuracy 기준
## 랜덤포레스트 94%

##평가 방법: Precision, Recall, Accuracy(정분류율)


