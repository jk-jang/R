##################################################
#Support Vector Machine 
##################################################
# SVM 알고리즘 - 두 범주를 직선으로 분류(이진분류) 
# 선형분리 - 2개의 집합을 직선으로 분리(초평면 분리) 
# 초평면(Separating Hyperplane) : 2차원 이상 공간에서 평면 
# 가상의 직선을 중심으로 거리를 계산하여 직사각형 형태로 영역 확장
# -> 가장 가까운 점이 만날때 까지 영역  확장 
# Margin : 직사각형의 넓이(최대값을 구하는 것이 관건)
# Support Vector : 마진과 가장 가까운 점들

# 바이오인포매틱스의 유전자 데이터 분류 
# 용도 : 인간의 얼굴, 문자, 숫자 인식(이미지 데이터 패턴 인식) 
# 예) 스캐너로 스캔된 문서 이미지를 문자로 인식 


###############################################
####### e1071 패키지 
###############################################
# 관련 패키지(e1071, kernlab, klaR 등) 4개 중 e1071 가장 많이 사용함 

install.packages('e1071')
library(e1071)  

# 1. SVM 기본 개념 익히기 - Support Vector, Margin
df = data.frame(
  x1 = c(1,2,1,2,4,5,6),
  x2 = c(8,7,5,6,1,3,2),
  y=factor(c(1,1,1,1,0,0,0))
)

# 2. svm 모델 생성 
# 형식) svm(y ~ x, data, type, kernel) : method, kernel 속성 생략 가능
model_svm = svm(y ~ ., data = df)
# default 속성 : , type = "C-classification", kernel="radial"
# type : 분리 방식 
# kernel : 비선형(non linear) 관계를 선형적(linear)으로 변환하는 역할 
# kernel 종류 : linear, polynomial, radial, sigmoid

model_svm
summary(model_svm)

# svm 모델 시각화 
par(mfrow=c(1,1))
plot(df$x1, df$x2, col=df$y)  
X11()
plot(model_svm, df) # 분류 Factor levels에 의해서 2개 분류 


# 3. kernel="linear" 변경 
model_svm2 = svm(y~., data=df, method = "C-classification", kernel="linear")
model_svm2


############################
# iris 데이터 실습 
############################

# 1. 데이터셋 생성 
data(iris)
set.seed(415) # random 결과를 동일하게 지
idx = sample(1:nrow(iris), 0.7*nrow(iris))
training = iris[idx, ]
testing = iris[-idx, ]
training
testing
dim(training) # 105
dim(testing) # 45


# 2. 분류모델 생성 
model_svm = svm(Species ~ ., data = training, na.action =na.omit)
summary(model_svm)


# 3. 분류모델 성능 평가(testing set 적용 예측값 생성)  
pred <- predict(model_svm, testing)

# 혼돈 matrix 작성 
table(pred, testing$Species)

##################################################
#Support Vector Machine 문제 : letterdata.csv
##################################################

#뒤틀린 영어 알파벳 대문자 26개를 무작위 순서로 배열한 후 
#컴퓨터로 스캔하여 각 문자별로 수직, 수평의 차원과 픽셀 비율,
#픽셀의 수직 위치와 수평 위치의 평균 등의 특징을 측정하여
#기록한 데이터 셋으로 20,000개의 관측치와 17개의 칼럼으로 
#구성되어 있다.
#y변수 : letters -> 영어 알파벳 대문자 26개 Factor형
#x변수 : 나머지 16개 칼럼으로 영어 알파벳 대문자의 특징   
str(letterdata) # 'data.frame':	20000 obs. of  17 variables:
# y : letter, x : 나머지 16

# 2. 데이터 셋 생성 
set.seed(415)
idx = sample(1:nrow(letterdata), 0.7*nrow(letterdata))
training_l = letterdata[idx, ]
testing_l  = letterdata[-idx, ]

# 3. NA 제거 
training_l2 = na.omit(training_l)
testing_l2 = na.omit(testing_l)

# 4. 분류모델 생성 
model_l <- svm(letter~., data = training_l2)

# 5. 분류모델 평가 
pre_l <- predict(model_l, testing_l2)
pre_l[1:10]

table(pre_l, testing_l2$letter)

# 정분류율 계산 = 94%
result <- pre_l == testing_l2$letter
prop.table(table(result))



