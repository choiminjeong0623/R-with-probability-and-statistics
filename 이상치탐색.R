## 이상치 찾기
## 1. 표준점수(Z-Score) 방법
st <- data.frame(state.x77)
testdata <- st$Income
mean_scores <- mean(testdata)
sd_scores <- sd(testdata)
z_scores <- (testdata - mean_scores) / sd_scores
outliers <- abs(z_scores) > 3
print(outliers)

## 이상치 찾기
## 2. IQR 방법
## 제1사분위수(Q1)와 제3사분위수(Q3) 계산
Q1 <- quantile(testdata, 0.25)
Q3 <- quantile(testdata, 0.75)

## IQR 계산하기
IQR <- Q3 - Q1

## 임계값 계산하기
lower_bound <- Q1 - 1.5 *IQR
upper_bound <- Q3 + 1.5 * IQR

## 이상치 찾기
outliers <- testdata < lower_bound | testdata > upper_bound
print(outliers)


## 3. 클러스터링 알고리즘
## 랜덤 데이터 생성하기
set.seed(1)
n <- 200
data <- rbind(matrix(rnorm(2*n), ncol=2))
matrix(rnorm(2*n, mean=3), ncol=2)

## 필요한 라이브러리 설치하기
install.packages("dbscan")
library(dbscan)

## DBSCAN 알고리즘을 실행한다.
dbscan_res <- dbscan(data, eps=0.6, minPts = 5)
dbscan_res

outliers <- data[dbscan_res$cluster == -1]
outliers

## 4. 회귀모델
## 회귀 모델 생성하기

## Boston 데이터셋 로드하기
data(Boston, package="MASS")
## medv를 종속변수로, 다른 모든 변수들을 독립변수로 하는 선형회귀 모델 생성
model <- lm(medv~., data=Boston)
summary(model)

## 잔차 계산하기
residuals <- abs(resid(model))
residuals

## 잔차의 표준화
z_scores <- scale(residuals)
z_scores

## 이상치 찾기(표준화된 잔차가 3을초과하는 경우)
outliers <- abs(z_scores) > 3
print(Boston[outliers,])
