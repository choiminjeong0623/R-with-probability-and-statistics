# 데이터 불러오기
winedata <- read.csv("./resource//winequality-red.csv",
                     sep  =";",
                     header = T,
                     stringsAsFactors = TRUE,
                     encoding="utf-8")
dim(winedata)

# 데이터 확인하기
# 자료형 확인하기
str(winedata)
summary(winedata)

# 범주형(quality) 데이터 기술통계
cnt = table(winedata$quality)
prop = prop.table(cnt)
cbind(cnt, prop)
barplot(cnt)
pie(cnt)


# 연속형 데이터 기술통계
summary(winedata)

# 각 연속형 변수의 표준편차 계산하기
continuous_vars <- names(winedata)[sapply(winedata, is.numeric)]
std_devs <- sapply(winedata[continuous_vars], sd)
std_devs

library(e1071)

# 각 연속형 변수의 표준편차 계산하기
skewness_wine <- sapply(winedata[continuous_vars], skewness)
skewness_wine
kurtosis_wine <- sapply(winedata[continuous_vars], kurtosis)
kurtosis_wine

data1 <- winedata$density
data2 <- winedata$pH
data3 <- winedata$volatile.acidity

## 이상치 탐색하기
# boxplot으로 확인하기
# 그래프 파라미터 설정하기
num_columns <- ncol(winedata)
par(mfrow=c(ceiling(num_columns/2), 2), mar=c(4,4,2,1), oma=c(1,1,2,1))

# 각 속성별 박스플롯롯 그리기
for (i in 1:num_columns) {
  boxplot(winedata[,i], main=colnames(winedata)[i])
}

# 그래프 복원
par(mfrow=c(1,1))

# 표준점수로 찾기
testdata <- winedata$residual.sugar

# 평균과 표준편차 계산하기
mean_scores <- mean(testdata)
sd_sroces <- sd(testdata)

# 표준점수 계산하기
z_scores <- (testdata - mean_scores) / sd_scores
z_scores

# 이상치 찾기(표준점수가 3 이상)
outlier_indices <- which(abs(z_scores) > 3)
print(outlier_indices)
print(length(outlier_indices))

