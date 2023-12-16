# 데이터 불러오기
adult <- read.csv("./resource/adult.data", sep = ","
                  , header=F, stringsAsFactors=TRUE
                  , encoding='utf-8')

# data에 column name 생성하기
colnames(adult) <- c("age", "workclass", "fnlwgt"
                     , "education", "educationnum"
                     , "maritalstatus", "occupation"
                     , "relationship", "race"
                     , "sex","capitalgain", "capitalloss"
                     , "hoursperweek", "nativecountry"
                     , "income")
# data 확인하기
dim(adult)

# 자료형 확인하기
str(adult)
summary(adult)

# 범주형(qaulity) 데이터 기술통계
cnt = table(adult$workclass)
prop = prop.table(cnt)
cbind(cnt, prop)
barplot(cnt)
pie(cnt)

# 각 연속형 변수의 표준편차 계산하기
# names() : 객체(벡터, 리스트, 데이터 프레임 등)의 이름을
# 가져오거나 설정하는 데 사용된다.
# sapply() : 벡터, 리스트 또는 데이터 프레임과 같은 객체에 함수를 적용
# 하는 데 사용된다. 이 함수는 입력 객체의 각 요소에 함수를 적용하여 
# 결과를 반환한다.
continuous_vars <- names(adult)[sapply(adult, is.numeric)]
std_devs <- sapply(adult[continuous_vars], sd)
std_devs

library(e1071)

# 각 연속형 변수의 표준편차를 계산한다.
skewness_wine <- sapply(adult[continuous_vars], skewness)
skewness_wine
kurtosis_wine <- sapply(adult[continuous_vars], kurtosis)
kurtosis_wine

# 이상치 탐색
# boxplot으로 확인하기
# 이상치 개수 테이블 만들기
# which() : 조건을 만족하는 원소의 인덱스를 반환하는 R의 유용한 함수이다.
# 이 함수는 주어진 조건을 만족하는 원소가 있는 위치(인덱스)를 찾을 때 사용된다.
num_columns <- which(sapply(adult, is.numeric))
df <- data.frame(Name=character(0), Z_count=numeric(0), IQR_count=numeric(0))
df

# 각 속성별 히스토그램 그리기
for (i in num_columns) {
  # 표준점수방법
  mean_scores <- mean(adult[,i])
  sd_scores <- sd(adult[,i])
  
  # 표준점수 계산하기
  z_scores <- (adult[,i] - mean_scores) / sd_scores
  
  # 이상치 찾기(표준점수가 3이상)
  outliers_indices <- which(abs(z_scores) > 3)
  
  # IQR 방법
  Q1 <- quantile(adult[,i], 0.25)
  Q3 <- quantile(adult[,i], 0.75)

  # IQR 계산하기
  IQR <- Q3 - Q1
  
  # 임계값 계산하기
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers_indices2 <- which(adult[,i] < lower_bound | adult[,i] > upper_bound)
  
  # 행 추가하기
  new_row <- data.frame(Name=colnames(adult)[i]
                        , Z_count = length(outlier_indices)
                        , IQR_count = length(outliers_indices2))
  
  df <- rbind(df, new_row)
}

# 결측치 탐색하기
# 1. 각 컬럼별로 결측치의 개수 확인하기
missing_values <- sapply(adult, function(x) sum(is.na(x)))
missing_values

# 대표성 확인하기
# 확률분포 확인하기
num_columns <- which(sapply(adult, is.numeric))
num_columns

# par() : R의 그래픽 매개변수를 설정하는 데 사용된다. 이 함수는 그래픽 장치에 대한
# 매개변수를 설정하고 조절하여 플롯(plot)의 외관 및 특성을 제어하는 데 활용된다.
par(mfrow=c(ceiling(length(num_columns)/2),2), mar=c(4,4,2,1), oma=c(1,1,2,1))

# 각 속성별 히스토그램
for(i in num_columns) {
  hist(adult[,i], main=colnames(adult)[i])
}

# 그래프 복원하기
par(mfrow=c(1,1))

# 특정 연속형 변수가 정규분포를 따르는지 확인하기
# Anderson-Darling 검정
# 데이터가 정규분포를 따르는지를 확인하는 검정 중 하나이다.
# H0 : 데이터가 정규분포를 따른다
# H1 : 데이터가 정규분포를 따르지 않는다.
# A = 검정통계량
# p-value : 0에 가까운 매우 작은 값
# p-value가 작은 경우, H0을 기각할 근거가 충분함을 의미한다.
# 따라서, 해당 데이터는 정규분포를 따르지 않는다.
install.packages("nortest")
library(nortest)

ad_result <- ad.test(adult$hoursperweek)
print(ad_result)

# options : R의 동작에 영향을 미치는 다양한 옵션들을 설정하고 관리하는 데 사용된다.
# scipen : 과학적 표기법을 사용할 때의 자릿수를 결정한다.
options(scipen = 100)

num_columns <- which(sapply(adult, is.numeric))
par(mfrow=c(ceiling(length(num_columns)/2),2), mar=c(4,4,2,1), oma=c(1,1,2,1))

# 각 속성별 boxplot 그리기
for(i in num_columns) {
  boxplot(adult[,i] ~ income
           ,data = adult
           ,main = paste(colnames(adult)[i])
           , "Distribution by Income"
           , xlab="Income"
           , ylab=colnames(adult)[i])
}

# 범주형-범주형 변수간의 관계 파악하기
categorical_vars <- names(adult)[sapply(adult, is.factor)]

# "income"은 검정 대상에서 제외한다.
categorical_vars <- categorical_vars[categorical_vars != "income"]

# 독립성 검증 결과를 저장할 데이터프레임워크를 초기화한다.
result_df <- data.frame(variable=character(), p_value = numeric(),
                        is_independent=logical())

# 각 범주형 변수에 대해 독립성 검증을 수행한다.
for (var in categorical_vars) {
  contingency_table <- table(adult[[var]], adult$income)
  chi_test_result <- chisq.test(contingency_table)
  
  result_df <- rbind(result_df, data.frame(
    variable = var,
    p_value = chi_test_result$p.value,
    is_independent = chi_test_result$p.value > 0.05
  ))
}

print(result_df)

# 카이제곱 검정의 p-value
# 검정 결과의 통계쩍 유의성을 평가하는 데 사용된다. 카이제곱 검정은 관찰된
# 빈도와 기대 빈도 간의 차이가 우연에 의한 것인지 아니면 진짜 차이가 있는
# 것인지를 판단하는 데 사용한다.
# p-vlaue는 H0 가설을 기각할 근거가 있는지를 나타내며, 귀무 가설은 일반적으로
# "두 변수 사이에 연관성이 없다."라는 것이다.
# p-value가 작을수록 H0 가설을 기각하는 데 더 많은 증거가 있다.

## 연속형 변수 - 연속형 변수간 관계
# 상관계수
num_columns <- which(sapply(adult, is.numeric))
cor_matrix <- cor(adult[,num_columns])
cor_matrix

# 필요한 라이브러리 불러오기
library(ggplot2)
install.packages("reshape2")
library(reshape2)

# 데이터를 long format으로 반환하기
cor_melted <- melt(cor_matrix)
cor_melted

# 히트맵 그리기
ggplot(data = cor_melted
       , aes(x=Var1, y=Var2)) +
  geom_tile(aes(fill=value)
          , color="white") +
  geom_text(aes(label=round(value,2)), size=3) +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint = 0
                       , limit=c(-1,1), name="Correlation") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
