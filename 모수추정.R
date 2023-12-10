## R을 이용하여 모평균과 표본오차 추정하기
# 모평균에 대한 추정치와 표준오차
# 주어진 데이터
trees <- c(2.6, 1.9, 1.8, 1.6, 1.4, 2.2, 1.2, 1.6, 1.6, 1.5,
           1.4, 1.6, 2.3, 1.5, 1.1, 1.6, 2.0, 1.5, 1.7, 1.5,
           1.6, 2.1, 2.8, 1.0, 1.2, 1.2, 1.8, 1.7, 0.8, 1.5,
           2.0, 2.2, 1.5, 1.6, 2.2, 2.1, 3.1, 1.7, 1.7, 1.2)
sample_mean <- mean(trees)
sample_sd <- sd(trees)
se <- sample_mean / sqrt(length(trees))
sample_mean
se

# R을 이용하여 모평균 구간추정
# 주어진 값 설정하기
sample_mean <- 42.7
sigma <- 8
n <- 25

# 총 95% 신뢰구간에 대한 z값 계산하기(표준정규분포의 97.5 백분위수)
z <- qnorm(0.975)
z

# 신뢰구간 계산하기
margin_of_error <- z * (sigma/sqrt(n))
confidence_interval <- c(sample_mean - margin_of_error,
                         sample_mean + margin_of_error)
confidence_interval

## 예제1.
# 식품의약품안전청에서는 어떤 생수의 단위량당 세균의 수치를 조사하고자 한다.
# 임의로 선택한 10개의 생수병을 검사한 결과 각 생수병에 대한 단위량당 세균의
# 수는 다음과 같았다. 각 생수병의 단위량당 세균의 수는 정규분포를 따른다고 가정했을 때,
# 해당 생수의 단위량당 평균 세균 수에 대한 95% 신뢰구간을 구하라.

# 주어진 데이터
samples <- c(175, 190, 215, 198, 184, 207, 210, 193, 196, 180)

# 표본평균과 표본표준편차 계산하기
sample_mean <- mean(samples)
sample_sd <- sd(samples)
n <- length(samples)

# 95% 신뢰구간에 대한 t값 계산하기
# 자유도 n-1의 t-분포의 97.5 백분위수
t_val <- qt(0.975, df=n-1)

# 신뢰구간 계산하기
margin_of_error <- t_val * (sample_sd / sqrt(n))
confidence_interval <- c(sample_mean - margin_of_error, sample_mean + margin_of_error)
confidence_interval


## 예제2.
# 어느 공정과정에서 나오는 제품 중 100개를 임의로 추출하여 조사한 결과 
# 6개가 불량품이었다. 이 공정과정에서 나오는 제품 전체의 불량률에 대한
# 95% 신뢰구간을 구하여라.

# 주어진 데이터
n <- 100
defective <- 6

# 표본 불량률 계산하기
p_hat <- defective / n

# 95% 신뢰구간에 대한 z값 계산하기(표준정규분포의 97.5 백분위수)
z <- qnorm(0.975)

margin_of_error <- z*sqrt(p_hat * (1-p_hat)/n)
confidence_interval <- c(p_hat - margin_of_error, p_hat + margin_of_error)
confidence_interval
