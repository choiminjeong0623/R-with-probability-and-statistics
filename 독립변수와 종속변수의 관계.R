# 독립변수와 종속변수의 관계
hf <- read.csv("https://raw.githubusercontent.com/data-8/materials-fa17/master/lec/galton.csv", header=T, stringsAsFactors = FALSE)
hf$gender <- factor(hf$gender, levels=c("male","female"))
hf.son <- subset(hf, gender=="male")
hf.son <- hf.son[c("father", "childHeight")]

lm(childHeight ~ father, data = hf.son)

out <- lm(childHeight ~ father, data = hf.son)
anova(out)

summary(out)

no <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(out)
par(no)

# 평균반응구간추정
# par() : 생성된 그래프의 그래픽 파라미터를 설정하거나
# 수정하는 데 사용된다.
no <- par(no.readonly = TRUE)
plot(childHeight ~ father, data=hf.son, main=""
     , xlab="아버지의 키(인치)", ylab="아들의 키(인치)"
     , ylim=c(65,75))
abline(out, lwd=1.5)

# predict() : 통계 모형에서 예측을 생성하는 데 사용.
# 주로 회귀 분석, 분류 모형 등과 같은 다양한 통계 모형에서 사용
ci <- predict(out, interval="confidence")
prd <- predict(out, interval="predict")
lines(hf.son$father, ci[,2], lty=3, lwd=1.5, col="red")
lines(hf.son$father, ci[,3], lty=3, lwd=1.5, col="red")
par(no)

# 데이터 생성하기
x <- Boston$tax
y <- Boston$medv

data <- data.frame(x,y)
data

# 선형 회귀 모델
model <- lm(y~x, data=data)
model

# 잔차 추출하기
residuals <- resid(model)

# Normal Q-Q plot
qqnorm(residuals)
qqline(residuals)
