## 범주형 변수의 관계
# 1. 적합도 검증하기
x <- c(315, 101, 108, 32)
chisq.test(x, p=c(9,3,3,1) / 16)

# 2. 동질성 검증하기
sns.c <- read.csv("./resource/snsbyage.csv", header=T,
                  stringsAsFactors = FALSE, 
                  encoding = 'utf-8')
str(sns.c)

sns.c <- transform(sns.c, age.c = factor(age, levels=c(1,2,3),
                                         labels=c("20대","30대","40대")))
sns.c <- transform(sns.c, service.c = 
                     factor(service, levels=c("F", "T", "K", "C", "E"), 
                            ordered=TRUE))
c.tab <- table(sns.c$age.c , sns.c$service.c)
c.tab

(a.n <- margin.table(c.tab, margin=1))
(s.n <- margin.table(c.tab, margin=2))
(s.p <- s.n/margin.table(c.tab))
(expected <- a.n %*% t(s.p))

(o.e <- c.tab - expected)
(t.t <- sum((o.e)^2 / expected))

qchisq(0.95, df=8)

1-pchisq(t.t, df=8)

chisq.test(c.tab) 
addmargins(chisq.test(c.tab)$expected)

ct.info <- chisq.test(c.tab) 
names(ct.info)
ct.info$residuals

# (다변량 범주형 자료) 독립성 검정
# 어느 대학원의 입학과정 중 여성에 대한 성차별 있었다는 소송발생
# 과연 성별이 합격에 영향을 미쳤을까?(성별에 따른 합격여부)

# 문제상황 : 과연 성별이 합격에 영향을 미쳤을까?
# H0 : 성별과 합격 여부는 관련이 없다.(서로 독립이다.)
# H1 : 성별과 합격 여부는 관련이 있다.(서로 연관이 있다.)

# H0이 참이라는 가정 하에 기대도수 = 711.54

# 독립성 검증하기
data("UCBAdmissions")
ucba.tab <- apply(UCBAdmissions, c(1,2), sum)
round(prop.table(ucba.tab, margin=2) * 100, 1)

(a.n <- margin.table(ucba.tab, margin=1))
(g.n <- margin.table(ucba.tab, margin=2))

(a.p <- a.n / margin.table(ucba.tab))
(g.p <- g.n / margin.table(ucba.tab))

(expected <- margin.table(ucba.tab) * (a.p %*% t(g.p)))
addmargins(expected)

# chi-square statistic
o.e <- (ucba.tab - expected) ^2 / expected
addmargins(o.e)

chisq.t <- sum(o.e)
chisq.t
1-pchisq(112.250, df=1)
1-pchisq(chisq.t, df=1)

chisq.test(ucba.tab, correct=TRUE)

# continuity correlation
o.e2 <- (abs(ucba.tab - expected)-0.5)^2 / expected
sum(o.e2)

chisq.test(ucba.tab, correct=FALSE)
