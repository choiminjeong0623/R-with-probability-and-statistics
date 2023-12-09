## 확률 생성하기
n <- 10 # 회원수
p <- 0.3 # 일주일에 5회 이상 운동할 확률
x <- 7

# dbinom 함수를 사용하여 정확히 7명이 성공할 확률을 구함
prob <- dbinom(x, size=n, prob=p)
prob

n <- 10 # 회원 수
p <- 0.3 # 일주일에 5회 이상 운동할 확률
x <- 5 # 성공 횟수

# 이항 분포의 누적 분포 함수의 보완을 이용하여
# 6명 이상이 성공할 확률을 구함
prob <- 1 - pbinom(x, size=n, prob=p)
prob

# 누적 분포 대응 분위수 생성하기
n <- 10 # 회원 수
p <- 0.3 # 일주일에 5회 이상 운동할 확률
cumulative_prob_target <- 0.7 # 목표 누적 확률

# 필요한 최소한의 성공 횟수를 구함
x_needed <- qbinom(cumulative_prob_target, size=n, prob=p)
x_needed

## 기하 분포
dgeom(x, prob, log=FALSE)
pgeom(q, prob, lower.tail = TRUE, log.p = FALSE)
qgeom(p, prob, lower.tail = TRUE, log.p = FALSE)
rgeom(n, prob)
