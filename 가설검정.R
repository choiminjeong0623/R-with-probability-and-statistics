# 가설검정
# 단일모집단 평균을 모르는 경우
data <- read.table("https://raw.githubusercontent.com/thomas-haslwanter/statsintro_python/master/ISP/Exercise_Solutions/babyboom.dat.txt", header=F)
str( data )
names(data) <- c("time", "gender","weight","minutes")
tmp <- subset(data, gender==1)
tmp
weight <- tmp[[3]]
class(weight)
barx <- mean(weight)
s <- sd(weight)
n <- length(weight)
h0 <- 2800
t.t <- (barx-h0)/(s/sqrt(n))

alpha <- 0.05
(c.u <- qt(1-alpha, df=n-1))
(p.value <- 1-pt(t.t, df=n-1))

t.test(weight, mu=2800, alternative = "greater")

# 도표 작성하기
par(mar=c(0,1,1,1))
x <- seq(-3,3,by=0.001)
y <- dt(x, df=n-1)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), main="", xlab="t", ylab="")
abline(h=0)

polygon(c(t.t, x[x>t.t],3),c(0,y[x>t.t],0), density=20, angle=45)
text(t.t, -0.02, paste("t=", round(t.t,3)), pos=4)
text(2.65, 0.1, expression(plain(P)(T>2.233)== 0.0196), cex=0.8)
arrows(2.7, 0.08, 2.5, 0.03, length=0.05)
