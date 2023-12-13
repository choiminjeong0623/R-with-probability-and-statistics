## 연속형 변수의 관계
library("MASS")
x <- Boston$tax
y <- Boston$medv

# 피어슨 상관계수
cor_val <- cor(x, y, method = "pearson")
cor_val

## 스피어만 상관계수
cor_val <- cor(x, y, method = "spearman")
cor_val

options("scipen" = 100)
# 피어슨 상관계수와 테스트하기
cor_test_result <- cor.test(x, y, method = "pearson")
cor_test_result

cor_test_result$estimate
cor_test_result$p.value

# 스피어만 상관계수와 테스트
cor_test_result <- cor.test(x, y, method = "spearman")

