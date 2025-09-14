#모평균 차이에 대한 가설검정(울산 지역이 다른 지역들에 비해서 통계적으로 유의한가)
data = c(76.2, 77.8, 73.8, 80.2, 71.8, 78.6, 84.0, 82.6, 72.4, 70.0, 73.0, 69.4, 66.4, 66.9, 72.0, 78.7, 83.1)

# 최고 값과 나머지 값으로 분리
highest_value = max(data)
other_values = data[data != highest_value]

# 정규성 검정 (Shapiro-Wilk 테스트), 셰피로 검정의 경우 표본의 수가 작은 경우에 유리한 검정
shapiro_test = shapiro.test(other_values)

# 평균과 표준편차 계산
mean_others = mean(other_values)
std_others = sd(other_values)

# t-검정 수행 (단일 표본 t-검정)
t_test = t.test(other_values, mu = highest_value)

# 결과 출력
shapiro_test
t_test

#혹시 몰라서 히스토그램 그려보기
hist(data,breaks=5, probability = TRUE, col = "lightblue", xlab = "스포츠 참여 + 취미오락", main = "여가율 hist")
lines(density(data), col = "red", lwd = 2)
rug(data)

#비율검정정
proportion_observed <- 84.0 / 100  # 84.0%를 비율로 변환
n <- length(data)  # 총 시도 수

# 전체 비율 (모집단 비율을 대체할 수 있는 평균 비율로 가정)
proportion_mean <- mean(data) / 100  # 평균 비율로 가정

# 비율 검정
prop_test <- prop.test(x = sum(data >= 84.0), n = n, p = proportion_mean)

# 결과 출력
prop_test

