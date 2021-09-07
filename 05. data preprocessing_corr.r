# 회귀분석을 위한 독립변수간의 상관관계를 확인하는 작업
# 동일한 정보를 가진 변수를 추리고 분석의 정확도를 높이고자 실시

data_chick <- read.csv('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/data_chicken_구축.csv')
data_chick <- data_chick[-1]
str(data_chick)

# 정규화  - 그래프를 위해 생성
standardzation <- function(arg){
  return ((arg-mean(arg))/(sd(arg)))
}
data_chick_std <- data.frame(lapply(data_chick[-1], standardzation))
str(data_chick_std)


# 상관관계 그래프
ggcorr(data_chick, label = TRUE, label_round = 1)

# 인구통계의 상관관계
ggcorr(data_chick[c(2:5, 8:12)], label = TRUE, label_round = 1) 

# 가구 -> 1인가구만 생존(2)
ggplot(data = data_chick_std) + 
  geom_density(aes(x=가구_1인, fill="가구_1인", alpha=.5)) + 
  geom_density(aes(x=가구_2인, fill="가구_2인", alpha=.5)) + 
  geom_density(aes(x=가구_3인, fill="가구_3인", alpha=.5)) + 
  geom_density(aes(x=가구_4인이상, fill="가구_4인이상", alpha=.5))

# 세대, 출생, 인구 -> 인구(11)
cor(data_chick[c(8,10,11)])
ggplot(data = data_chick_std) + geom_density(aes(x=출생, fill='출생', alpha=.5)) + 
  geom_density(aes(x=인구, fill="인구", alpha=.5)) +
  geom_density(aes(x=세대, fill="세대", alpha=.5))

# 사업체수, 종사자수  -> 사업체수(6)
cor(data_chick[c(6,7)])
ggplot(data = data_chick_std) + geom_density(aes(x=사업체수, fill="사업체수", alpha=.5)) + 
  geom_density(aes(x=종사자수, fill="종사자수", alpha=.5))
boxplot(data_chicken[c(6,7)])
boxplot(data_chicken[c(6)])

# 소득, 소비  -> 소득(2)이지만 일단 남겨둠
cor(data_chick[c(19,20)])
ggplot(data = data_chick_std) + geom_density(aes(x=소득, fill="소득", alpha=.5)) + 
  geom_density(aes(x=소비, fill="소비", alpha=.5))
boxplot(data_chicken[c(19,20)])
ggplot(data = data_chicken) + geom_density(aes(x=소득, fill="소득"))


# 상관관계확인후 제거  -> 최종데이터셋
data_chick <- data_chick[-c(3,4,5,7,8,10,20)]
str(data_chick)
colnames(data_chick)
ggcorr(data_chick, label = TRUE, label_round = 1) #상관계수

write.csv(data_chick, 'C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/data_최종_끝.csv')

