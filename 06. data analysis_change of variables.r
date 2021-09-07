# 분석을 위해 선정된 독립변수와 새로 바뀐 종속변수를 합치는 작업
# 그 후, 회귀분석의 조건에 맞게 독립성을 만들기 위해 변환을 하는 작업을 실시

data_final <- read.csv('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/data_최종_끝.csv')
str(data_final)
data_final <- data_final[-1]

result <- read.csv('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/교촌치킨서울_배민리뷰수.csv')
str(result)
result <- result[c(1,9)]
sum(is.na(result$평균리뷰수))

library(dplyr)
result[result$store_name =='가양','평균리뷰수'] <- result %>% dplyr::select(store_name, 평균리뷰수) %>% 
  filter(store_name %in% c('등촌', '마곡1호', '강서구청', '발산역')) %>% 
  summarise(n=round(mean(평균리뷰수),2))

result[result$store_name =='동소문','평균리뷰수'] <- result %>% dplyr::select(store_name, 평균리뷰수) %>% 
  filter(store_name %in% c('보문역', '정릉2호', '종암', '안암', '정릉1호', '대학로')) %>% 
  summarise(n=round(mean(평균리뷰수),2))

result[result$store_name =='문래','평균리뷰수'] <- result %>% dplyr::select(store_name, 평균리뷰수) %>% 
  filter(store_name %in% c('오목교', '당산', '영등포역', '신도림1호', '신길1호')) %>% 
  summarise(n=round(mean(평균리뷰수),2))

result[result$store_name =='신림2호','평균리뷰수'] <- result %>% dplyr::select(store_name, 평균리뷰수) %>% 
  filter(store_name %in% c('신림1호', '독산1호', '신대방', '구로디지털')) %>% 
  summarise(n=round(mean(평균리뷰수),2))

result[result$store_name =='신월2호','평균리뷰수'] <- result %>% dplyr::select(store_name, 평균리뷰수) %>% 
  filter(store_name %in% c('신월1호', '까치산역', '신트리')) %>% 
  summarise(n=mean(평균리뷰수, na.rm=T))

result[result$store_name =='신트리','평균리뷰수'] <- result %>% dplyr::select(store_name, 평균리뷰수) %>% 
  filter(store_name %in% c('신정1호', '신월1호', '동양미래대학', '고척1호', '오목교', '목동오거리', '신월2호')) %>% 
  summarise(n=round(mean(평균리뷰수),2))


View(merge(data_final, result, by='store_name', all=T))
sum(is.na(merge(data_final, result, by='store_name', all=T)))



# 데이터 완성
data_배민 <- merge(data_final, result, by='store_name', all=T)
str(data_배민)
data_배민 <- data_배민[-16]

library(GGally)
ggpairs(data_배민[-1])

str(data_배민)
write.csv(data_배민, 'C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/data_배민.csv')


#------------------------------------------------
data_배민 <- read.csv('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/data_배민.csv')
str(data_배민)
data_배민 <- data_배민[-1]

ggplot(data_배민, aes(x=1:214, y=평균리뷰수)) + geom_point(size=2)
data_배민 %>% filter(평균리뷰수>1000)
data_배민 %>% filter(평균리뷰수<20)
ggplot(data_배민) + geom_density(aes(x=평균리뷰수, fill='평균리뷰수'))
ggplot(data_배민) + geom_density(aes(x=log(평균리뷰수), fill='평균리뷰수'))
ggplot(data_배민) + geom_density(aes(x=1/(평균리뷰수), fill='평균리뷰수'))
ggplot(data_배민) + geom_density(aes(x=sqrt(평균리뷰수), fill='평균리뷰수'))
ggplot(data_배민) + geom_density(aes(x=(평균리뷰수)^2, fill='평균리뷰수'))

# 정규성검정
options("scipen" = 100)
shapiro.test(data_배민$평균리뷰수)
shapiro.test(log(data_배민$평균리뷰수))

# 소득과 주평균매출
# age와 charge와의 관계
ggplot(data_배민, aes(x=소득, y=평균리뷰수)) + geom_point(size=2)
ggplot(data_배민, aes(x=소득)) + geom_histogram(bins = 10)

library(dplyr)
data_배민 <- data_배민 %>% mutate(소득_cut=factor(ifelse(소득<3500000, 0, 1))) %>% dplyr::select(-소득)
str(data_배민)
ggplot(data_배민, aes(x=소득_cut)) + geom_bar()


# 변수 변환한 값으로 변경
data_배민 <- data_배민 %>% mutate(사업체수=log(사업체수), 인구밀도=(인구밀도^2), 임대료=(1/임대료),
                                        최근접지하철거리=log(최근접지하철거리), 평균리뷰수=log(평균리뷰수),
                                        지하철수_1키로=factor(지하철수_1키로))
data_배민 <- data_배민 %>% dplyr::select(-내국인비율)
data_배민 <- data_배민 %>% dplyr::select(1:13,15,14)
str(data_배민)

# 변환 끝
ggpairs(data_배민[-1])

write.csv(data_배민, 'C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/data_배민_변환끝.csv')

