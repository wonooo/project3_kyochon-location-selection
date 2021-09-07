stat <- read.csv('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/인구통계/인구통계데이터.csv')
stat <- stat[-1]
str(stat)

dong <- c()
for(i in 1:nrow(stat)){
  dong <-  append(dong,strsplit(stat$동, '_')[[i]][2])
}
stat$dong <- dong
stat[stat$dong=='신사동','dong'] <- c('신사동2', '신사동3')    # 중복되는 동의 이름에 순서를 넣음
stat[stat$dong=='신사동2',] 
stat[stat$dong=='신사동3',] 


# 각 지점의 2KM이내를 배달가능 지역으로 정함
# 참고로, 가능 지역이란 서울시의 동을 기준으로 하였고, 동을 나타내는 지점은 동사무소로 결정
region <- read.csv('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/지점별_배달_가능_지역2.csv')
str(region)
strsplit(region$배달가능, ', ')
region$store_name

dd <- data.frame(rbind((apply(stat[dong%in%strsplit(region$배달가능, ', ')[[1]],c(-1, -15)], 2, mean))))
dd
for(i in 2:nrow(region)){
  dd <- rbind(dd,apply(stat[dong%in%strsplit(region$배달가능, ', ')[[i]],c(-1, -15)], 2, mean, na.rm=T))
}
View(dd)

dataaa <- cbind(store_name=region$store_name, dd)
str(dataaa)


# 경쟁업체수
distance <- read.csv('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/1,2KM이내 경쟁업체 수.csv')
distance
data_chicken <- merge(dataaa, distance, by='store_name', all=T)
str(data_chicken)
sum(is.na(data_chicken))
View(data_chicken)

# 최근접 지하철역
subway <- read.csv('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/교촌치킨_최근접_지하철역_5개_거리.csv')
subway
summary(subway)
subway <- subway[c(1,2,7)]
data_chicken <- merge(data_chicken, subway, by='store_name', all=T)
str(data_chicken)

# 임대료, 소득,소비 추가
plus <- read.csv('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/교촌치킨_회귀.csv')
str(plus)
plus[c(1,3:5, 19)]
data_chicken <- merge(data_chicken, plus[c(1,3:5, 19)], by='store_name', all=T)
str(data_chicken)


# 처음에 종속변수로 설정했던 '요기요'에서 수집한 각 지점별 평균매출액(리뷰의 메뉴를 하나씩 가격으로 환산함)

# 종속변수 추가
result <- read_excel('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/교촌_매출.xlsx')
str(result)

# 종속변수 합치기
data_chick <- merge(data_chicken, result[c(1,4)], by='store_name', all =T)
str(data_chick)
sum(is.na(data_chick))

str(data_chick)
data_chick[is.na(data_chick$주평균매출),]

data_chick[data_chick$store_name =='가양','주평균매출'] <- data_chick %>% dplyr::select(store_name, 주평균매출) %>% 
  filter(store_name %in% c('등촌', '마곡1호', '강서구청', '발산역')) %>% 
  summarise(n=mean(주평균매출))
data_chick[data_chick$store_name =='상도','주평균매출'] <- data_chick %>% dplyr::select(store_name, 주평균매출) %>% 
  filter(store_name %in% c('상도터널', '노량진', '신대방삼거리', '흑석1호', '신길2호')) %>% 
  summarise(n=mean(주평균매출))
data_chick[data_chick$store_name =='신월2호','주평균매출'] <- data_chick %>% dplyr::select(store_name, 주평균매출) %>% 
  filter(store_name %in% c('신월1호', '까치산역', '신트리')) %>% 
  summarise(n=mean(주평균매출))
data_chick[data_chick$store_name =='양재1호','주평균매출'] <- data_chick %>% dplyr::select(store_name, 주평균매출) %>% 
  filter(store_name %in% c('개포1호', '우면', '도곡')) %>% 
  summarise(n=mean(주평균매출))
str(data_chick)

# 최종 데이터셋 
ggcorr(data_chick, label = TRUE, label_round = 1) #상관계수

str(data_chick)
write.csv(data_chick, 'C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/data_chicken_구축.csv')

