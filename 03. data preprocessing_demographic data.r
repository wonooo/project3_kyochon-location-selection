library(readxl)
library(dplyr)

# 가구원수별 가구수
data1 <- read_excel('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/data.xlsx', sheet = '가구원수별  가구수')
data1
colnames(data1) <- c('자치구', '동', '총계', '가구_1인', '가구_2인', '가구_3인', '가구_4인', '가구_5인', '가구_6인', '가구_7인이상')
data1 <- data1[c(-1:-3),]
data1 <- data1 %>% filter(동!='소계')
str(data1)

data1 <- data1 %>% mutate(총계 = as.numeric(총계), 가구_1인 = as.numeric(가구_1인), 가구_2인 = as.numeric(가구_2인), 가구_3인 = as.numeric(가구_3인),
                    가구_4인 = as.numeric(가구_4인), 가구_5인 = as.numeric(가구_5인), 가구_6인 = as.numeric(가구_6인), 가구_7인이상 = as.numeric(가구_7인이상))
data1 <- data1 %>% mutate(동=paste0(data1$자치구, '_', data1$동)) %>% select(-1,-3)
str(data1)

# 사업체수
data2 <- read_excel('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/data.xlsx', sheet = '사업체수')
data2 <- data2[-1,]
data2 <- data2 %>% filter(동!='소계')
str(data2)

data2 <- data2 %>% mutate(동=paste0(data2$자치구, '_', data2$동)) %>% select(-1)
str(data2)

# 인구밀도
data3 <- read_excel('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/data.xlsx', sheet = '인구밀도')
data3 <- data3[-1,]
data3 <- data3 %>% filter(동!='소계')
str(data3)

data3 <- data3 %>% mutate(동=paste0(data3$자치구, '_', data3$동)) %>% select(-1)
str(data3)

# 세대수, 넘여비율, 한국인수, 고령자수
data4 <- read_excel('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/data.xlsx', sheet = '주민등록인구')
data4
colnames(data4) <- c('자치구', '동', '세대', '인구', '남자', '여자', '한국인수', '고령자수')
data4 <- data4[c(-1:-4),]
data4 <- data4 %>% filter(동!='소계')
str(data4)

data4 <- data4 %>% mutate(세대 = as.numeric(세대), 인구 = as.numeric(인구), 남자 = as.numeric(남자), 여자 = as.numeric(여자),
                            한국인수 = as.numeric(한국인수), 고령자수 = as.numeric(고령자수))
data4 <- data4 %>% mutate(동=paste0(data4$자치구, '_', data4$동)) %>% select(-1)
str(data4)

# 출생자수, 사망자수
data5 <- read_excel('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/data.xlsx', sheet = '출생사망')
data5
colnames(data5) <- c('자치구', '동', '출생', '사망')
data5 <- data5[c(-1:-3),]
data5 <- data5 %>% filter(동!='소계')
data5 <- data5 %>% filter(동!='미상')
str(data5)

data5 <- data5 %>% mutate(출생 = as.numeric(출생)) %>% select(-4)
data5 <- data5 %>% mutate(동=paste0(data5$자치구, '_', data5$동)) %>% select(-1)
str(data5)


# 인구밀도 데이터를 최신으로 업데이트 == data3 update
data3$동 == data4$동      # 정보가 동일한지 확인
data3_up <- merge(data3[,c(1,3)], data4[, c('동', '인구')], by ='동')
data3_up <- data3_up %>% mutate(인구밀도 = round(인구/면적, 2)) %>% select(1,4)
str(data3_up)

# 가구수 -> 1인, 2인-3인, 4인이상
data1
str(data1)
data1_up <- data1 %>% mutate(가구_23인 = apply(data1[c(3,4)],1, sum), 
                             가구_4인이상 = apply(data1[c(5:8)],1, sum, na.rm=T)) %>% 
                  select(1,2,9,10)
str(data1_up)
data1_up
boxplot(data1_up[3:6])

data1_up2 <- data1 %>% mutate(가구_4인이상 = apply(data1[c(5:8)],1, sum, na.rm=T)) %>% 
  select(1:4,9)
boxplot(data1_up2[3:7])
str(data1_up2)


# 성비, 고령화
str(data4)
data4_up <- data4 %>% mutate(성비=(남자/여자), 고령자비율=(고령자수/인구), 내국인비율=(한국인수/인구)) %>% 
            select(1:3,8:10)
str(data4_up)


#---------------------------------------------------
# 최종으로 만들어진 데이터 확인
data1_up2
data2
data3_up
data4_up
data5

str(data1_up2)
str(data2)
str(data3_up)    # 1개씩 많음
str(data4_up)    # 1개씩 많음
str(data5)

# 데이터 merge
merge(merge(data1_up2, data2, by='동', all=T), data5, by='동', all=T)
merge(data3_up, data4_up, by='동', all=T)
data_ <- merge(merge(merge(data1_up2, data2, by='동', all=T), data5, by='동', all=T), 
      merge(data3_up, data4_up, by='동', all=T), by='동', all=T)   # 구로구항동

write.csv(data_, 'C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/인구통계데이터.csv')

