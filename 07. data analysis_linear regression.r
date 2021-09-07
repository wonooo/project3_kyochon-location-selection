data_배민 <- read.csv('C:/Users/wjddk/Desktop/학원/팀프로젝트/이후/data_배민_변환끝.csv')
str(data_배민)
data_배민 <- data_배민[-1]

# 1.예측데이터 나누기
set.seed(1234)
idx = sample(2, nrow(data_배민), replace = T, prob=c(0.8,0.2))

chicken_train = data_배민[idx==1,2:15]
chicken_train_label = data_배민[idx==1,1]
chicken_test = data_배민[idx==1,2:15]
chicken_test_label = data_배민[idx==2,1]

chicken_train[1,]

#---------------------- 분석

# 2.회귀모형 적합
fit_1=lm(평균리뷰수~1,chicken_train)  #절편
summary(fit_1)
fit_full=lm(평균리뷰수~.,chicken_train) #전체
summary(fit_full)


# 3.모형선택 
# 3_1.aic에 의한 단계별 선택& 적합 결과 확인
fit1=MASS::stepAIC(fit_1, scope=formula(fit_full), trace = F)  #전진선택
fit2=MASS::stepAIC(fit_full, trace = F)  #후진소거
summary(fit1)
summary(fit2)

# 3_2.bic에 의한 단계별 선택&적합 결과 확인
fit3=MASS::stepAIC(fit_1, scope=formula(fit_full), k=log(nrow(chicken_train)), trace = F)
fit4=MASS::stepAIC(fit_full, k=log(nrow(chicken_train)), trace = F)
summary(fit3)
summary(fit4)

# 3_3.모든 가능한 회귀
library(leaps)
fits=regsubsets(평균리뷰수~.,chicken_train)
summary(fits)
plot(fits,scale = "adjr2")
plot(fits,scale = "Cp")
plot(fits)   # 디폴트는 BIC


# 모형 선택
BIC(fit1,fit2,fit3,fit4)
AIC(fit1,fit2,fit3,fit4)

# 최종모형 
summary(fit1)


# 4.모형진단
# 가정확인(오차항의 동분산성, 정규성, 독립성, 선형성)
# 잔차산점도, 정규성, 동분산성, 이상값
par(mfrow=c(2,2))
plot(fit1)         
par(mfrow=c(1,1))

plot(fit5, which = 2)      # 곡선일경우 회귀모형을 2차항을 넣어서 다시 적합시키고 다시 모형진단
plot(fit5, which = 4)      # 이상값 존재 확인해서 필요하다면 제거할지 생각


#5.예측
pred=predict(fit1,newdata=chicken_test)
caret::defaultSummary(data.frame(obs=chicken_test$평균리뷰수,pred=pred))

ggplot(data.frame(obs=chicken_test$평균리뷰수, pred=pred)) + 
  geom_point(aes(x=obs, y=pred)) + geom_abline(intercept = 0, slope = 1)

pred=predict(fit1,newdata=chicken_train[1,])
pred


chicken_train[1,]

fit1$fitted.values
fit1$xlevels
fit1$model
