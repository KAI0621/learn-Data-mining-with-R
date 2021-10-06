## install packages
install.packages('DMwR2')
install.packages('car')

## 2.3 数据加载到R
library(DMwR2)
head(algae)

## 2.4 数据可视化和摘要

summary(algae)   
hist(algae$mxPH,pro=T)

library(DMwR2)
library(car)
par(mfrow=c(1,2))     ##绘制一行两列图形
hist(algae$mxPH,prob=T,xlab='',main = "Histogram of maximum pH Value",ylim=0:1)  ##直方图绘制
lines(density(algae$mxPH,na.rm = T))   ##绘制趋势线
rug(jitter(algae$mxPH))            ##绘制分布密度线
qqPlot(algae$mxPH,main="Normal QQ plot of maximum pH")  ##绘制qq plot
par(mfrow=c(1,1))     ##设置为一行一列图形

boxplot(algae$oPO4,ylab="Orthophosphate(oPO4)")  ##绘制箱型图
rug(jitter(algae$oPO4),side = 2)  ##绘制分布密度线
abline(h= mean(algae$oPO4,na.rm = T),lty=2) ##均值处绘制虚线

plot(algae$NH4, xlab="")  ##绘制散点图
abline(h=mean(algae$NH4,na.rm= T),lty=1) ##均值处绘制实线
abline(h=mean(algae$NH4,na.rm = T)+sd(algae$NH4,na.rm = T),lty=2) ## 增加一条标准差线，虚线
abline(h=median(algae$NH4,na.rm=T),lty=3) ##增加中位数线
identify(algae$NH4) ##交互式命令

plot(algae$NH4,xlab="") ##绘制散点图
clicked.lines<- identify(algae$NH4) ##交互式命令，点击点
algae[clicked.lines,] ##显示点击点详细数据

algae[algae$NH4 > 19000,] ##显示大于19000的点的数据

library(lattice)
bwplot(size ~ a1,data = algae,ylab="River size",xlab="Algal A1") ## 按照Size的类型显示a1数据的分布

install.packages("Hmisc")
library(Hmisc)
bwplot(size~a1,data=algae,panel = panel.bpplot,probs=seq(.01,.49,by=.01),datadensity=T,ylab = 'River Size',xlab="Algal A1")  ## 按照Size的类型显示a1数据的分布,包括密度和分布

minO2<-equal.count(na.omit(algae$mnO2),number=4,overlap=1/5)  ##equal.count 对连续变量mnO2离散化，na.omit 去除NA值
stripplot(season~a3|minO2,data = algae[!is.na(algae$mnO2),])  ## 去除NA的函数

## 2.5 数据缺失

library(DMwR2)
data(algae) ##读取数据

algae[!complete.cases(algae),]  ##显示缺失的数据
nrow(algae[!complete.cases(algae),])   ##显示缺失数据的数量

algae<- na.omit(algae)  ##剔除缺失的数据

algae<-algae[-c(62,199),] ##剔除62和199行的数据
apply(algae,1,function(x) sum(is.na(x)))  

manyNAs(algae,0.2)   ##找出缺失值大于20%的行数
algae<-algae[-manyNAs(algae),]   ##去除含有缺失值较多的行

algae[48,"mxPH"] ##显示48行mxPH值
algae[48,"mxPH"]<- mean(algae$mxPH,na.rm = T) ##使用平均数补充48行mxPH值,na.rm = T计算时忽略Na值

algae[is.na(algae$Chla),"Chla"]<-median(algae$Chla,na.rm=T) ##使用中位数补充所有Chla缺失数值

algae<-algae[-manyNAs(algae),]
algae<-centralImputation(algae) ##对于名义变量采用众数，对于数值变量采用中位数

cor(algae[,4:18],use = "complete.obs") ##计算4-18列的相关性，方法忽略NA值
symnum(cor(algae[,4:18],use = "complete.obs")) ##使用符号来显示相关性

data(algae)
algae<-algae[-manyNAs(algae),]
lm(PO4~oPO4,data = algae)  ##计算两者之间的线性相关性
algae[28,"PO4"]<-42.897+1.293*algae[28,"oPO4"] ##利用相关性补足样本28上PO4的缺失数值

data(algae)
algae<-algae[-manyNAs(algae),]
fillPO4<-function(oP){
  if(is.na(oP))
    return(NA)
  else return(42.897+1.293*oP)} ##定义函数fillPO4
algae[is.na(algae$PO4),"PO4"]<-sapply(algae[is.na(algae$PO4),"oPO4"],fillPO4) ##sapply函数应用

histogram(~mxPH|season,data=algae)  ##不同季节下的mxPH值

algae$season<-factor(algae$season,levels = c("spring","summer","autumn","winter")) ##按照季节排序显示

histogram(~mxPH|size*speed,data = algae)  ##不同size和speed下mxPH的直方图

stripplot(size~mxPH|speed,data = algae,jitter=T)  ##不同size和speed下mxPH的散点分布图

data(algae)
algae<-algae[-manyNAs(algae),]

## 2.6 获取预测模型
data(algae)
algae<-algae[-manyNAs(algae),]
clean.algae<-knnImputation(algae,k=10)

lm.a1<-lm(a1~.,data=clean.algae[,1:12]) ##通过其他数据（12个数据，也可单独使用或组合使用数据）来预测a1的数值
summary(lm.a1)
plot(lm.a1) ##预测模型的线性图
anova(lm.a1)  ##对建立的模型进行方差分析
lm2.a1<-update(lm.a1,.~.-season) ##去除seaon的变量进行新的模型制作
summary(lm2.a1)
anova(lm.a1,lm2.a1) ##比较两个模型有无显著差异
final.lm<-step(lm.a1) ##使用向后消元法得到新的线性模型
summary(final.lm)

library(rpart)
data(algae)
algae<-algae[-manyNAs(algae),]
rt.a1<-rpart(a1~.,data = algae[,1:12]) ##通过其他数据（12个数据，也可单独使用或组合使用数据）建立回归树
rt.a1
summary(rt.a1)
printcp(rt.a1)  ##复杂度损失修剪

rt2.a1<-prune(rt.a1,cp=0.08)  ##使用不同的cp值来建树
rt2.a1
(rt.a1<-rpartXse(a1~.,data=algae[,1:12]))  ##rpartXse函数来进行修建
first.tree<-rpart(a1~.,data=algae[,1:12])  ##指出需要修建的节点
snip.rpart(first.tree,c(4,7)) ##修剪回归树

## 2.7 模型的评价和选择
lm.predictions.a1<-predict(final.lm,clean.algae) ##使用final.lm模型的预测值
rt.predictions.a1<-predict(rt.a1,algae) ##使用rt.a1模型的预测值

(mae.a1.lm<-mean(abs(lm.predictions.a1-algae$a1))) #
(mae.a1.rt<-mean(abs(rt.predictions.a1 - algae[,"a1"]))) #计算预测模型的平均绝对误差

(mse.a1.lm<-mean(lm.predictions-algae[,"a1"])) #
(mse.a1.rt<-mean(rt.predictions-algae[,"a1"])) #计算预测模型的均方误差

(nmse.a1.lm<-mean((lm.predictions.a1-algae[,'a1'])^2)/mean((mean(algae[,'a1'])-algae[,'a1'])^2))
(nmse.a1.rt<-mean((rt.predictions.a1-algae[,'a1'])^2)/mean((mean(algae[,'a1'])-algae[,'a1'])^2)) #计算预测模型的平均绝对误差

regr.eval(algae[,"a1"],rt.predictions.a1,train.y=algae[,"a1"]) ##内置函数计算各种预测值

old.par<-par(mfrow=c(1,2))
plot(lm.predictions.a1,algae[,"a1"],main="Linear Model",xlab="Predictions",ylab="True Values")
abline(0,1,lty=2)
plot(rt.predictions.a1,algae[,"a1"],main="Regression Model",xlab="Predictions",ylab="True Values")
abline(0,1,lty=2)
par(old.par)

plot(lm.predictions.a1,algae$a1,main="Linear Model",xlab="Predictions",ylab="True Values") ##预测和真实图形化显示
abline(0,1,lty=2)
algae[identify(lm.predictions.a1,algae$a1)] ##交互显示点
sensible.lm.predictions.a1<-ifelse(lm.predictions.a1 < 0,0,lm.predictions.a1) ##ifelse来改进模型预测结果
regr.eval(algae$a1,lm.predictions.a1,stats=c("mae","mse"))

cv.rpart<-function(form,train,test,...){
  m<-rpartXse(form,train,...)
  p<-predict(m,test)
  mse<-mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}

cv.lm<-function(form,train,test,...){
  m<-lm(form,train,...)
  p<-predict(m,test)
  p<-ifelse(p<0,0,p)
  mse<-mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}

res <- experimentalComparison(
  c(dataset(a1~.,clean.algae[,1:12],'a1')),
  c(variants('cv.lm'),
   variants('cv.rpart',se=c(0,0.5,1))),
  cvSettings(3,10,1234))

