#####匯入資料#####
library(data.table)
library(rpart)
train_0<-as.data.frame(fread('C:/Users/cai/Desktop/國泰競賽/train(v1).csv'))
test_0<-as.data.frame(fread('C:/Users/cai/Desktop/國泰競賽/test(v1).csv'))
train = rbind(train_0[,1:131],test_0) #將train以及test合併做缺失值補值
summary(train)

#####EDA及統計檢定方法#####
pairwise.t.test(train_0$Y1,train_0$CHARGE_CITY_CD ,p.adjust.method = "bonferroni" )
wilcox.test(train_0$CHANNEL_B_POL_CNT,train_0$Y1)
train_0$L1YR_B_ISSUE_CNT<-ifelse(train_0$L1YR_B_ISSUE_CNT==0,0,1)
chisq.test(train_0$L1YR_B_ISSUE_CNT,train_0$Y1)

#####資料欄位前處理#####

#L1YR_C_CNT
train = train[,-which(colnames(train) == 'L1YR_C_CNT')]

#X_IND
sum_ABC_IND = apply(train[,which(colnames(train) %in% c('A_IND','B_IND','C_IND'))],1,sum)
sum_ABC_IND = ifelse(sum_ABC_IND>0,1,0)
sum_ABC_IND[which(is.na(sum_ABC_IND))] = 2
train[,'A_IND'] = sum_ABC_IND; colnames(train)[which(colnames(train) == 'A_IND')] = 'ABC_IND'
train = train[,-which(colnames(train)%in%c('B_IND','C_IND'))]

#FINANCETOOLS
FINANCETOOLS_NA = numeric(nrow(train))
FINANCETOOLS_NA[which(is.na(train$FINANCETOOLS_A))] = 1
ind = which(colnames(train) == 'FINANCETOOLS_G')
if(ind==ncol(train)){
  train = cbind(train[,1:ind],FINANCETOOLS_NA)
}else{
  train = cbind(train[,1:ind],FINANCETOOLS_NA,train[(ind+1):ncol(train)])
}

train[which(is.na(train$FINANCETOOLS_A)),which(colnames(train) %in% paste0('FINANCETOOLS_',c('A','B','C','D','E','F','G')))] = 0

#IF_ADD_INSD_X_IND
train = train[,-which(colnames(train)%in%paste0('IF_ADD_INSD_',c('F','L','Q','G','R'),'_IND'))]

#刪除DIEACCIDENT_AMT
train = train[,-which(colnames(train) %in%c('DIEACCIDENT_AMT'))]

#IF_ISSUE_INSD_X_IND
IF_ISSUE_INSD_IND_NA = numeric(nrow(train))
IF_ISSUE_INSD_IND_NA[which(is.na(train$IF_ISSUE_INSD_A_IND))] = 1
ind = which(colnames(train) == 'IF_ISSUE_INSD_Q_IND')
train = cbind(train[,1:ind],IF_ISSUE_INSD_IND_NA,train[(ind+1):ncol(train)])
train[which(is.na(train$IF_ISSUE_INSD_Q_IND)),which(colnames(train) %in% paste0('IF_ISSUE_INSD_',c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q'),'_IND'))] = 0

data_x2<-train
dim(data_x2)
