#####針對非歸一化及神秘轉換的連續型變數#####
#using train+test to standardize & normalize
data_full<-rbind(data_train[,-125],data_test)

#未歸一化的conti. variables
cc2=c('AGE','EDUCATION_CD','L1YR_A_ISSUE_CNT','L1YR_B_ISSUE_CNT','CHANNEL_A_POL_CNT','OCCUPATION_CLASS_CD',
      'APC_CNT','INSD_CNT','INSD_1ST_AGE','RFM_R','APC_1ST_AGE',
      'REBUY_TIMES_CNT','LEVEL','RFM_M_LEVEL','LIFE_CNT','AG_CNT','AG_NOW_CNT',
      'CLC_CUR_NUM','TERMINATION_RATE','TOOL_VISIT_1YEAR_CNT')
data_full[,cc2]<-scale(data_full[,cc2]) #standardized first

d1<-data_full[1:100000,];dd<-data_full[100001:250000,]
#L1YR_A_ISSUE_CNT
d1$L1YR_A_ISSUE_CNT<-ifelse(d1$L1YR_A_ISSUE_CNT==0,0,1)
# CHARGE_CITY_CD
library(FSA)
kruskal.test(train_0$CONTACT_CITY_CD,train_0$Y1)
kruskal.test(train_0$CHARGE_CITY_CD,train_0$Y1)
dunnTest(train_0$Y1,train_0$CHARGE_CITY_CD,method='bonferroni')
CHARGE_CITY_IS_C<-as.data.frame(ifelse(d1$CHARGE_CITY_CD==3,1,0))
colnames(CHARGE_CITY_IS_C)<-'CHARGE_CITY_IS_C'

#####主成分分析(PCA)#####
#是否持有特定商品-IM_IS_A_IND
pca1<-prcomp(formula = ~IM_IS_A_IND+IM_IS_B_IND+IM_IS_C_IND+IM_IS_D_IND+IM_CNT,
             data=d1)
vars1<- (pca1$sdev)^2  
props1 <- vars1 / sum(vars1)    
cumulative.props1 <- cumsum(props1) 
cumulative.props1 
t1<-as.data.frame(pca1$x[,c(1,2)]);colnames(t1)<-c('IM_pca_PC1','IM_pca_PC2')

#目前是否壽險保單持有有效類別_F(附約)
pca2<-prcomp(formula = ~ IF_ADD_F_IND+IF_ADD_L_IND+IF_ADD_Q_IND+
               IF_ADD_R_IND+IF_ADD_G_IND+IF_ADD_IND,data = d1)

vars2 <- (pca2$sdev)^2  
props2 <- vars2 / sum(vars2)    
cumulative.props2 <- cumsum(props2) 
cumulative.props2 
t2<-as.data.frame(pca2$x[,c(1,2,3)])
colnames(t2)<-c('IF_ADD_IND_pca_PC1','IF_ADD_IND_pca_PC2','IF_ADD_IND_pca_PC3')

#當年度保障_意外醫療住院日額保險金
pca3<-prcomp(formula = ~ OUTPATIENT_SURGERY_AMT+DISEASES_HOSPITAL_REC_AMT+
               ACCIDENT_HOSPITAL_REC_AMT+INPATIENT_SURGERY_AMT,data = d1)

vars3 <- (pca3$sdev)^2  
props3 <- vars3 / sum(vars3)    
cumulative.props3 <- cumsum(props3)  
cumulative.props3 
t3<-as.data.frame(pca3$x[,1])
colnames(t3)<-c('Hospital_pca')

a=c('IM_IS_A_IND','IM_IS_B_IND','IM_IS_C_IND','IM_IS_D_IND','IM_CNT')
b=c('IF_ADD_F_IND','IF_ADD_L_IND','IF_ADD_Q_IND','IF_ADD_R_IND','IF_ADD_G_IND','IF_ADD_IND')
c=c('OUTPATIENT_SURGERY_AMT','DISEASES_HOSPITAL_REC_AMT','ACCIDENT_HOSPITAL_REC_AMT',
    'INPATIENT_SURGERY_AMT')
delete_col<-c('CUS_ID','CHARGE_CITY_CD','CONTACT_CITY_CD','L1YR_B_ISSUE_CN','CHANNEL_B_POL_CNT','IF_Y_REAL_IND')
old=c(a,c,b,delete_col)
d1$CHARGE_CITY_CD<-ifelse(d1$CHARGE_CITY_CD==0,0,1)
d2=d1[,-which(colnames(d1)%in%old)]
Y1=data_train[,125]
d2<-cbind(d2,t1,t2,t3,CHARGE_CITY_IS_C,Y1)

#####處理test#####

#L1YR_A_ISSUE_CNT
dd$L1YR_A_ISSUE_CNT<-ifelse(dd$L1YR_A_ISSUE_CNT==0,0,1)
# CHARGE_CITY_CD
CHARGE_CITY_IS_C<-as.data.frame(ifelse(dd$CHARGE_CITY_CD==3,1,0))
colnames(CHARGE_CITY_IS_C)<-'CHARGE_CITY_IS_C'

a=c('IM_IS_A_IND','IM_IS_B_IND','IM_IS_C_IND','IM_IS_D_IND','IM_CNT')
coef_d1<-pca1$rotation[,c(1,2)] 
#coef_d1<-pca1$rotation[,c(1,2,3)] 
tt1<-as.data.frame(as.matrix(dd[,a])%*%as.matrix(coef_d1))
colnames(tt1)<-c('IM_pca_PC1','IM_pca_PC2')
#colnames(tt1)<-c('IM_pca_PC1','IM_pca_PC2','IM_pca_PC3')

b=c('IF_ADD_F_IND','IF_ADD_L_IND','IF_ADD_Q_IND','IF_ADD_R_IND','IF_ADD_G_IND','IF_ADD_IND')
coef_d2<-pca2$rotation[,c(1,2,3)] #only choose first PCA
tt2<-as.data.frame(as.matrix(dd[,b])%*%as.matrix(coef_d2))
colnames(tt2)<-c('IF_ADD_IND_pca_PC1','IF_ADD_IND_pca_PC2','IF_ADD_IND_pca_PC3')

c=c('OUTPATIENT_SURGERY_AMT','DISEASES_HOSPITAL_REC_AMT','ACCIDENT_HOSPITAL_REC_AMT',
    'INPATIENT_SURGERY_AMT')
coef_d3<-pca3$rotation[,1] #only choose first PCA
tt3<-as.data.frame(as.matrix(dd[,c])%*%as.matrix(coef_d3))
colnames(tt3)<-c('Hospital_pca')

delete_col<-c('CUS_ID','CHARGE_CITY_CD','CONTACT_CITY_CD','L1YR_B_ISSUE_CN','CHANNEL_B_POL_CNT','IF_Y_REAL_IND')
old=c(a,c,b,delete_col)
dd$CHARGE_CITY_CD<-ifelse(dd$CHARGE_CITY_CD==0,0,1)
dd1=dd[,-which(colnames(dd)%in%old)]
dd1<-cbind(dd1,tt1,tt2,tt3,CHARGE_CITY_IS_C)

#####對於非有序型的多類別變數做成虛擬變數#####
library(dummies)
library(Matrix)
library(drat)
d3<-d2 #train
d4<-dd1 #test

for(i in c('MARRIAGE_CD','ABC_IND','CUST_9_SEGMENTS_CD')){
  d3[,i] = as.factor(d3[,i])
  d4[,i] = as.factor(d4[,i])
}
d3.dummy = dummy.data.frame(d3)
d4.dummy = dummy.data.frame(d4)

#將經資料轉換的test匯出
write.csv(d4.dummy,'C:/Users/cai/Desktop/國泰競賽/testv21.csv',row.names=F)

#####利用馬氏距離找出離群值(針對連續型變數)#####
datav21<-d3.dummy
MD<-mahalanobis(x=datav21[,conti_col[-3]], center=colMeans(datav21[,conti_col[-3]]), 
                cov=cov(datav21[,conti_col[-3]]) )
MD<-round(MD,3)
head(MD,100)
md.order<-order(MD,decreasing = T)
md.order05<-md.order[1:(0.01*nrow(datav21))]
ddd=datav21[-md.order05,]

#將經資料轉換以及刪除離群值的train匯出
write.csv(ddd,'C:/Users/cai/Desktop/國泰競賽/trainv21.csv',row.names=F)
