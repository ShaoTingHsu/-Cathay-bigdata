#####找尋重要參數#####

library(Ckmeans.1d.dp)
mylogit_stack_train <- glm(Y1 ~., data = stack_m_train, family = "binomial")
Ypred.train = predict(mylogit_stack, newdata = stack_m_train, type = "response")
Ypred.train = as.numeric(Ypred.train)
best.critical = list('ind' = sort(Ypred.train)[1],'F' = 0)
for(i in seq(0.01231,0.01623,length.out = 200)){
  Ypred.train.response = ifelse(Ypred.train>i,1,0)
  cm.xgboost = table(rep(c(rep(0,9800),rep(1,200)),10), Ypred.train.response, dnn = c("Real", "Predict"))
  precision.temp = cm.xgboost[2,2]/(cm.xgboost[2,2]+cm.xgboost[1,2])
  recall.temp = cm.xgboost[2,2]/(cm.xgboost[2,2]+cm.xgboost[2,1])
  F.temp = (precision.temp*recall.temp)/(precision.temp+recall.temp)
  if(F.temp>best.critical$F){best.critical$ind = i;best.critical$F = F.temp}
}
Ypred.train.response = ifelse(Ypred.train>best.critical$ind,1,0)
cm.xgboost.train = table(rep(c(rep(0,9800),rep(1,200)),10), Ypred.train.response, dnn = c("Real", "Predict"));cm.xgboost
Ypred.test = predict(mylogit_stack, newdata = stack_m_test, type = "response")
Ypred.test = as.numeric(Ypred.test)
Ypred.test.response = ifelse(Ypred.test>best.critical$ind,1,0)
data_real_test_predictYis1 = data_real_test[which(Ypred.test.response==1),]
conti_col=c('AGE','EDUCATION_CD','L1YR_B_ISSUE_CNT','CHANNEL_A_POL_CNT','OCCUPATION_CLASS_CD',
            'APC_CNT','INSD_CNT','APC_1ST_AGE','INSD_1ST_AGE','APC_1ST_YEARDIF','RFM_R',
            'REBUY_TIMES_CNT','LEVEL','RFM_M_LEVEL','LIFE_CNT','ANNUAL_PREMIUM_AMT','AG_CNT','AG_NOW_CNT',
            'CLC_CUR_NUM','ANNUAL_INCOME_AMT','BANK_NUMBER_CNT','INSD_LAST_YEARDIF_CNT',
            'BMI','TERMINATION_RATE','TOOL_VISIT_1YEAR_CNT','DIEBENEFIT_AMT',
            'POLICY_VALUE_AMT','ANNUITY_AMT','EXPIRATION_AMT',
            'PAY_LIMIT_MED_MISC_AMT','FIRST_CANCER_AMT','ILL_ACCELERATION_AMT','ILL_ADDITIONAL_AMT',
            'LONG_TERM_CARE_AMT','MONTHLY_CARE_AMT','LIFE_INSD_CNT',
            'L1YR_GROSS_PRE_AMT','IM_pca_PC1','IM_pca_PC2',
            'IF_ADD_IND_pca_PC1','IF_ADD_IND_pca_PC2','IF_ADD_IND_pca_PC3','Hospital_pca')

par(mfrow = c(1,1)) 
for(i in intersect(colnames(data_real_test),conti_col)){
  jpeg(paste0('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\結論圖片\\',i,'_overall.jpeg'))
  hist(data_real_test[,i],main = paste0('overall | ',i))
  dev.off()
  jpeg(paste0('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\結論圖片\\',i,'_Yis1.jpeg'))
  hist(data_real_test_predictYis1[,i],main = paste0('Y is 1 | ',i),xlim = range(data_real_test[,i]))
  dev.off()
}
for(i in intersect(colnames(data_real_test),conti_col)){
  jpeg(paste0('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\結論圖片_連續型變數、boxplot\\',i,'.jpeg'))
  boxplot(data_real_test[,i],data_real_test_predictYis1[,i],main = paste0(i))
  dev.off()
}
for(i in setdiff(colnames(data_real_test),conti_col)){
  jpeg(paste0('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\結論圖片_類別型變數\\',i,'_overall.jpeg'))
  hist(data_real_test[,i],breaks = dim(table(data_real_test[,i])),main = paste0('overall | ',i))
  dev.off()
  jpeg(paste0('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\結論圖片_類別型變數\\',i,'_Yis1.jpeg'))
  hist(data_real_test_predictYis1[,i],breaks = dim(table(data_real_test_predictYis1[,i])),main = paste0('Y is 1 | ',i),xlim = range(data_real_test[,i]))
  dev.off()
}

# ** xgboost importance----
xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1[,-ncol(data_1)]),
                            label = data_1$Y1)
Y = data_1$Y1
xgb.params = list(
  colsample_bytree = 0.5,                    
  subsample = 0.9,                      
  max_depth = 6,           
  eval_metric = "auc",                      
  objective = "binary:logistic",
  "num_class" = 1
)   
result = xgboost(param = xgb.params, data = xgboost.data1, label = Y, nrounds = 24) ;result