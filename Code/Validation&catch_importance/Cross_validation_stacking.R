#####模型驗證: 用V15資料做XGB(AUC)、XGB(MERROR) stacking#####
library(dummies)
library(Matrix)
library(drat)
drat:::addRepo("dmlc")
library(xgboost)
library ( ROCR )
library ( pROC )
library(data.table)
library(kknn)
library(glmnet)
library(magrittr)
library(locfit)

data_origin = as.data.frame(fread('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\3.auc = 0.8479798714 test_submit_今天改到爆炸_發現AUC其實random_error很大_花了一段時間發現V15還不錯.csv\\train_v15_連標.csv'))
set.seed(100)
data_origin = data_origin[sample(1:100000),]
#697297
auc_sum = numeric(10)
for(wtf in 1:10){
  data_1_train_hehehehe = rbind(data_origin[which(data_origin$Y1==0)[c(1:98000)[-c(((wtf-1)*9800+1):(9800*wtf))]],],data_origin[which(data_origin$Y1==1)[c(1:2000)[-c(((wtf-1)*200+1):(200*wtf))]],])
  data_1_test_hehehehe = rbind(data_origin[which(data_origin$Y1==0)[((wtf-1)*9800+1):(9800*wtf)],],data_origin[which(data_origin$Y1==1)[((wtf-1)*200+1):(200*wtf)],])
  
  data = data_1_train_hehehehe
  data_1 = data[sample(1:nrow(data)),]
  data_real_test =  data_1_test_hehehehe
  data_real_test = data_real_test[,-ncol(data_real_test)]
  validation_predict_model_1 = matrix(0,0,1)
  test_predict_model_1 = matrix(0,nrow(data_real_test),1)
  fold_ind_train = length(which(data_1$Y1==0))
  fold_ind_test = length(which(data_1$Y1==1))
  for(i in 1:10){
    data_1_train = rbind(data_1[which(data_1$Y1==0)[c(1:fold_ind_train)[-c(((i-1)*floor(fold_ind_train/10)+1):(floor(fold_ind_train/10)*i))]],],data_1[which(data_1$Y1==1)[c(1:fold_ind_test)[-c(((i-1)*floor(fold_ind_test/10)+1):(floor(fold_ind_test/10)*i))]],])
    data_1_test = rbind(data_1[which(data_1$Y1==0)[((i-1)*floor(fold_ind_train/10)+1):(floor(fold_ind_train/10)*i)],],data_1[which(data_1$Y1==1)[((i-1)*floor(fold_ind_test/10)+1):(floor(fold_ind_test/10)*i)],])
    
    
    # ** xgboost----
    
    # Use sparse matrix in Matrix package
    # convert all feature variables to a sparse matrix
    train.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_train[,-ncol(data_1)]),
                                      label = data_1_train$Y1)
    test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_test[,-ncol(data_1)]),
                                     label = data_1_test$Y1)
    real_test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_real_test))
    
    # number of categories in response variable
    m = 2
    
    # change BUY_TYPE into 0~7, it must start in 0
    # recode Y as 0,1,2,...,m-1
    Y = data_1_train$Y1
    
    # xgboost parameters setup
    xgb.params = list(
      colsample_bytree = 0.5,                    
      subsample = 0.9,                      
      max_depth = 6,           
      #eta = 0.03,
      eval_metric = "auc",                      
      objective = "binary:logistic",
      "num_class" = 1
      #gamma = 0
    )   
    
    # build the model
    result = xgboost(param = xgb.params, data = train.xgboost.data1, label = Y, nrounds = 24) ;result
    
    # get prediction
    Ypred = predict(result, train.xgboost.data1)
    Ypred.test = predict(result, test.xgboost.data1)
    Ypred.real.test = predict(result, real_test.xgboost.data1)
    validation_predict_model_1 = rbind(validation_predict_model_1,as.matrix(Ypred.test))
    test_predict_model_1 = test_predict_model_1+Ypred.real.test
  }
  test_predict_model_1 = as.matrix(test_predict_model_1/10)
  
  
  
  #xgboost number2
  validation_predict_model_2 = matrix(0,0,1)
  test_predict_model_2 = matrix(0,nrow(data_real_test),1)
  for(i in 1:10){
    data_1_train = rbind(data_1[which(data_1$Y1==0)[c(1:fold_ind_train)[-c(((i-1)*floor(fold_ind_train/10)+1):(floor(fold_ind_train/10)*i))]],],data_1[which(data_1$Y1==1)[c(1:fold_ind_test)[-c(((i-1)*floor(fold_ind_test/10)+1):(floor(fold_ind_test/10)*i))]],])
    data_1_test = rbind(data_1[which(data_1$Y1==0)[((i-1)*floor(fold_ind_train/10)+1):(floor(fold_ind_train/10)*i)],],data_1[which(data_1$Y1==1)[((i-1)*floor(fold_ind_test/10)+1):(floor(fold_ind_test/10)*i)],])
    
    # ** xgboost----
    
    # Use sparse matrix in Matrix package
    # convert all feature variables to a sparse matrix
    train.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_train[,-ncol(data_1)]),
                                      label = data_1_train$Y1)
    test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_test[,-ncol(data_1)]),
                                     label = data_1_test$Y1)
    real_test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_real_test))
    
    # number of categories in response variable
    m = 2
    
    # change BUY_TYPE into 0~7, it must start in 0
    # recode Y as 0,1,2,...,m-1
    Y = data_1_train$Y1
    
    # xgboost parameters setup
    xgb.params = list(
      colsample_bytree = 0.8,
      subsample = 0.8,
      max_depth = 7,
      #eta = 0.03,
      eval_metric = "merror",
      objective = "multi:softprob",
      "num_class" = 2
      #gamma = 0
    )
    
    # build the model
    result = xgboost(param = xgb.params, data = train.xgboost.data1, label = Y, nrounds = 30) ;result
    
    # get prediction
    Ypred = predict(result, train.xgboost.data1)
    Ypred.test = predict(result, test.xgboost.data1)
    Ypred.test = t(matrix(Ypred.test,2,length(Ypred.test)/2))
    Ypred.real.test = predict(result, real_test.xgboost.data1)
    Ypred.real.test = t(matrix(Ypred.real.test,2,length(Ypred.real.test)/2))
    
    validation_predict_model_2 = rbind(validation_predict_model_2,as.matrix(Ypred.test[,2]))
    test_predict_model_2 = test_predict_model_2+Ypred.real.test[,2]
  }
  test_predict_model_2 = as.matrix(test_predict_model_2/10)
  
  
  stack_m_train = cbind(validation_predict_model_1,validation_predict_model_2,rep(c(rep(0,8820),rep(1,180)),10));colnames(stack_m_train) = c('model_1','model_2','Y1')
  stack_m_test = cbind(test_predict_model_1,test_predict_model_2);colnames(stack_m_test) = c('model_1','model_2')
  stack_m_train = as.data.frame(stack_m_train)
  stack_m_test = as.data.frame(stack_m_test)
  mylogit_stack <- glm(Y1 ~., data = stack_m_train, family = "binomial")
  Ypred.stack = predict(mylogit_stack, newdata = stack_m_test, type = "response")
  
  auc_sum[wtf] = auc(data_1_test_hehehehe$Y1,as.numeric(Ypred.stack))
}
#write.csv(test_predict_model_1,'C:\\Users\\USER\\Desktop\\Tim\\Stacking\\V12\\test_submit_是那個衝到0.85雖然檔名是V15但實際上是V13的那個.csv')