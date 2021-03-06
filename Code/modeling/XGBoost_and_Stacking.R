#####將5個不同版本前處理的資料集做stacking#####
library(data.table)
data = as.data.frame(fread('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\8. auc = 0.8479301658  test_submit.csv\\0930 有最好結果的 auc 0.8479 nruns=30-20191003T161027Z-001\\0930 有最好結果的 auc = 0.8479 nruns=30\\train_v10.csv'))
data = cbind(1:100000,data);colnames(data)[1] = 'ind'
set.seed(100)
data_1 = data[sample(1:100000),]
data_1_train_Yis0 = list()
data_1_train_Yis1 = list()
data_1_test_Yis0 = list()
data_1_test_Yis1 = list()
#切分stacking用training set& testing set
for(i in 1:10){
  data_1_train_Yis0[[i]] = data_1$ind[which(data_1$Y1==0)[c(1:98000)[-c(((i-1)*9800+1):(9800*i))]]]
  data_1_train_Yis1[[i]] = data_1$ind[which(data_1$Y1==1)[c(1:2000)[-c(((i-1)*200+1):(200*i))]]]
  data_1_test_Yis0[[i]] = data_1$ind[which(data_1$Y1==0)[((i-1)*9800+1):(9800*i)]]
  data_1_test_Yis1[[i]] = data_1$ind[which(data_1$Y1==1)[((i-1)*200+1):(200*i)]]
}

#第8個檔案夾 V10
data_origin = as.data.frame(fread('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\8. auc = 0.8479301658  test_submit.csv\\0930 有最好結果的 auc 0.8479 nruns=30-20191003T161027Z-001\\0930 有最好結果的 auc = 0.8479 nruns=30\\train_v10.csv'))
data_real_test = as.data.frame(fread('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\8. auc = 0.8479301658  test_submit.csv\\0930 有最好結果的 auc 0.8479 nruns=30-20191003T161027Z-001\\0930 有最好結果的 auc = 0.8479 nruns=30\\test_v10.csv'))
validation_predict_model_1 = matrix(0,0,1)
test_predict_model_1 = matrix(0,150000,1)
for(i in 1:10){
  data_1_train = data_origin[c(data_1_train_Yis0[[i]],data_1_train_Yis1[[i]]),]
  data_1_test = data_origin[c(data_1_test_Yis0[[i]],data_1_test_Yis1[[i]]),]
  
  # ** xgboost----
  
  # Use sparse matrix in Matrix package
  # convert all feature variables to a sparse matrix
  train.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_train[,-ncol(data_1_train)]),
                                    label = data_1_train$Y1)
  test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_test[,-ncol(data_1_test)]),
                                   label = data_1_test$Y1)
  real_test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_real_test))

  # change BUY_TYPE into 0~7, it must start in 0
  # recode Y as 0,1,2,...,m-1
  Y = data_1_train$Y1
  
  # xgboost parameters setup
  xgb.params = list(
    colsample_bytree = 0.6,                    
    subsample = 1,                      
    max_depth = 5,           
    #eta = 0.03,
    eval_metric = "auc",                      
    objective = "binary:logistic",
    "num_class" = 1
    #gamma = 0
  )   
  
  # build the model
  result = xgboost(param = xgb.params, data = train.xgboost.data1, label = Y, nrounds = 31) ;result
  
  # get prediction
  Ypred = predict(result, train.xgboost.data1)
  Ypred.test = predict(result, test.xgboost.data1)
  Ypred.real.test = predict(result, real_test.xgboost.data1)
  validation_predict_model_1 = rbind(validation_predict_model_1,as.matrix(Ypred.test))
  test_predict_model_1 = test_predict_model_1+Ypred.real.test
}
test_predict_model_1 = as.matrix(test_predict_model_1/10)

#第7個資料夾 V11全標歸、xgb
data_origin = as.data.frame(fread('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\7.auc = 0.8473069239 test_submit_V11.csv\\0930 全標歸-20191003T160814Z-001\\0930 全標歸\\trainv11_dummy.csv'))
data_real_test = as.data.frame(fread('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\7.auc = 0.8473069239 test_submit_V11.csv\\0930 全標歸-20191003T160814Z-001\\0930 全標歸\\testv11_dummy.csv'))
validation_predict_model_2 = matrix(0,0,1)
test_predict_model_2 = matrix(0,150000,1)
for(i in 1:10){
  data_1_train = data_origin[c(data_1_train_Yis0[[i]],data_1_train_Yis1[[i]]),]
  data_1_test = data_origin[c(data_1_test_Yis0[[i]],data_1_test_Yis1[[i]]),]
  
  # ** xgboost----
  
  # Use sparse matrix in Matrix package
  # convert all feature variables to a sparse matrix
  train.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_train[,-ncol(data_1_train)]),
                                    label = data_1_train$Y1)
  test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_test[,-ncol(data_1_test)]),
                                   label = data_1_test$Y1)
  real_test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_real_test))
  # change BUY_TYPE into 0~7, it must start in 0
  # recode Y as 0,1,2,...,m-1
  Y = data_1_train$Y1
  
  # xgboost parameters setup
  xgb.params = list(
    colsample_bytree = 0.8,                    
    subsample = 0.8,                      
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
  validation_predict_model_2 = rbind(validation_predict_model_2,as.matrix(Ypred.test))
  test_predict_model_2 = test_predict_model_2+Ypred.real.test
}
test_predict_model_2 = as.matrix(test_predict_model_2/10)

#第6個資料夾 V18
data_origin = as.data.frame(fread('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\6.auc = 0.8439446091  test_submit_v18_v10刪5.csv\\train_v18_v10刪5.csv'))
data_real_test = as.data.frame(fread('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\6.auc = 0.8439446091  test_submit_v18_v10刪5.csv\\test_v18_v10刪5.csv'))
validation_predict_model_3 = matrix(0,0,1)
test_predict_model_3 = matrix(0,150000,1)
for(i in 1:10){
  data_1_train = data_origin[c(data_1_train_Yis0[[i]],data_1_train_Yis1[[i]]),]
  data_1_test = data_origin[c(data_1_test_Yis0[[i]],data_1_test_Yis1[[i]]),]
  
  # ** xgboost----
  
  # Use sparse matrix in Matrix package
  # convert all feature variables to a sparse matrix
  train.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_train[,-ncol(data_1_train)]),
                                    label = data_1_train$Y1)
  test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_test[,-ncol(data_1_test)]),
                                   label = data_1_test$Y1)
  real_test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_real_test))
  # change BUY_TYPE into 0~7, it must start in 0
  # recode Y as 0,1,2,...,m-1
  Y = data_1_train$Y1
  
  # xgboost parameters setup
  xgb.params = list(
    colsample_bytree = 0.8,                    
    subsample = 0.8,                      
    max_depth = 6,           
    #eta = 0.03,
    eval_metric = "auc",                      
    objective = "binary:logistic",
    "num_class" = 1
    #gamma = 0
  )   
  
  # build the model
  result = xgboost(param = xgb.params, data = train.xgboost.data1, label = Y, nrounds = 30) ;result
  
  # get prediction
  Ypred = predict(result, train.xgboost.data1)
  Ypred.test = predict(result, test.xgboost.data1)
  Ypred.real.test = predict(result, real_test.xgboost.data1)
  validation_predict_model_3 = rbind(validation_predict_model_3,as.matrix(Ypred.test))
  test_predict_model_3 = test_predict_model_3+Ypred.real.test
}
test_predict_model_3 = as.matrix(test_predict_model_3/10)

#第5個資料夾 V13刪五連標歸
data_origin = as.data.frame(fread('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\5.auc = 0.8462951066  test_submit_v13_刪5連標歸.csv\\train_v13_刪5連標歸.csv'))
data_real_test = as.data.frame(fread('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\5.auc = 0.8462951066  test_submit_v13_刪5連標歸.csv\\test_v13_刪5連標歸.csv'))
validation_predict_model_4 = matrix(0,0,1)
test_predict_model_4 = matrix(0,150000,1)
for(i in 1:10){
  data_1_train = data_origin[c(data_1_train_Yis0[[i]],data_1_train_Yis1[[i]]),]
  data_1_test = data_origin[c(data_1_test_Yis0[[i]],data_1_test_Yis1[[i]]),]
  
  # ** xgboost----
  
  # Use sparse matrix in Matrix package
  # convert all feature variables to a sparse matrix
  train.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_train[,-ncol(data_1_train)]),
                                    label = data_1_train$Y1)
  test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_test[,-ncol(data_1_test)]),
                                   label = data_1_test$Y1)
  real_test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_real_test))
  # change BUY_TYPE into 0~7, it must start in 0
  # recode Y as 0,1,2,...,m-1
  Y = data_1_train$Y1
  
  # xgboost parameters setup
  xgb.params = list(
    colsample_bytree = 0.8,                    
    subsample = 0.8,                      
    max_depth = 6,           
    #eta = 0.03,
    eval_metric = "auc",                      
    objective = "binary:logistic",
    "num_class" = 1
    #gamma = 0
  )   
  
  # build the model
  result = xgboost(param = xgb.params, data = train.xgboost.data1, label = Y, nrounds = 30) ;result
  
  # get prediction
  Ypred = predict(result, train.xgboost.data1)
  Ypred.test = predict(result, test.xgboost.data1)
  Ypred.real.test = predict(result, real_test.xgboost.data1)
  validation_predict_model_4 = rbind(validation_predict_model_4,as.matrix(Ypred.test))
  test_predict_model_4 = test_predict_model_4+Ypred.real.test
}
test_predict_model_4 = as.matrix(test_predict_model_4/10)

#第3個資料夾
data_origin = as.data.frame(fread('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\3.auc = 0.8479798714 test_submit_今天改到爆炸_發現AUC其實random_error很大_花了一段時間發現V15還不錯.csv\\train_v15_連標.csv'))
data_real_test = as.data.frame(fread('D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\3.auc = 0.8479798714 test_submit_今天改到爆炸_發現AUC其實random_error很大_花了一段時間發現V15還不錯.csv\\test_v15_連標.csv'))
validation_predict_model_5 = matrix(0,0,1)
test_predict_model_5 = matrix(0,150000,1)
for(i in 1:10){
  data_1_train = data_origin[c(data_1_train_Yis0[[i]],data_1_train_Yis1[[i]]),]
  data_1_test = data_origin[c(data_1_test_Yis0[[i]],data_1_test_Yis1[[i]]),]
  
  # ** xgboost----
  
  # Use sparse matrix in Matrix package
  # convert all feature variables to a sparse matrix
  train.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_train[,-ncol(data_1_train)]),
                                    label = data_1_train$Y1)
  test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_1_test[,-ncol(data_1_test)]),
                                   label = data_1_test$Y1)
  real_test.xgboost.data1 = xgb.DMatrix(data = as.matrix(data_real_test))
  # change BUY_TYPE into 0~7, it must start in 0
  # recode Y as 0,1,2,...,m-1
  Y = data_1_train$Y1
  
  # xgboost parameters setup
  xgb.params = list(
    colsample_bytree = 0.6,                    
    subsample = 1,                      
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
  validation_predict_model_5 = rbind(validation_predict_model_5,as.matrix(Ypred.test))
  test_predict_model_5 = test_predict_model_5+Ypred.real.test
}
test_predict_model_5 = as.matrix(test_predict_model_5/10)

#做五個模型的stacking
train_stack = cbind(validation_predict_model_1,validation_predict_model_2,validation_predict_model_3,validation_predict_model_4,validation_predict_model_5,rep(c(rep(0,9800),rep(1,200)),10));colnames(train_stack) = c('model1','model2','model3','model4','model5','Y1')
test_stack =  cbind(test_predict_model_1,test_predict_model_2,test_predict_model_3,test_predict_model_4,test_predict_model_5);colnames(test_stack) = c('model1','model2','model3','model4','model5')
train_stack = as.data.frame(train_stack)
test_stack = as.data.frame(test_stack)
#第二層使用logistic regression----
mylogit_stack <- glm(Y1 ~., data = train_stack, family = "binomial")
Ypred.stack = predict(mylogit_stack, newdata = test_stack, type = "response")
write.csv(Ypred.stack,'D:\\USER\\Documents\\Tim Hsu\\國泰大數據\\流程圖\\test_submit_五個模型放在一起_第二層用logistic.csv')