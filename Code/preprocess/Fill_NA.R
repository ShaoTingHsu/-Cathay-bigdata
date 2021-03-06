#針對數值型變數以及有序的類別型變數
anova_mod<-rpart(formula = APC_1ST_YEARDIF ~ ., data = data_x2[!is.na(data_x2$APC_1ST_YEARDIF),][-1], method = "anova", na.action = na.omit)
APC_1ST_YEARDIF_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$APC_1ST_YEARDIF),])
data_x2$APC_1ST_YEARDIF[is.na(data_x2$APC_1ST_YEARDIF)]<-APC_1ST_YEARDIF_predict

anova_mod<-rpart(formula = DIEBENEFIT_AMT ~ ., data = data_x2[!is.na(data_x2$DIEBENEFIT_AMT),][-1], method = "anova", na.action = na.omit)
DIEBENEFIT_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$DIEBENEFIT_AMT),])
data_x2$DIEBENEFIT_AMT[is.na(data_x2$DIEBENEFIT_AMT)]<-DIEBENEFIT_AMT_predict

anova_mod<-rpart(formula = POLICY_VALUE_AMT ~ ., data = data_x2[!is.na(data_x2$POLICY_VALUE_AMT),][-1], method = "anova", na.action = na.omit)
POLICY_VALUE_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$POLICY_VALUE_AMT),])
data_x2$POLICY_VALUE_AMT[is.na(data_x2$POLICY_VALUE_AMT)]<-POLICY_VALUE_AMT_predict

anova_mod<-rpart(formula = ANNUITY_AMT ~ ., data = data_x2[!is.na(data_x2$ANNUITY_AMT),][-1], method = "anova", na.action = na.omit)
ANNUITY_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$ANNUITY_AMT),])
data_x2$ANNUITY_AMT[is.na(data_x2$ANNUITY_AMT)]<-ANNUITY_AMT_predict

anova_mod<-rpart(formula = EXPIRATION_AMT ~ ., data = data_x2[!is.na(data_x2$EXPIRATION_AMT),][-1], method = "anova", na.action = na.omit)
EXPIRATION_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$EXPIRATION_AMT),])
data_x2$EXPIRATION_AMT[is.na(data_x2$EXPIRATION_AMT)]<-EXPIRATION_AMT_predict

anova_mod<-rpart(formula = ACCIDENT_HOSPITAL_REC_AMT ~ ., data = data_x2[!is.na(data_x2$ACCIDENT_HOSPITAL_REC_AMT),][-1], method = "anova", na.action = na.omit)
ACCIDENT_HOSPITAL_REC_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$ACCIDENT_HOSPITAL_REC_AMT),])
data_x2$ACCIDENT_HOSPITAL_REC_AMT[is.na(data_x2$ACCIDENT_HOSPITAL_REC_AMT)]<-ACCIDENT_HOSPITAL_REC_AMT_predict

anova_mod<-rpart(formula =  DISEASES_HOSPITAL_REC_AMT ~ ., data = data_x2[!is.na(data_x2$DISEASES_HOSPITAL_REC_AMT),][-1], method = "anova", na.action = na.omit)
DISEASES_HOSPITAL_REC_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$DISEASES_HOSPITAL_REC_AMT),])
data_x2$DISEASES_HOSPITAL_REC_AMT[is.na(data_x2$DISEASES_HOSPITAL_REC_AMT)]<- DISEASES_HOSPITAL_REC_AMT_predict


anova_mod<-rpart(formula = OUTPATIENT_SURGERY_AMT ~ ., data = data_x2[!is.na(data_x2$OUTPATIENT_SURGERY_AMT),][-1], method = "anova", na.action = na.omit)
OUTPATIENT_SURGERY_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$OUTPATIENT_SURGERY_AMT),])
data_x2$OUTPATIENT_SURGERY_AMT[is.na(data_x2$OUTPATIENT_SURGERY_AMT)]<-OUTPATIENT_SURGERY_AMT_predict

anova_mod<-rpart(formula =INPATIENT_SURGERY_AMT ~ ., data = data_x2[!is.na(data_x2$INPATIENT_SURGERY_AMT),][-1], method = "anova", na.action = na.omit)
INPATIENT_SURGERY_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$INPATIENT_SURGERY_AMT),])
data_x2$INPATIENT_SURGERY_AMT[is.na(data_x2$INPATIENT_SURGERY_AMT)]<- INPATIENT_SURGERY_AMT_predict

anova_mod<-rpart(formula = PAY_LIMIT_MED_MISC_AMT ~ ., data = data_x2[!is.na(data_x2$PAY_LIMIT_MED_MISC_AMT),][-1], method = "anova", na.action = na.omit)
PAY_LIMIT_MED_MISC_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$PAY_LIMIT_MED_MISC_AMT),])
data_x2$PAY_LIMIT_MED_MISC_AMT[is.na(data_x2$PAY_LIMIT_MED_MISC_AMT)]<- PAY_LIMIT_MED_MISC_AMT_predict

anova_mod<-rpart(formula = FIRST_CANCER_AMT ~ ., data = data_x2[!is.na(data_x2$FIRST_CANCER_AMT),][-1], method = "anova", na.action = na.omit)
FIRST_CANCER_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$FIRST_CANCER_AMT),])
data_x2$FIRST_CANCER_AMT[is.na(data_x2$FIRST_CANCER_AMT)]<-FIRST_CANCER_AMT_predict

anova_mod<-rpart(formula = ILL_ACCELERATION_AMT ~ ., data = data_x2[!is.na(data_x2$ILL_ACCELERATION_AMT),][-1], method = "anova", na.action = na.omit)
ILL_ACCELERATION_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$ILL_ACCELERATION_AMT),])
data_x2$ILL_ACCELERATION_AMT[is.na(data_x2$ILL_ACCELERATION_AMT)]<- ILL_ACCELERATION_AMT_predict

anova_mod<-rpart(formula = ILL_ADDITIONAL_AMT ~ ., data = data_x2[!is.na(data_x2$ILL_ADDITIONAL_AMT),][-1], method = "anova", na.action = na.omit)
ILL_ADDITIONAL_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$ILL_ADDITIONAL_AMT),])
data_x2$ILL_ADDITIONAL_AMT[is.na(data_x2$ILL_ADDITIONAL_AMT)]<- ILL_ADDITIONAL_AMT_predict

anova_mod<-rpart(formula = LONG_TERM_CARE_AMT ~ ., data = data_x2[!is.na(data_x2$LONG_TERM_CARE_AMT),][-1], method = "anova", na.action = na.omit)
LONG_TERM_CARE_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$LONG_TERM_CARE_AMT),])
data_x2$LONG_TERM_CARE_AMT[is.na(data_x2$LONG_TERM_CARE_AMT)]<-LONG_TERM_CARE_AMT_predict

anova_mod<-rpart(formula =  MONTHLY_CARE_AMT ~ ., data = data_x2[!is.na(data_x2$MONTHLY_CARE_AMT),][-1], method = "anova", na.action = na.omit)
MONTHLY_CARE_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$MONTHLY_CARE_AMT),])
data_x2$MONTHLY_CARE_AMT[is.na(data_x2$MONTHLY_CARE_AMT)]<- MONTHLY_CARE_AMT_predict

anova_mod<-rpart(formula =  LEVEL ~ ., data = data_x2[!is.na(data_x2$LEVEL),][-1], method = "anova", na.action = na.omit)
LEVEL_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$LEVEL),])
data_x2$LEVEL[is.na(data_x2$LEVEL)]<- LEVEL_predict

anova_mod<-rpart(formula =  RFM_R ~ ., data = data_x2[!is.na(data_x2$RFM_R),][-1], method = "anova", na.action = na.omit)
RFM_R_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$RFM_R),])
data_x2$RFM_R[is.na(data_x2$RFM_R)]<- RFM_R_predict

anova_mod<-rpart(formula =APC_1ST_AGE ~ ., data = data_x2[!is.na(data_x2$APC_1ST_AGE),][-1], method = "anova", na.action = na.omit)
APC_1ST_AGE_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$APC_1ST_AGE),])
data_x2$APC_1ST_AGE[is.na(data_x2$APC_1ST_AGE)]<- APC_1ST_AGE_predict

anova_mod<-rpart(formula =REBUY_TIMES_CNT ~ ., data = data_x2[!is.na(data_x2$REBUY_TIMES_CNT),][-1], method = "anova", na.action = na.omit)
REBUY_TIMES_CNT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$REBUY_TIMES_CNT),])
data_x2$REBUY_TIMES_CNT[is.na(data_x2$REBUY_TIMES_CNT)]<- REBUY_TIMES_CNT_predict

anova_mod<-rpart(formula =TERMINATION_RATE ~ ., data = data_x2[!is.na(data_x2$TERMINATION_RATE),][-1], method = "anova", na.action = na.omit)
TERMINATION_RATE_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$TERMINATION_RATE),])
data_x2$TERMINATION_RATE[is.na(data_x2$TERMINATION_RATE)]<- TERMINATION_RATE_predict

anova_mod<-rpart(formula =RFM_M_LEVEL ~ ., data = data_x2[!is.na(data_x2$RFM_M_LEVEL),][-1], method = "anova", na.action = na.omit)
RFM_M_LEVEL_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$RFM_M_LEVEL),])
data_x2$RFM_M_LEVEL[is.na(data_x2$RFM_M_LEVEL)]<- RFM_M_LEVEL_predict

anova_mod<-rpart(formula =BMI ~ ., data = data_x2[!is.na(data_x2$BMI),][-1], method = "anova", na.action = na.omit)
BMI_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$BMI),])
data_x2$BMI[is.na(data_x2$BMI)]<- BMI_predict

anova_mod<-rpart(formula =EDUCATION_CD ~ ., data = data_x2[!is.na(data_x2$EDUCATION_CD),][-1], method = "anova", na.action = na.omit)
EDUCATION_CD_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$EDUCATION_CD),])
data_x2$EDUCATION_CD[is.na(data_x2$EDUCATION_CD)]<- EDUCATION_CD_predict

anova_mod<-rpart(formula =ANNUAL_INCOME_AMT ~ ., data = data_x2[!is.na(data_x2$ANNUAL_INCOME_AMT),][-1], method = "anova", na.action = na.omit)
ANNUAL_INCOME_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$ANNUAL_INCOME_AMT),])
data_x2$ANNUAL_INCOME_AMT[is.na(data_x2$ANNUAL_INCOME_AMT)]<- ANNUAL_INCOME_AMT_predict

anova_mod<-rpart(formula =OCCUPATION_CLASS_CD ~ ., data = data_x2[!is.na(data_x2$OCCUPATION_CLASS_CD),][-1], method = "anova", na.action = na.omit)
OCCUPATION_CLASS_CD_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$OCCUPATION_CLASS_CD),])
data_x2$OCCUPATION_CLASS_CD[is.na(data_x2$EOCCUPATION_CLASS_CD)]<- OCCUPATION_CLASS_CD_predict

anova_mod<-rpart(formula =INSD_1ST_AGE ~ ., data = data_x2[!is.na(data_x2$INSD_1ST_AGE),][-1], method = "anova", na.action = na.omit)
INSD_1ST_AGE_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$INSD_1ST_AGE),])
data_x2$INSD_1ST_AGE[is.na(data_x2$INSD_1ST_AGE)]<- INSD_1ST_AGE_predict

anova_mod<-rpart(formula =INSD_LAST_YEARDIF_CNT ~ ., data = data_x2[!is.na(data_x2$INSD_LAST_YEARDIF_CNT),][-1], method = "anova", na.action = na.omit)
INSD_LAST_YEARDIF_CNT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$INSD_LAST_YEARDIF_CNT),])
data_x2$INSD_LAST_YEARDIF_CNT[is.na(data_x2$INSD_LAST_YEARDIF_CNT)]<- INSD_LAST_YEARDIF_CNT_predict

anova_mod<-rpart(formula =OCCUPATION_CLASS_CD ~ ., data = data_x2[!is.na(data_x2$OCCUPATION_CLASS_CD),][-1], method = "anova", na.action = na.omit)
OCCUPATION_CLASS_CD_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$OCCUPATION_CLASS_CD),])
data_x2$OCCUPATION_CLASS_CD[is.na(data_x2$OCCUPATION_CLASS_CD)]<- OCCUPATION_CLASS_CD_predict

anova_mod<-rpart(formula = ANNUAL_PREMIUM_AMT ~ ., data = data_x2[!is.na(data_x2$ANNUAL_PREMIUM_AMT),][-1], method = "anova",na.action = na.omit)
ANNUAL_PREMIUM_AMT_predict <- predict(object = anova_mod, newdata = data_x2[is.na(data_x2$ANNUAL_PREMIUM_AMT),])
data_x2$ANNUAL_PREMIUM_AMT[is.na(data_x2$ANNUAL_PREMIUM_AMT)]<-ANNUAL_PREMIUM_AMT_predict

#針對類別型變數
class_mod<-rpart(formula = X_A_IND ~ ., data = data_x2[!is.na(data_x2$X_A_IND),][-1], method = "class",na.action = na.omit)
X_A_IND_predict <- predict(object = class_mod, newdata = data_x2[is.na(data_x2$X_A_IND),], type = 'class')
X_A_IND_predict<-as.numeric(as.character(X_A_IND_predict))
data_x2$X_A_IND[is.na(data_x2$X_A_IND)]<-X_A_IND_predict

class_mod<-rpart(formula = X_B_IND ~ ., data = data_x2[!is.na(data_x2$X_B_IND),][-1], method = "class",na.action = na.omit)
X_B_IND_predict <- predict(object = class_mod, newdata = data_x2[is.na(data_x2$X_B_IND),], type = 'class')
X_B_IND_predict<-as.numeric(as.character(X_B_IND_predict))
data_x2$X_B_IND[is.na(data_x2$X_B_IND)]<-X_B_IND_predict

class_mod<-rpart(formula = X_C_IND ~ ., data = data_x2[!is.na(data_x2$X_C_IND),][-1], method = "class",na.action = na.omit)
X_C_IND_predict <- predict(object = class_mod, newdata = data_x2[is.na(data_x2$X_C_IND),], type = 'class')
X_C_IND_predict<-as.numeric(as.character(X_C_IND_predict))
data_x2$X_C_IND[is.na(data_x2$X_C_IND)]<-X_C_IND_predict

class_mod<-rpart(formula = X_D_IND ~ ., data = data_x2[!is.na(data_x2$X_D_IND),][-1], method = "class",na.action = na.omit)
X_D_IND_predict <- predict(object = class_mod, newdata = data_x2[is.na(data_x2$X_D_IND),], type = 'class')
X_D_IND_predict<-as.numeric(as.character(X_D_IND_predict))
data_x2$X_D_IND[is.na(data_x2$X_D_IND)]<-X_D_IND_predict

class_mod<-rpart(formula = X_E_IND ~ ., data = data_x2[!is.na(data_x2$X_E_IND),][-1], method = "class",na.action = na.omit)
X_E_IND_predict <- predict(object = class_mod, newdata = data_x2[is.na(data_x2$X_E_IND),], type = 'class')
X_E_IND_predict<-as.numeric(as.character(X_E_IND_predict))
data_x2$X_E_IND[is.na(data_x2$X_E_IND)]<-X_E_IND_predict

class_mod<-rpart(formula = X_F_IND ~ ., data = data_x2[!is.na(data_x2$X_F_IND),][-1], method = "class",na.action = na.omit)
X_F_IND_predict <- predict(object = class_mod, newdata = data_x2[is.na(data_x2$X_F_IND),], type = 'class')
X_F_IND_predict<-as.numeric(as.character(X_F_IND_predict))
data_x2$X_F_IND[is.na(data_x2$X_F_IND)]<-X_F_IND_predict

class_mod<-rpart(formula = X_G_IND ~ ., data = data_x2[!is.na(data_x2$X_G_IND),][-1], method = "class",na.action = na.omit)
X_G_IND_predict <- predict(object = class_mod, newdata = data_x2[is.na(data_x2$X_G_IND),], type = 'class')
X_G_IND_predict<-as.numeric(as.character(X_G_IND_predict))
data_x2$X_G_IND[is.na(data_x2$X_G_IND)]<-X_G_IND_predict

class_mod<-rpart(formula = X_H_IND ~ ., data = data_x2[!is.na(data_x2$X_H_IND),][-1], method = "class",na.action = na.omit)
X_H_IND_predict <- predict(object = class_mod, newdata = data_x2[is.na(data_x2$X_H_IND),], type = 'class')
X_H_IND_predict<-as.numeric(as.character(X_H_IND_predict))
data_x2$X_H_IND[is.na(data_x2$X_H_IND)]<-X_H_IND_predict

class_mod<-rpart(formula = IF_ADD_INSD_IND ~ ., data = data_x2[!is.na(data_x2$IF_ADD_INSD_IND),][-1], method = "class",na.action = na.omit)
IF_ADD_INSD_IND_predict <- predict(object = class_mod, newdata = data_x2[is.na(data_x2$IF_ADD_INSD_IND),], type = 'class')
IF_ADD_INSD_IND_predict<-as.numeric(as.character(IF_ADD_INSD_IND_predict))
data_x2$IF_ADD_INSD_IND[is.na(data_x2$IF_ADD_INSD_IND)]<-IF_ADD_INSD_IND_predict

class_mod<-rpart(formula = MARRIAGE_CD ~ ., data = data_x2[!is.na(data_x2$MARRIAGE_CD),][-1], method = "class",na.action = na.omit)
MARRIAGE_CD_predict <- predict(object = class_mod, newdata = data_x2[is.na(data_x2$MARRIAGE_CD),], type = 'class')
MARRIAGE_CD_predict<-as.numeric(as.character(MARRIAGE_CD_predict))
data_x2$MARRIAGE_CD[is.na(data_x2$MARRIAGE_CD)]<-MARRIAGE_CD_predict

class_mod<-rpart(formula = GENDER ~ ., data = data_x2[!is.na(data_x2$GENDER),][-1], method = "class",na.action = na.omit)
GENDER_predict <- predict(object = class_mod, newdata = data_x2[is.na(data_x2$GENDER),], type = 'class')
GENDER_predict<-as.numeric(as.character(GENDER_predict))
data_x2$GENDER[is.na(data_x2$GENDER)]<-GENDER_predict

data_train = data_x2[1:100000,]
data_train = cbind(data_train,train_0$Y1);colnames(data_train)[ncol(data_train)] = 'Y1'
data_test = data_x2[100001:250000,]

#再將補完值的資料重新拆成train及test
save(data_train,file='data_train.R');save(data_test,file='data_test.R')
load('data_train.R');load('data_test.R')
d1<-data_train;dd<-data_test