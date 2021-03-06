# Cathay bigdata competition
國泰大數據競賽，為預測是否保重大疾病險問題。使用logistic regression, XGBoost, ANN, Stacking等技巧。

主辦方整理出一個XY table，其中有各個ID對應之金融、保險紀錄、人口學資料，以及其未來一年是否保重大疾病險。XY table內含缺失值。
## 資料探索：
1. 訓練資料集100,000筆、測試資料集150,000筆
2. 共有131個Feature
3. 含有缺失值的變量約為65，各個缺失值的缺失比例如下圖
![image](https://github.com/ShaoTingHsu/Cathay_bigdata_competition/blob/master/Pictures/rate_missing.PNG)
## 分析過程：
### 1. 資料清洗：
#### (1)資料清理（Data Cleaning）：
使用決策樹的方式將缺失值補齊
#### (2)資料整合（Data Integration）：
由於資料乾淨，並不需要多個dataset做合併或是同個變量下不同尺度間的統一。亦使用程式檢查，並無重複之ID，無須根據時序重新整合資料。
#### (3)資料轉換（Data Transformation）：
根據使用情境不同，我們有做出不同版本的前處理，主要在一些變量的標準化、類別資料的轉換(dummy variable)
### 2. 建立模型：
#### (1)XGBoost：Boosting架構預測器
#### (2)ANN：類神經網路架構進行分類
#### (3)Stacking：為整合多個模型所使用的技巧
它和Bagging概念雷同，使用多個模型和不同前處理，各自做出預測機率，以類似加權計算的方式算出最後可能保重大疾病險之機率(0~1之間的分數)。操作方式如下圖：
![image](https://github.com/ShaoTingHsu/Cathay_bigdata_competition/blob/master/Pictures/Staking_operation.png)
## 結果：
比賽以AUC進行排名：ROC曲線下面積，ROC曲線為計算不同threshold下，同一份答案所算出之對應True Positive Rate(Y軸)以及False Positive Rate(X軸)之值。
照理來說，預測愈有信心，則AUC愈高，因此使用Stacking來微調各筆資料預測之機率是有意義的。
最終Public AUC到達0.850789，為9/244名；Private AUC到達0.846202，為21/244名。
