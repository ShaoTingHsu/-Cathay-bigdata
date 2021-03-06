#####basic library#####
import os
import numpy as np
import pandas as pd

from tensorflow import set_random_seed 
from keras.models import Sequential
from keras.layers import Activation, Dropout, Dense
from keras.optimizers import Adam

#####read the dataset#####
root = "/home/0752619/怍/"
os.chdir(root) 
pddata1 = pd.read_csv("train000.csv") 
pddata2 = pd.read_csv("test000.csv")

X1=pddata1.iloc[:, 0:15]
X2=pddata2.iloc[:, 0:15]

Y1=pddata1['Y1']

#####model#####
model=Sequential()
model.add(Dense(input_dim=15,units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=1, activation='sigmoid'))
print('Model Summary')
model.summary()

#####compile the model#####
model.compile(optimizer = Adam(lr = 1e-4), loss = 'binary_crossentropy',
                          metrics = ['acc'])

np.random.seed(4444)
modelres=model.fit(X1, Y1,
             batch_size = 1024,
             epochs = 100,
             verbose = 1,
             shuffle = True,
             validation_split = 0.2)

#####re-read the data with the model######
pddata1 = pd.read_csv("train000.csv")
pddata1.head()
X=pddata1.iloc[:, 0:15]
Y=pddata1['Y1']
x_train, x_test, y_train, y_test = train_test_split(X, Y, test_size = 0.2, random_state = 4444)

#####build model#####
model=Sequential()
model.add(Dense(input_dim=15,units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=256, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(units=1, activation='sigmoid'))
print('Model Summary')
model.summary()

#####compile the model#####
model.compile(optimizer = Adam(lr = 1e-4), loss = 'binary_crossentropy',
                           metrics = ['accuracy'])

#####fit the model#####
np.random.seed(4)
set_random_seed(7)
modelres=model.fit(x_train, y_train,
             batch_size = 512,
             epochs = 100,
             verbose = 1,
             shuffle = True,
             validation_split = 0.2)