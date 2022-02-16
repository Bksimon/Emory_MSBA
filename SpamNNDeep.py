# -*- coding: utf-8 -*-
"""
Created on Wed Sep 26 17:03:29 2018

@author: GEASTON
"""

#%% Get Spiral Data

import numpy as np
import pandas as pd
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Activation
from tensorflow.keras import optimizers

NEpochs = 10000
BatchSize=250
Optimizer=optimizers.RMSprop(lr=0.001)

# Read in the data

TrainDF = pd.read_csv('HWTrain.csv',sep=',',header=0,quotechar='"')
list(TrainDF)
ValDF = pd.read_csv('HWVal.csv',sep=',',header=0,quotechar='"')
list(ValDF)
TestDF = pd.read_csv('HWTest.csv',sep=',',header=0,quotechar='"')
list(TestDF)

#

TrIsSpam = np.array(TrainDF['IsSpam'])

TrX = np.array(TrainDF.iloc[:,:-1])

TrXrsc = (TrX - TrX.min(axis=0))/TrX.ptp(axis=0)
print(TrXrsc.shape)
print(TrXrsc.min(axis=0))
print(TrXrsc.max(axis=0))

# No need to rescale the Y because it is already 0 and 1. But check
print(TrIsSpam.min())
print(TrIsSpam.max())

# Rescale the validation data

ValIsSpam = np.array(ValDF['IsSpam'])

ValX = np.array(ValDF.iloc[:,:-1])

ValXrsc = (ValX - TrX.min(axis=0))/TrX.ptp(axis=0)
print(ValXrsc.shape)
print(ValXrsc.min(axis=0))
print(ValXrsc.max(axis=0))

print(ValIsSpam.min())
print(ValIsSpam.max())

# Rescale the test data

TestIsSpam = np.array(TestDF['IsSpam'])

TestX = np.array(TestDF.iloc[:,:-1])

TestXrsc = (TestX - TrX.min(axis=0))/TrX.ptp(axis=0)
print(TestXrsc.shape)
print(TestXrsc.min(axis=0))
print(TestXrsc.max(axis=0))

print(TestIsSpam.min())
print(TestIsSpam.max())


#%% Set up Neural Net Model

SpamNN = Sequential()

SpamNN.add(Dense(units=4,input_shape=(TrXrsc.shape[1],),activation="relu",use_bias=True))
SpamNN.add(Dense(units=4,activation="relu",use_bias=True))
SpamNN.add(Dense(units=4,activation="relu",use_bias=True))
SpamNN.add(Dense(units=4,activation="relu",use_bias=True))
SpamNN.add(Dense(units=4,activation="relu",use_bias=True))
SpamNN.add(Dense(units=1,activation="sigmoid",use_bias=True))

SpamNN.compile(loss='binary_crossentropy', optimizer=Optimizer,metrics=['binary_crossentropy','accuracy'])
print(SpamNN.summary())
#%% Fit NN Model

from keras.callbacks import EarlyStopping

StopRule = EarlyStopping(monitor='val_loss',mode='min',verbose=0,patience=100,min_delta=0.0)
FitHist = SpamNN.fit(TrXrsc,TrIsSpam,validation_data=(ValXrsc,ValIsSpam), \
                    epochs=NEpochs,batch_size=BatchSize,verbose=0, \
                    callbacks=[StopRule])

# StopRule = EarlyStopping(monitor='loss',mode='min',verbose=0,patience=100,min_delta=0.0)
# FitHist = SpamNN.fit(TrXrsc,TrIsSpam, \
#                     epochs=NEpochs,batch_size=BatchSize,verbose=0, \
#                     callbacks=[StopRule])    

print("Number of Epochs = "+str(len(FitHist.history['accuracy'])))
print("Final training accuracy: "+str(FitHist.history['accuracy'][-1]))
print("Recent history for training accuracy: "+str(FitHist.history['accuracy'][-10:-1]))
# print("Final validation accuracy: "+str(FitHist.history['val_accuracy'][-1]))
# print("Recent history for validation accuracy: "+str(FitHist.history['val_accuracy'][-10:-1]))

#%% Make Predictions

TrP = SpamNN.predict(TrXrsc,batch_size=TrXrsc.shape[0])
ValP = SpamNN.predict(ValXrsc,batch_size=TrXrsc.shape[0])
TestP = SpamNN.predict(TestXrsc,batch_size=TrXrsc.shape[0])

#%% Write out prediction

TrainDF['TrP'] = TrP.reshape(-1)
ValDF['ValP'] = ValP.reshape(-1)
TestDF['TestP'] = TestP.reshape(-1)

TrainDF.to_csv('SpamNNDeepTrainDFOutput.csv',sep=',',na_rep="NA",header=True,index=False)
ValDF.to_csv('SpamNNDeepValDFOutput.csv',sep=',',na_rep="NA",header=True,index=False)
TestDF.to_csv('SpamNNDeepTestDFOutput.csv',sep=',',na_rep="NA",header=True,index=False)

