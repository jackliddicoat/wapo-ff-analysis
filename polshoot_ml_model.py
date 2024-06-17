import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split

#load the data in
df = pd.read_csv("polshoot.csv")

# drop unnecessary columns (variables we won't use)
df = df.dropna() # first drop na vals of df
df = df.drop(['date', 'police_departments_involved', 'city', 'state', 'name'], axis=1)
X = df.drop('armed', axis = 1)

### cleaning data ###

# make dummy variables
X['gender'] = df['gender'].apply(lambda x: 1 if x == "male" else 0)
X['race'] = df['race'].apply(lambda x: 1 if x == "Black" else 0)
X['flee'] = df['flee'].apply(lambda x: 0 if x == "not" else 1)
X['signs_of_mental_illness'] = df['signs_of_mental_illness'].apply(lambda x: 1 if x == True else 0)
X['body_camera'] = df['body_camera'].apply(lambda x: 1 if x == True else 0)
y = df['armed'].apply(lambda x: 1 if x == "unarmed" else 0)
print(X); print(y) # print them both

# make our train-test split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = .2)

print(X_train.head())
print(y_train.head())


#### now we use tensorflow to make our model

import tensorflow as tf
import keras
from tensorflow.keras.models import Sequential, load_model
from tensorflow.keras.layers import Dense
from sklearn.metrics import accuracy_score

# make our model
model = Sequential()
model.add(Dense(units = 32, activation = 'relu', input_dim=len(X_train.columns)))
model.add(Dense(units = 64, activation = 'relu'))
model.add(Dense(units = 1, activation = 'sigmoid'))

# compile the model
model.compile(loss='binary_crossentropy', optimizer='sgd', metrics=['accuracy'])

# use our model on the training data
model.fit(X_train, y_train, epochs = 200, batch_size = 12) # epochs is the size to train on

# get our fitted values (yhat)
y_hat = model.predict(X_test)
y_hat = [0 if num < 0.5 else 1 for num in y_hat]
print(y_hat)

# then we can see the accuracy of our model
print(accuracy_score(y_test, y_hat))
# The model is ~92% accurate!s