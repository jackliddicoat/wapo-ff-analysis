import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression

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
# print(X); print(y) # print them both

# make our train-test split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = .2)

# logistic model
model = LogisticRegression()

# train the model
model.fit(X_train, y_train)

# use the model to predict 
model.predict(X_test)

# we can see the accuracy of our model
print(model.score(X_test, y_test))
