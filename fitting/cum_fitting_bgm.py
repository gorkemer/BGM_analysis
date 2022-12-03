#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thurs 10/06/22

@author: gorkem.er


"""

import csv
import pandas as pd
import numpy as np
import datetime
import os
import matplotlib.pyplot as plt
import seaborn as sns # for plotting
import sys
print(sys.version)


from sklearn.linear_model import LogisticRegression

os.chdir('/Users/gorkem.er/Desktop/21Projects/background-motion')

print(os.getcwd())

d = pd.read_csv('bgmdata_cleaned.csv')  

#d_LC = pd.read_csv('2AFC_LC.csv')  


#Count of task_ID 
#sns.countplot('task_ID',data=data)
#dataset['Sex'].value_counts()
#sns.lmplot(x='Age',y='Survived',data=dataset)

# aggreate responses of each of the id
#d = {'task_ID': ['mean', 'sum'], 'ATM_drawings': ['mean', 'sum']}
#res = data.groupby('participant_ID').agg(task_ID)

#before agg, I might need to round the AR diff
def trunc(values, decs=0):
    return np.trunc(values*10**decs)/(10**decs)

#data_agg_colAll = data_agg.groupby(["arDiffe2e1_round", "motionThreeLevels"]).agg({'propResp': ['mean']}).reset_index()
#data_agg_colAll.columns = ["AR_diff", "motionThreeLevels", "propRest"]


# Logistic Regression Analysis
lr_model = LogisticRegression()

from scipy.optimize import curve_fit
import scipy as sy

# psychometric function
def pf(x, alpha, beta):
    return 1. / (1 + np.exp( -(x-alpha)/beta ))


# fitting
""" par0 = sy.array([100., 1.]) # use some good starting values, reasonable default is [0., 1.]
par, mcov = curve_fit(pf, d, p2, par0)
print(par)
plt.plot(d, p2, 'ro')
plt.plot(d, pf(d, par[0], par[1]))
plt.show() """

## plotting

fig, ax = plt.subplots(figsize=(6, 6))

""" sns.regplot(x="arDiffe2e1", y = "propResp",
            data = only00.same,
            logistic = True,
            ax = ax,
            label = "same-motion")

sns.regplot(x="arDiffe2e1", y = "propResp",
            data = only00.diff,
            logistic = True,
            ax = ax,
            label = "diff-motion") """

ax.set(ylabel='y', xlabel='x')
ax.legend()
plt.show()
plt.show()











