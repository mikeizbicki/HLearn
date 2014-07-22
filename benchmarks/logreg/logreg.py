#!/usr/bin/python

import sys

import numpy as np

from sklearn import linear_model

dataset = sys.argv[1]
col = int(sys.argv[2])

csv = np.loadtxt(dataset, dtype=np.object, delimiter=",")

if col==0:
    x = csv[:,1:].astype(np.float)
    y = csv[:,0] 
else:
    x = csv[:,0:col].astype(np.float)
    y = csv[:,col] 

logreg = linear_model.LogisticRegression() #tol=0.00000001)

m = logreg.fit(x,y)

print m.get_params
