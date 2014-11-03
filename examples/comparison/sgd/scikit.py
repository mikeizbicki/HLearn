#!/usr/bin/python

import sys
import numpy as np
from sklearn import cross_validation
from sklearn.linear_model import SGDClassifier

# load data

dataset = sys.argv[1]
col = int(sys.argv[2])
csv = np.loadtxt(dataset, dtype=np.object, delimiter=",")

if col==0:
    x = csv[:,1:].astype(np.float)
    y = csv[:,0] 
else:
    x = csv[:,0:col].astype(np.float)
    y = csv[:,col] 


# set model hyperparsms

clf = SGDClassifier(loss="log", penalty="none",n_iter=10000,alpha=0.0001,shuffle=False)
print clf
#m = clf.fit(x, y)
#print m.get_params

# cross-validate

scores = cross_validation.cross_val_score(clf, x, y, cv=5, scoring='accuracy')
print "scores =", scores
print "  mean   =", np.mean(scores)
print "  stddev =", np.std(scores)

# print results

