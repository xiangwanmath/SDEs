#!/usr/bin/env python
# coding: utf-8

# In[91]:


import random 
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import time

def oneDimRandomWalk(discreteTime):
    rWalk = pd.DataFrame([(0,0)])
    #rWalk = rWalk.rename_axis(mapper="t",axis=0)
    rWalk = rWalk.rename({0: 't', 1: 'x'}, axis=1)
    #print(rWalk)
    t=0
    x = 0
    for i in range(discreteTime):
        dx = random.choice([1,-1])
        t+=1
        x+=dx
        #y+=dy
        rWalk.loc[len(rWalk.index)] = [t, x]
        #rWalk = rWalk.append(pd.concat(pd.DataFrame([t,x,y])))
    return(rWalk)

#data = oneDimRandomWalk(100000)
#plt.step(data.loc[:,"t"],data.loc[:,"x"])
#plt.show



def oneDimAverageWalk(n,steps):
    base = oneDimRandomWalk(steps)
    base["sumX"] = base['x']
    for i in range(n-1):
        tempRWalk = oneDimRandomWalk(steps)
        base["sumX"] += tempRWalk['x']
#         print(base)
#         print(tempRWalk)
    base["sumX"] = base["sumX"]/n
    plt.step(base.loc[:,"t"],base.loc[:,"sumX"])
    plt.show
    
oneDimAverageWalk(5000,100)


# In[80]:


def twoDimRandomWalk(discreteTime):
    rWalk = pd.DataFrame([(0,0,0)])
    #rWalk = rWalk.rename_axis(mapper="t",axis=0)
    rWalk = rWalk.rename({0: 't', 1: 'x',2:'y'}, axis=1)
    #print(rWalk)
    t=0
    x, y = 0,0 
    for i in range(discreteTime):
        (dx,dy) = random.choice([(1,0),(0,1),(-1,0),(0,-1)])
        t+=1
        x+=dx
        y+=dy
        rWalk.loc[len(rWalk.index)] = [t, x, y]
        #rWalk = rWalk.append(pd.concat(pd.DataFrame([t,x,y])))
    return(rWalk)

# data = twoDimRandomWalk(1000)
# plt.step(data.loc[:,"x"],data.loc[:,"y"])
# plt.show

def averageWalk(n,steps):
    base = twoDimRandomWalk(steps)
    base["sumX"] = base['x']
    base["sumY"] = base['y']
    for i in range(n-1):
        tempRWalk = twoDimRandomWalk(steps)
        base["sumX"] += tempRWalk['x']
#         print(base)
#         print(tempRWalk)
        base["sumY"] += tempRWalk['y']
    base["sumX"] = base["sumX"]/n
    base["sumY"] = base["sumY"]/n
    plt.step(base.loc[:,"sumX"],base.loc[:,"sumY"])
    plt.show
    
averageWalk(10000,100)


# In[75]:


twoDimRandomWalk(1000)


# In[76]:


data = twoDimRandomWalk(1000)
plt.step(data.loc[:,"x"],data.loc[:,"y"])
plt.show


# In[ ]:




