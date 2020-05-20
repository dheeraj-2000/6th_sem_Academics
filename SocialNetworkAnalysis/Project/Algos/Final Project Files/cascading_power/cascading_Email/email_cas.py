import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


df=pd.read_csv("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/Final Project Files/cascading_power/cascading_Email/emailcascade.csv",encoding = "ISO-8859-1")



df["meanplussd"] = df["pow"] + df["std"]
df["meanminusssd"] = df["pow"] - df["std"]

x = df['s']
y1 = df['meanplussd']
y2 = df['meanminusssd']
z = df['pow']


# fig = plt.figure(figsize=(10, 8))

plt.plot(x,z, color = "black", label = "mean line")
plt.plot(x,y1, color = "red", label = "for standard deviation")
plt.plot(x,y2, color = "red")
plt.fill_between(x, y1, y2, where=None,color = "yellow", interpolate=False, step=None, data=None, alpha='0.3')
plt.xlabel('Shell Number')
plt.ylabel('Cascading Power')
plt.title('Cascading power in Email University Network')
plt.legend()
plt.show()
# plt.savefig('cascading_haster.png', dpi=800)
