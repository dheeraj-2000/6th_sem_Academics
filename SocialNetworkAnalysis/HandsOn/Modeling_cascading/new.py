import igraph
from igraph import *
import matplotlib.pyplot as plt
import networkx as nx

def set_all_B(g):
    for each in g.nodes():
        g.nodes[each]['action'] = 'B'

def set_A(g, list1):
    for each in list1:
        g.nodes[each]['action'] ='A'

def get_colors(g):
    list1=[]
    for each in g.nodes():
        if g.nodes[each]['action']=='B':
            list1.append('red')
        else:
            list1.append('green')
    return list1


g = nx.read_gml('/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/Modeling_cascading/main_graph.gml')

set_all_B(g)

list1=[3,7]

set_A(g,list1)

colors= get_colors(g)

nx.draw(g, node_color = colors, node_size=800)
plt.show()
