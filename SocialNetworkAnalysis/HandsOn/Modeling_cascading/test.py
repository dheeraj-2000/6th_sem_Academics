#import igraph
#from igraph import *
import matplotlib.pyplot as plt
import networkx as nx
t = int(input('Enter Number of Test cases: '))
#g = nx.erdos_renyi_graph(20, 0.5)

#g = nx.read_gml('/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/Modeling_cascading/main_graph.gml')
g = nx.Graph()

g.add_edges_from([("1","2"), ("1","3"), ("2", "3"), ("2","6"), ("6","4"), ("6", "9"), ("4","5"), ("4","7"), ("5", "7"), ("5", "8"), ("8", "7"), ("8", "10"), ("8", "14"), ("9", "7"), ("9", "10"), ("9", "11"), ("7", "10"), ("10", "12"), ("11", "12"), ("11", "15"), ("12", "15"), ("12", "16"), ("13", "12"), ("13", "16"), ("13", "14"), ("13", "17"), ("14", "17"), ("17", "16"), ("15","16")])
nx.draw(g,node_size=1200, with_labels=True)
plt.show()

while(t):

    b = int(input('Enter PAYOFF for Initial bahaviour: '))
    a = int(input('Enter PAYOFF for New bahaviour: '))

    def col_list(g):
        infected_seed=[]
        for each in g.nodes():
            if g.nodes[each]['behaviour']=='initial_behaviour':
                infected_seed.append('yellow')
            else:
                infected_seed.append('green')
        return infected_seed

    def find_neigh(each, c, g):
        num=0
        for each1 in g.neighbors(each):
            if g.nodes[each1]['behaviour']==c:
                num=num+1
        return num

    def recalculate_options(g):
        dict1= {}
        #Payoff(A) =a=4
        #Payoff(B) =b=3
        #a=5
        #b=2

       # a = int(input('Enter payoff for new bahaviour: '))
        #b = int(input('Enter payoff for initial bahaviour: '))
        for each in g.nodes():
            num_A = 0
            num_A = find_neigh(each, 'new_behaviour', g)
            num_B = find_neigh(each, 'initial_behaviour', g)
            # for each1 in g.neighbors(each):
            #     if g.nodes[each1]['behaviour']=='new_behaviour':
            #         num_A=num_A+1
            # return num_A
            #
            # for each1 in g.neighbors(each):
            #     num_B = 0
            #     if g.nodes[each1]['behaviour']=='initial_behaviour':
            #         num_B=num_B+1
            # return num_B

            payoff_A=a*num_A
            payoff_B=b*num_B
            if payoff_A >= payoff_B:
                dict1[each]='new_behaviour'
            else:
                dict1[each]= 'initial_behaviour'
        return dict1

    def reset_node_attributes(g, behaviour_di):
        for each in behaviour_di:
            g.nodes[each]['behaviour']= behaviour_di[each]

    def terminate_1(c, g):
        f=1
        for each in g.nodes():
            if g.nodes[each]['behaviour']!=c:
                f=0
                break
        return f

    def terminate(g, count):
        flag1=terminate_1('new_behaviour', g)
        flag2=terminate_1('initial_behaviour',g)
        if flag1==1 or flag2==1 or count>=100:
            return 1
        else:
            return 0




    for u in g.nodes():
        for v in g.nodes():
            if u<v:
                print(u,v, ':')
                infected_seed = []
                infected_seed.append(u)
                infected_seed.append(v)


                external_behaviour = "new_behaviour"
                bahaviour_1 = "initial_behaviour"
                for each in g.nodes():
                    g.nodes[each]['behaviour'] = bahaviour_1


                for each in infected_seed:
                    g.nodes[each]['behaviour'] = external_behaviour

                temp =0
                count =0
                while(1):
                    temp = terminate(g, count)
                    if temp==1:
                        break
                    count = count+1
                    behaviour_di  = recalculate_options(g)
                    reset_node_attributes(g, behaviour_di)
                    colors = col_list(g)

                c =terminate_1('new_behaviour', g)
                if c==1:
                    print('cascade complete with size: ', len(g))
                else:
                    print('cascade incomplete')
#                     count =0
#                     for i in g.nodes():
#                         if(g.nodes[each]['behaviour'] == "new_behaviour"):
#                             count = count +1
#                     print(count)
