t = int(input('Enter Number of Test cases: '))
g = nx.erdos_renyi_graph(20, 0.5)

#g = nx.read_gml('/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/Modeling_cascading/main_graph.gml')

while(t):

    b = int(input('Enter PAYOFF for Initial bahaviour: '))
    a = int(input('Enter PAYOFF for New bahaviour: '))

    def col_fun(g):
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
        #Payoff(A) =a=5
        #Payoff(B) =b=2
        #a=5
        #b=2
       # a = int(input('Enter payoff for new bahaviour: '))
        #b = int(input('Enter payoff for initial bahaviour: '))
        for each in g.nodes():
            num_A = find_neigh(each, 'new_behaviour', g)
            num_B = find_neigh(each, 'initial_behaviour', g)
            payoff_A=a*num_A
            payoff_B=b*num_B
            if payoff_A >= payoff_B:
                dict1[each]='new_behaviour'
            else:
                dict1[each]= 'initial_behaviour'
        return dict1

    # def reset_node_attributes(g, behaviour_di):
    #     for each in behaviour_di:
    #         g.nodes[each]['behaviour']= behaviour_di[each]

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


    external_behaviour = "new_behaviour"
    bahaviour_1 = "initial_behaviour"
    for each in g.nodes():
        g.nodes[each]['behaviour'] = bahaviour_1

    #n1 = input()
    #n2 = input()
    #infected_seed = [2,5,8]
    infected_seed = []
    n= int(input("Enter number of inital seeds: "))

    for i in range(0,n):
        seed_val = int(input('Enter seed value: '))
        infected_seed.append(seed_val)

    print('Initial seed input is: ', infected_seed)




    for each in infected_seed:
        g.nodes[each]['behaviour'] = external_behaviour

    #colors = col_fun(g)

    #a = input("payoff for A: ")
    #b= input("payoff for B: ")
    temp =0
    count =0
    while(1):
        temp = terminate(g, count)
        if temp==1:
            break
        count = count+1
        behaviour_di  = recalculate_options(g)
        # reset_node_attributes(g, behaviour_di)
            # def reset_node_attributes(g, behaviour_di):
        for each in behaviour_di:
            g.nodes[each]['behaviour']= behaviour_di[each]
        colors = col_fun(g)

    c =terminate_1('new_behaviour', g)
    if c==1:
        print('cascade is COMPLETE ( with cascading size of: ', len(g),')')
    else:
        print('cascade is incomplete')
        for i in g.nodes():
            if(g.nodes[each]['behaviour'] == "new_behaviour"):
                count = count +1
            print(count)

    nx.draw(g,node_color= colors , node_size=1200,with_labels=True)
    plt.show()
    t=t-1
