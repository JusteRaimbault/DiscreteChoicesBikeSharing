### Cloning_randomtimes_constant-pop.py

import random, math, pylab, heapq, numpy

#globals
#w=3
#k=3
#L=(2*k* w) + w
#print(numpy.matrix(([0]*10)*10))
#m[2][2]=random.random()
#print(m)[2][2]
#l=range(20)
#print(l[0:6:2])

# procedure to remove a element of index i from a heap, keeping the heap structure intact
def heapq_remove(heap, index):

    # Move slot to be removed to top of heap
    while index > 0:
        up = (index + 1) / 2 - 1
        heap[index] = heap[up]
        index = up
    # Remove top of heap and restore heap property
    heapq.heappop(heap)




# System state is described by parameter values
# includes also MSE and local var


#insert uniformely in phase list
def insert(list,element):
    index = random.randint(0,len(list))
    return list[0:index] + [element]+list[index:]


#param_bounds = []
#param_bounds.append([p1_min,p1_max])
#param_bounds.append([p2_min,p2_max])
#etc append all param values


# draw a random point in parameter space
def random_initial_state(param_bounds,param_steps):
    n=len(param_bounds)
    values=[]
    
    for i in range(param_bounds):
        mi=param_bounds[i][1]
        ma=param_bounds[i][2]
        #draw random param value
        p = mi + ((ma-mi) * random.random())
        values.append(p)
    return values        

    h=[]
    v=[]
    ph=[]
    L=(2*k* w) + w
    for i in range(w):
        h.append(([0]*L))
    for i in range(L):
        v.append(([0]*w))
    #fill randomly
    for i in range(len(h)):
        for j in range(len(h[0])):
            if random.random()<initial_load:h[i][j]=1;ph=insert(ph,[i,j,0])
    for i in range(len(v)):
        for j in range(len(v[0])):
            if i >= k*w and i < (k+1)*w:
                if random.random()<initial_load and h[ ((k+1) * w) -i-1][j + (k*w) ]!=1:v[i][j]=1;ph=insert(ph,[i,j,1])
            else:
                if random.random()<initial_load:v[i][j]=1;ph=insert(ph,[i,j,1])
    return [h,v,ph]

#print(random_initial_state(2,2,0.2))

def next_state(state, alpha):
    h = state[0]
    v = state[1]
    L=len(v)
    w=len(h)
    k=(L-w)/2
    phases = state[2]
    next_phases = phases
    to_remove=[]
    activity_increase = 0
    #first we make all particle try to advance
    i=0
    for p in phases:
    #horizontal
        if p[2]==0:
 #if at the end, dies !
            if p[1]==L-1:
                to_remove.append(p)
                h[p[0]][p[1]]=0
            else:
                #check if nobody in front ! -> beware because also need to look at the other matrix
                free= h[p[0]][p[1]+1] == 0
                if p[1]+1 >= k*w and p[1]+1 < (k+1)*w:free = free and (v[ ((k+1) * w)-p[0]-1][p[1]+1 - (k*w) ]==0)
                if free :
                   h[p[0]][p[1]]=0
                   h[p[0]][p[1]+1]=1
                   next_phases[i]=[p[0],p[1]+1,0]
                   activity_increase+=1
        else:
            if p[0]==L-1:
               to_remove.append(p)
               v[p[0]][p[1]]=0
            else:
                free=  v[p[0]+1][p[1]] == 0
                if p[0]+1 >= k*w and p[0]+1 < (k+1)*w:free = free and (h[ ((k+1) * w)-p[0]-2][p[1] - (k*w) ]==0)
                if free:
                   v[p[0]][p[1]]=0
                   v[p[0]+1][p[1]]=1
                   next_phases[i]=[p[0]+1,p[1],1]
                   activity_increase+=1
        i+=1
    for r in to_remove:
        next_phases.remove(r)
#
#    #insert particles
#
    for i in range(w):
        if random.random()<alpha:
            h[i][0]=1
            next_phases=insert(next_phases,[i,0,0])
        if random.random()<alpha:
            v[0][i]=1
            next_phases=insert(next_phases,[0,i,1])

    return [[h,v,next_phases],activity_increase]


#test
def test(initial,alpha):
    state=random_initial_state(10,5,initial)
    ktot=0
    for t in range(10000):
        for s in state[0]:print(s)
        print("\n")
        (s,k)=next_state(state,alpha)
        state=s;ktot+=k
        print 'Total Activity : ', ktot

#test(0.1,0.3)


#
# s-modified escape rate
#
#we are still conjugated with activity, so exp(-s) also
#excape rate of one situation? \sum W(C'->C)
# antecedents of deterministic transfert function ? 2^w states from the end
#we jump from these at rate alpha for each new particle in beginning line
#so rate is 2^w  * alpha
def sescape(w,alpha,s):
    esc=(pow(2.,w+1) * alpha)*math.exp(-s)
    return esc

# s-dependent cloning rate
#what is the cloning rate in our case ?
#e^s -1*escape
def cloningrate(w,alpha,s):  # always positive, as we took s<0
    rate=(pow(2.,w+1) * alpha)*(math.exp(-s)-1.)
    return rate

def n_rand(state):
    h=state[0]
    v=state[1]
    res=0.
    for i in range(len(h)):
        if h[i][0]==1:res+=1.
        if v[0][i]==1:res+=1.
    return res



def cloning(w,alpha,s,tmax):
    # heapq provides the Heap Queue structure which allows to manipulate efficiently sorted list.
    # Here we use a list of copies of the system, sorted according to their next time of evolution.
    t=0.            # initial and maximal time
    popsize=400     # population size -> fixed pop size !
    populat=[]
                # initial state of the population : 
                # each member of (or "copy" in) the population is described by a 3-uple (time,dt,state) 
                #   time  = next time at which it will evolve
                #   dt = time since last evolution
                #   state = 0 or 1 = empty or occupied 
    tmin=tmax/2     # measures start at tmin
    step=0          # counter for the number of steps in the "mutation/selection" process



    # lists to sample the logarithm of the cloning ratios Y a function of time
    samplestime,samplesY,samplesYint=[],[],[]
    Y,Yint=0.,0.
    nmean=0
    kmean=0

    # initialization of the population
    #generate a random configuration !
    #lane length will be always fixed as 10*w to simplify
    #initial load is alpha because of independance of events
    populat=[ (0.,0.,random_initial_state(w,10,alpha),0.) for count in range(popsize)]
    heapq.heapify(populat)  # orders the population into a Heap Queue


    #while t<tmax:
    #try with fixed number of iteration
    #(corresponds to fixed final pop?)
    it_max=10000
    for it in range(it_max):
        #print 't = ', t
        if it % (it_max / 100) == 0:print it
        # we pop the first element of populat, which is always the next to evolve
        (t,dt,state,k)=heapq.heappop(populat) 
        # the copy we poped out is to be replaced by p copies ; random.random() is uniform on [0,1[
        cloningfactor= math.exp(dt*cloningrate(w,alpha,s))
        p=int( cloningfactor + random.random() ) 

        if p==0:    # one copy chosen at random replaces the current copy
            toclone=random.choice(populat)
            heapq.heappush(populat,toclone)
        elif p==1:  # the current copy is evolved without cloning
            (nextstate,nextk) = next_state(state,alpha)
            Deltat=random.expovariate(sescape(w,alpha,s))
            toclone=(t+Deltat,Deltat,nextstate,k+nextk)
            heapq.heappush(populat,toclone)
        else: # p>1 : make p clones ; population size becomes N+p-1 ; remove p-1 clones uniformly 
            pcount=p
            while pcount>0:
                pcount-=1
                (nextstate,nextk) = next_state(state,alpha)
                Deltat=random.expovariate(sescape(w,alpha,s)) # interval until next evolution
                toclone=(t+Deltat,Deltat,nextstate,k+nextk)
                heapq.heappush(populat,toclone)
            # we first chose uniformly the p-1 distinct indices to remove, among the N+p-1 indices 
            listsize=popsize+p-1;indices=random.sample(xrange(listsize),p-1) 
            # the list of indices to remove is sorted from largest to smallest, so as to remove largest indices first
            indices.sort(reverse=True)
            for i in indices:
                heapq_remove(populat,i)

        if t>tmin:
            Yint+=math.log((popsize+p-1.)/(1.*popsize))
            Y   +=math.log((popsize+cloningfactor-1.)/(1.*popsize))
        if step%5 == 0: 
            samplestime.append(t)
            samplesY.append(Y)
            samplesYint.append(Yint)
        step+=1

    # Bulk numerical estimation of psiK(s) from population size ~ e^(t psiK(s) )
    psiK=Y/(t-tmin)
    psiKint=Yint/(t-tmin)

    # better estimation by fitting log(popsize(t)) starting from a given threshold so as to isolate the large-time 
    # exponential behaviour popsize(t) ~ e^(t psiK(s) )

    psiKintfit,const = pylab.polyfit(samplestime,samplesYint,1)
    psiKfit,const    = pylab.polyfit(samplestime,samplesY,   1)

    print 's = ',s
    print '      bulk numerical psi(s) = ', psiK
    print '(int) bulk numerical psi(s) = ', psiKint
    print 'fitted numerical fit psi(s) = ', psiKfit
    print '(int) ---------- fit psi(s) = ', psiKintfit

    #pylab.plot(samplestime,samplesY, 'r')
    #pylab.plot(samplestime,samplesYint, 'b')
    #pylab.plot(samplestime,[const+psiKfit*samplestime[i] for i in range(len(samplestime)) ], 'g-')
    #pylab.show()

    nn = len(populat)
    for i in range(nn):
        (t,dt,state,k)=heapq.heappop(populat)
        kmean += k / t
    kmean = kmean / nn

    print 'Mean activity <K>s = ', kmean

    return (psiK,psiKfit,kmean)



##############
# main part
##############

#Q of the scale of t ? with our system, very small cloning time, so small t !
#beware, both alpha and t_max have to be functions of w, since D_t of evolution get significantly
#smaller when w increase (ex from 15 to 20, divided by 100!)
#
# concerning alpha, we extract it from empirical results ?
# 


def main():
    #w=10
    alpha=0.195
    #-> for size = 10 !
    x = numpy.arange(0,0.5,0.05)
    
    psiKfit = []
    m = []
    w=10
    for w in numpy.arange(10,20,2):
        k = []
        tmax=0.05 / (w^2)
        psiK = []

        for s in x:
            print 'Running cloning algorithm...'
            print '...size of the system w = ',w
            print '...injection rate alpha = ',alpha
            print '...conjugated var s = = ',s
            #print '...max time tmax = ',tmax

            (psK,psKfit,kmean) = cloning(w,alpha,s,tmax)
            psiK.append(psK)
            psiKfit.append(psKfit)
            k.append(kmean)


        print(psiK)
        print 'k= ',k
    #pylab.plot(x,c*(math.exp(-2.*x)-1.))
        #pylab.plot(x,psiK)
        pylab.plot(x,k)
    pylab.show()

    #mean of n should be c(s)
    #pylab.plot(x,c*math.exp(-1.*x))
    #pylab.plot(x,nmean)

    #for r_s-r
    #pylab.plot(x,c*(math.exp(-2.*x)-1))
    #pylab.plot(x,(math.exp(-2.*x)-1)*(c + nmean))

    #derivative of psi
    #pylab.plot(x,2*c*(math.exp(-2.*x)))
    #



main()
