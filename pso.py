import numpy as np
import scipy as sp
import math as ma

def sphere(x):
    # definition of cost functional
    c = ((x-1)**2).sum(axis=0)
    return c

def rosenbrock2(x):
    # definition of cost functional
    xx = x[0,] # I need to know the date type of x
    yy = x[1,]
    a  = 2.
    c  = (a-xx)**2 + 100.*(yy-xx**2)**2
    return c

def rosenbrock3(x):
    # definition of cost functional
    xx = x[0,]
    yy = x[1,]
    zz = x[2,]
    c  = (1.-xx)**2 + (1.-yy)**2 + \
         100.*(yy-xx**2)**2 + 100.*(zz-yy**2)**2
    return c

def rastrigin2(x):
    # definition of cost functional
    xx = x[0,]
    yy = x[1,]
    a  = 10.
    c  = 2*a + xx**2 - a*np.cos(2*ma.pi*xx) + \
         yy**2 - a*np.cos(2*ma.pi*yy)
    return c

def rastrigin3(x):
    # definition of cost functional
    xx = x[0,]
    yy = x[1,]
    zz = x[2,]
    a = 10.
    c  = 3*a + xx**2 + yy**2 + zz**2 - \
         a*np.cos(2*ma.pi*xx) - a*np.cos(2*ma.pi*yy) - \
         a*np.cos(2*ma.pi*zz)
    return c

if __name__ == '__main__':

    #------ particle swarm optimization (PSO)

    # number of variables and swarm size
    nvar   = 2
    nswarm = 30

    # bounds on the variable x
    xMin = -5
    xMax =  5

    # bounds on the velocity
    vMax = 0.1*(xMax-xMin)
    vMin = -vMax

    # maximum number of iterations
    itMax = 5000
    # inertia and inertia damping
    w = 1
    wdamp = 0.99
    # acceleration coefficients
    c1 = 2
    c2 = 2

    # initialization
    x     = np.random.uniform(xMin,xMax,(nvar,nswarm))
    v     = np.zeros((nvar,nswarm),float)
    c     = rosenbrock2(x)
    p     = x.copy()
    g, ig = c.min(0),c.argmin(0)
    gx    = x[:,ig]

    # PSO main loop
    for i in range(1,itMax+1):

        # inertia damping
        w = w*wdamp

        for j in range(0,nswarm):

            # random weights for cognitive and social behavior
            Rcog = np.random.uniform(0,1,(1,nvar))
            Rsoc = np.random.uniform(0,1,(1,nvar))

            # update velocity (inertia + cognitive + social)
            v[:,j] = w*v[:,j] + c1*Rcog*(p[:,j] - x[:,j]) + c2*Rsoc*(gx - x[:,j])

            # apply velocity limits
            v[:,j] = np.clip(v[:,j],vMin,vMax)

            # update position
            x[:,j] += v[:,j]

            # velocity mirroring
            imirr      = ( (x[:,j] < xMin).nonzero() or (x[:,j] > xMax).nonzero() )
            v[imirr,j] = -v[imirr,j]

            # apply variable limits
            x[:,j] = np.clip(x[:,j],xMin,xMax)

            # evaluation of best cost
            cc  = rosenbrock2(x[:,j])
            ccp = rosenbrock2(p[:,j])

            # update particle-best and globally best solution
            if cc < ccp:
                # update particle-best solution
                p[:,j] = (x[:,j]).copy()
                if cc < g:
                    # update globally best solution
                    g  = cc.copy()
                    gx = (x[:,j]).copy()
                    print(g)
