import sys
import numpy as np

import cplex
from cplex.callbacks import MIPInfoCallback


import csv


class TimeLimitCallback(MIPInfoCallback):

    def __call__(self):
        if not self.aborted and self.has_incumbent():
            gap = 100.0 * self.get_MIP_relative_gap()
            timeused = self.get_time() - self.starttime
            if timeused > self.timelimit and gap < self.acceptablegap:
                print("Good enough solution at", timeused, "sec., gap =",
                      gap, "%, quitting.")
                self.aborted = True
                self.abort()

def write_file(filename, array, two_d=True):
   with open(filename, mode='w') as file:
       file_writer = csv.writer(file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
       if (two_d):
           for row in array:
               #print("----------",row,"------------")
               file_writer.writerow(row)
       else:
           file_writer.writerow(array)

def read_file(filename, two_d=True):
    """
    return a list containing the file's data
    expects the first row to be (useless) header data

    each row is stored as an array, an entire file is stored
    as a 2-d array
    """
    ret = []
    with open(filename) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=",")
        first=True
        if two_d:
            for row in csv_reader:
                if first:
                    # print(f'Column names are {", ".join(row)}')
                    first=False
                else:
                    f_row = []
                    for e in row:
                        f_row.append(float(e))
                    ret.append(f_row)
        else:
            for row in csv_reader:
                if first:
                    # print(f'Column names are {", ".join(row)}')
                    first=False
                else:
                    for e in row:
                        ret.append(float(e))
    return ret

def extract_variables(variables):
    obj = [];ub = [];lb = [];colnames = [];types = [];
    for v in variables:
        obj      += v["obj"]
        ub       += v["ub"]
        lb       += v["lb"]
        colnames += v["name"]
        types    += v["type"]
    return obj, ub, lb, colnames, types

def extract_constraints(constraints):
    rows = []; senses = []; rhs = [];
    for c in constraints:
        rows   += c["lin_expr"]
        senses += c["senses"]
        rhs    += c["rhs"]
        range_values += c["range_values"]
    return rows, senses, rhs

def read_and_partition():
    """Solve (un)capacitated facility location problem."""
    # Read in the csv files. The data we read is
    # fixedcost  -- a list/array of facility fixed cost
    # cost       -- a matrix for the costs to serve each client by each
    #               facility
    print("read_and_partition")
    cost_c = read_file("connect_cost.csv")
    cost_f = read_file("facility_cost.csv", two_d=False)
    capacity = read_file("facility_capacity.csv", two_d=False)
    num_customers = read_file("customer_size.csv", two_d=False)
    sizes = 3
    parition(cost_c, cost_f, capacity, num_customers, sizes)


def partition(cost_c, cost_f, capacity, customers, sizes, sol_lim=10, time_lim=3*60*60, optim_lim=0.1, emphasis=0):
    print('Partitioning the network...', flush=True)

    num_f = len(cost_f)
    num_c = len(cost_c)
    num_l = int(num_f // sizes)

    I = range(num_c)
    J = range(num_f)
    K = range(int(sizes))
    L = range(num_l)

    print('I =', I)
    print('J =', J)
    print('K =', K)
    print('L =', L)

    # Create the modeler/solver.
    cpx = cplex.Cplex()
    cpx.objective.set_sense(cpx.objective.sense.minimize)

    # if not feas:
    #     cpx.parameters.preprocessing.relax.set(0)
    #cpx.parameters.preprocessing.presolve.set(0)

    cpx.parameters.mip.limits.solutions.set(sol_lim)
    # cpx.parameters.timelimit.set(time_lim)
    # cpx.parameters.simplex.tolerances.optimality.set(optim_lim)
    # cpx.parameters.preprocessing.symmetry=2

    cpx.parameters.emphasis.mip.set(emphasis)

    # register our timer
    timelim_cb = cpx.register_callback(TimeLimitCallback)
    timelim_cb.starttime = cpx.get_time()
    timelim_cb.timelimit = time_lim
    timelim_cb.acceptablegap = optim_lim
    timelim_cb.aborted = False


    # Create variables. We have variables
    # F[j]        if facility j is open.
    # C[i][j]]    if a client i is located to a location j

    ############################################################################
    # Objective function

    def F_name(j):
        return "F_"+str(j)
    def C_name(i,j):
        return "C_"+str(i)+"_"+str(j)

    cpx.variables.add(
        names = [C_name(i,j) for j in J for i in I],
        obj   = [cost_c[i][j] for j in J for i in I],
        types = ["B" for j in J for i in I],
        ub    = [1 for j in J for i in I],
        lb    = [0 for j in J for i in I]
    )
    cpx.variables.add(
        names = [F_name(j) for j in J],
        obj   = [cost_f[j] for j in J],
        types = ["B" for j in J],
        ub    = [1 for j in J],
        lb    = [0 for j in J]
    )

    #print(variables)
    ############################################################################
    # constraints

    # every customer must be connected to one store
    # for i in I: sum(C_i_j) >= 1
    # print([[[C_name(i,j) for j in J], [1 for j in J]] for i in I])
    cpx.linear_constraints.add(
        names    = ['c:'+C_name(i,0) for i in I],
        lin_expr = [[[C_name(i,j) for j in J], [1 for j in J]] for i in I],
        senses   = ["G" for i in I],
        rhs      = [1 for i in I]
    )
    # every customer must be connected to an open store
    # F_j - C_i_j >= 0
    # print([[[C_name(i,j),F_name(j)], [-1,1]] for i in I for j in J])
    cpx.linear_constraints.add(
        names    = [C_name(i,j) + F_name(j) for i in I for j in J],
        lin_expr = [[[C_name(i,j),F_name(j)], [-1,1]] for i in I for j in J],
        senses   = ["G" for i in I for j in J],
        rhs      = [0 for i in I for j in J],
    )

    ## every store with < k1 customers is subject to cost i1
    ## stores ares stored such that all the sizes are together,
    ## eg [s, s, s, m, m, m, l, l, l]
    ## stores can be access via (k+1)*l
    # print(K)
    # print(L)

    # cannot have collocated stores
    # f1 + f2 + f3 <= 1
    #print([[[F_name(l+(k*num_l)) for k in K], [1 for k in K]] for l in L])
    cpx.linear_constraints.add(
        names    = ["col" + F_name(l) for l in L],
        lin_expr = [[[F_name(l+(k*num_l)) for k in K], [1 for k in K]] for l in L],
        senses   = ["L" for l in L],
        rhs      = [1 for l in L],
    )

    # ## all stores must respect their capacity
    # num customers connected - store cacapicty <= 0
    # print([[[C_name(i,j) for i in I] + [F_name(j)], [customers[i] for i in I] + [-capacity[j]]] for j in J])
    cpx.linear_constraints.add(
        names    = ["cap" + F_name(j) for j in J],
        lin_expr = [[[C_name(i,j) for i in I] + [F_name(j)], [customers[i] for i in I] + [-capacity[j]]] for j in J],
        senses   = ["L" for j in J],
        rhs      = [0 for j in J],
    )
    #print(constraints)
    ############################################################################
    # save and solve
    #cpx.write("model.lp")

    cpx.solve()

    try:
        solution = cpx.solution.get_values()
    except:
        print("--------------------------------------------")
        print("No solution exists!! Attempting optimization")
        print("--------------------------------------------", flush=True)
        cpx.feasopt(cpx.feasopt.all_constraints())
        solution = cpx.solution.get_values()

    # print("---------------")
    #print(solution)
    I = len(I);
    J = len(J);
    #write_file("solution.csv", solution, two_d=False)
    connect = np.reshape(solution[0:I*J],(I,J), order='F')
    open    = solution[I*J:I*J+J]

    #write_file("connect_sol.csv", connect)
    #write_file("open_sol.csv", open, two_d = False)

    print("-------------Solution Found-------------", flush=True)
    #cpx.solution.write("model.sol")

    cost = 0

    I = range(I)
    J = range(J)

    for j in J:
        if open[j] > 0:
            cost += cost_f[j]
            # print("Facility {0} is open, it serves customers {1}".format(
            #     j, " ".join([str(i) for i in I if connect[i][j] > 0])))
            for i in I:
                if connect[i][j] > 0:
                    cost += cost_c[i][j]

    #print(open)
    ret = dict()
    ret['open'] = open
    ret['connect'] = connect
    ret['cost'] = cost
    return ret
