package TA

class TA {

}

class TimedAutomaton:

def __init__(self):
# map state labels to states
self.states = {}

# map states to transition in adjacency list form
self.adj = {}

def __str__(self):
return str(self.states.values()) + "\n" + str(self.adj.items()) + "\n"

def addState(self, label, inv = None):
s = State(label, inv)
self.states[label] = s

def addTransition(self, source, dest, guard = None):
ts = Transition(source, dest, guard)
if self.adj.get(source) is None:
self.adj[source] = []
self.adj[source].append(ts)

def setInit(self, init):
self.init = init
self.states[init].reset = 0

def toTCN(self):
tcn = TCN()

# add states and abtract out invariants
for label, state in self.states.items():
stateB = label + "-b"
stateE = label + "-e"

tcn.addEvent(stateB)
tcn.addEvent(stateE)
if(state.inv is not None):
tcn.addEdge(stateB, stateE, state.inv.toConstraint(), True)
else:
tcn.addEdge(stateB, stateE, Constraint(0, Constraint.inf), True)

# set initial state
tcn.setInit(self.init + "-b")

# add edges with guards->constraints
for origin in self.adj.values():
for transition in origin:
source = transition.source + "-e"
dest = transition.dest + "-b"

if transition.guard is not None:
tcn.addEdge(source, dest, transition.guard.toConstraint(), True)
else:
tcn.addEdge(source, dest, Constraint(0, 0), True)

# perform topological sorting of events
sort = tcn.topSort()

# gather the clock resets in topological order
clockResets = []
currentReset = (tcn.init,0)

for event in sort:
for label, state in self.states.items():
if label + "-b" == event and state.reset is not None:
currentReset = (event, state.reset)
clockResets.append(currentReset)

# replace constrained edges with adjusted constrained edges

for event, (resetState, reset) in zip(sort, clockResets):
for edge in tcn.events[event]:
if not edge.constr.isTrivialConstraint():
tcn.removeEdge(edge.source, edge.dest)
tcn.addEdge(resetState, edge.dest, edge.constr.getTimeAdjustedConstraint(reset), resetState == edge.source)

# this step needs to be added to algo description

#check if the two
if not resetState == edge.source:
if edge.source[:-2] == edge.dest[:-2]:
tcn.addEdge(edge.source, edge.dest, Constraint(0, Constraint.inf), True)
else:
tcn.addEdge(edge.source, edge.dest, Constraint(0, 0), True)

return tcn