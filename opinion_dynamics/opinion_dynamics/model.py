import networkx as nx

from mesa import Model
from mesa.time import RandomActivation
from mesa.space import NetworkGrid
from mesa.datacollection import DataCollector
from .agent import OpinionAgent

def count_opinion(model, opinion):
    return sum([1 for a in model.schedule.agents if a.opinion == opinion])

def opinion_0_ct(model):
    return count_opinion(model, 0)

def opinion_1_ct(model):
    return count_opinion(model, 1)

class OpinionNetwork(Model):
    '''An opinion model with N agents on an Erdos-Renyi network.'''

    def __init__(self, N, avg_node_degree, rule = 'CMR', zero_prob = 0.5):
        self.num_agents = N
        self.rule = rule
        self.schedule = RandomActivation(self)
        prob = avg_node_degree / self.num_agents
        self.G = nx.erdos_renyi_graph(n = self.num_agents, p = prob)
        self.grid = NetworkGrid(self.G)

        #create the agents
        for i, node in enumerate(self.G.nodes()):
            a = OpinionAgent(i, self, initial_opinion = self.random.choices([0, 1], weights = [zero_prob, 1 - zero_prob], k = 1)[0]) #we can seed w/a non-equal probability
            self.schedule.add(a)
            self.grid.place_agent(a, node)

        self.datacollector = DataCollector(model_reporters = {'opinion_0' : opinion_0_ct, 'opinion_1' : opinion_1_ct})

        self.running = True #we could check to see if we've reached a steady state
        self.datacollector.collect(self)

    def step(self):
        self.schedule.step() #each agent gets a step
        self.datacollector.collect(self) #collect data
