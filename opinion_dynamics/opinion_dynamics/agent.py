from collections import Counter

from mesa import Agent

class OpinionAgent(Agent):
    def __init__(self, unique_id, model, initial_opinion):
        super().__init__(unique_id, model)
        self.opinion = initial_opinion

    def classic_majority_rule(self):
        if len(self.model.grid.get_neighbors(self.pos)) == 0: #no neighbors, so opinion doesn't change
            pass
        else: #the agent will change it's opinion via a simple majority rule
            neighbors = [self.model.grid.get_cell_list_contents([node])[0] for node in self.model.grid.get_neighbors(self.pos)] #get all neighboring agents
            neighbor_opinions = Counter([a.opinion for a in neighbors]) #count up neighboring opinions
            self.opinion = neighbor_opinions.most_common(1)[0][0] #ties seem to be broken semi-arbitrarily by Counter

    def influence_majority_rule(self):
        if len(self.model.grid.get_neighbors(self.pos)) == 0: #no neighbors, so no need to change opinion
            pass
        else:
            neighbors = [self.model.grid.get_cell_list_contents([node])[0] for node in self.model.grid.get_neighbors(self.pos)]
            neighbor_weights = [len(self.model.grid.get_neighbors(a.pos)) for a in neighbors] #get the degree of each neighbor

            #performing the weighted sum
            opinion_0_ct = 0
            opinion_1_ct = 0
            for i, a in enumerate(neighbors):
                if a.opinion == 0:
                    opinion_0_ct += neighbor_weights[i] #add to running count, scaled by the influence of that neighbor
                else:
                    opinion_1_ct += neighbor_weights[i]

            #assume the most popular WEIGHTED opinion
            if opinion_0_ct > opinion_1_ct:
                self.opinion = 0
            else:
                self.opinion = 1

    def step(self):
        if self.model.rule == 'CMR':
            self.classic_majority_rule() #updating opinion by classic majority rule
        elif self.model.rule == 'IMR':
            self.influence_majority_rule() #updating opinion by influence majority rule

    #For exporting the graph for visualization in R
    def __str__(self):
        return str(self.opinion)
