from .model import OpinionNetwork, count_opinion
from mesa.visualization.modules import NetworkModule
from mesa.visualization.UserParam import UserSettableParameter
from mesa.visualization.modules import ChartModule
from mesa.visualization.modules import TextElement
from mesa.visualization.ModularVisualization import ModularServer

#This code is adapted from the "virus_on_network" example
def network_portrayal(G):
    def node_color(agent):
        return {0 : '#FF75D8', 1 : '#B4F8C8'}.get(agent.opinion, '#808080')

    def get_agents(source, target):
        return G.nodes[source]['agent'][0], G.nodes[target]['agent'][0]

    def degree(agent):
        return len(agent.model.grid.get_neighbors(agent.pos))

    portrayal = dict()
    portrayal['nodes'] = [
        {
            'size' : degree(agents[0]),
            'color' : node_color(agents[0]),
            'tooltip' : 'id: {}<br>opinion: {}<br>degree: {}'.format(
                agents[0].unique_id, agents[0].opinion, degree(agents[0])
            ),
        }
        for (_, agents) in G.nodes.data('agent')
    ]
    portrayal["edges"] = [
        {
            "source" : source,
            "target" : target,
            "color" : '#000000',
        }
        for (source, target) in G.edges
    ]

    return portrayal

network = NetworkModule(network_portrayal, 500, 500, library = 'd3')

chart = ChartModule([{'Label' : 'opinion_0', 'Color' : '#FF75D8'},
                     {'Label' : 'opinion_1', 'Color' : '#B4F8C8'}])

class MyTextElement(TextElement):
    def render(self, model):
        opinion_0 = count_opinion(model, 0)
        opinion_1 = count_opinion(model, 1)

        return 'Opinion 0 count: {}<br>Opinion 1 count: {}'.format(opinion_0, opinion_1)

model_params = {
    'N' : UserSettableParameter(
        'slider',
        'Number of agents',
        10,
        10,
        500,
        10,
        description = 'Choose how many agents to include in the model',
    ),
    'avg_node_degree' : UserSettableParameter(
        'slider',
        'Avg Node Degree',
        5,
        1,
        10,
        1,
        description = 'Avg Node Degree'
    ),
    'rule' : UserSettableParameter(
        'choice',
        'Rule Type',
        'CMR',
        choices = ['CMR', 'IMR'],
        description = 'The decision rule for updating'
    )
}

server = ModularServer(OpinionNetwork, [network, MyTextElement(), chart], 'Opinion Model', model_params)
server.port = 8521
