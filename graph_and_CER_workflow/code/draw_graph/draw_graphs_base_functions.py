
"""


"""

from datetime import timedelta
from pathlib import Path
import sys
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from collections import defaultdict
from networkx.drawing.nx_agraph import to_agraph
from itertools import combinations
from networkx.drawing.nx_agraph import write_dot
import pygraphviz as pgv

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *


EDGETYPE_TO_COLOR = {
    "GeneticDistance" : "#595959",
    "SurvnetKontaktperson" : "#66C2A5",
    "SurvnetAngestecktBei" : "#FC8D62",
    "True_SelbeAdresse" : "#8DA0CB",
    "SelbeAdresseNachname" : "#E78AC3",
    "Outbreak" : "#A6D854",
}

# all color options in the palette
# "#66C2A5" 
# "#FC8D62"
# "#8DA0CB"
# "#E78AC3"
# "#A6D854"
# "#FFD92F"
# "#E5C494"
# "#B3B3B3"

# "#595959"





# def draw_each_node_group(graph: object, node_groups: list, dm: dict, node_group_names: list, directory: str, filenames: list, layout_alg = 'fdp' ) -> None:
def draw_each_node_group(graph: object, node_groups: list, dm: dict, node_group_names: list, directory: str, filenames: list, 
                         node_labels="number", edge_directions=True, small_subgraphs = []) -> None:
    """

    Args:
        graph (object): _description_
        node_groups (list): _description_
        dm (dict): _description_
    """
    
    
    # if small_subgraphs != []:
    #     print(small_subgraphs)
    #     small_graphs = nx.induced_subgraph(graph, small_subgraphs).copy()
    #     small_graphs.graph['name'] = 'cluster_small_subgraphs'
        
    #     # prepare node_group nodes and edges
    #     id_to_cvdid = prepare_single_node_group(small_graphs, node_labels, edge_directions)
        
    #     # add genetic distance edges to the graph 
    #     add_genetic_edges(small_graphs, dm)
        
         
    
    
    for number, node_group in enumerate(node_groups):
        
        print(node_group_names[number])
        
        if small_subgraphs != []:
            node_group += small_subgraphs
        
        # only keep nodes of the node_group
        subgraph = nx.induced_subgraph(graph, node_group).copy()
        subgraph.graph['name'] = 'cluster_main_graph'
        
        
        
        # prepare node_group nodes and edges
        id_to_cvdid = prepare_single_node_group(subgraph, node_labels, edge_directions)
        
        
        # add genetic distance edges to the graph 
        add_genetic_edges(subgraph, dm)
        
        
    
        # graph attributes
                
        subgraph.graph["edge"] = {
            "splines": "true",
            "fontname":"Arial",
            }
        
        subgraph.graph["node"] = {
            "fontname":"Arial",
            }
        
        subgraph.graph["graph"] = {
            "outputorder":"edgesfirst",
            "fontname":"Arial",
            "label":f"{node_group_names[number]}",
            
            }

        graphviz_graph = to_agraph(subgraph)


        graphviz_graph.subgraph(node_group, name="big_subgraph")
        graphviz_graph.get_subgraph('big_subgraph').graph_attr.update(K='3', sep="4")

        if small_subgraphs != []:
            graphviz_graph.subgraph(small_subgraphs, name="small_subgraphs")
            graphviz_graph.get_subgraph('small_subgraphs').graph_attr.update(K='0.5', sep="5")
        

        # layout graph
        
        
        legend_graph = pgv.AGraph()
                
        legend_graph.from_string("""
            digraph {
                subgraph cluster_01 {
                label = "Legend";
                 node [shape=point]
                
                gd0 [pos="0.1,0.3!", style = invis];
                gd1 [pos="0.9,0.3!", shape=plaintext, label="   Genetic distance\l", fixedsize=true, width=2.5];
                cp0 [pos="0.1,0.4!", style = invis];
                cp1 [pos="0.9,0.4!", shape=plaintext, label="   Contact person\l", fixedsize=true, width=2.5];
                if0 [pos="0.1,0.5!", style = invis];
                if1 [pos="0.9,0.5!", shape=plaintext, label="   Infected by\l", fixedsize=true, width=2.5];
                sa0 [pos="0.1,0.6!", style = invis];
                sa1 [pos="0.9,0.6!", shape=plaintext, label="   Same adress\l", fixedsize=true, width=2.5];
                san0 [pos="0.1,0.7!", style = invis];
                san1 [pos="0.9,0.7!", shape=plaintext, label="   Same adress and family name", fixedsize=true, width=2.5];
                out0 [pos="0.1,0.8!", style = invis];
                out1 [pos="0.9,0.8!", shape=plaintext, label="   Outbreak\l", fixedsize=true, width=2.5];
                
                spacer1 [pos="0.95,0.95!", style = invis ]
                    
                    
                gd0 -> gd1 [penwidth=3 color="#595959", style=dashed, dir=none]
                cp0 -> cp1 [penwidth=3 color="#66C2A5"]
                if0 -> if1 [penwidth=3 color="#FC8D62"]
                sa0 -> sa1 [penwidth=3 color="#8DA0CB"]
                san0 -> san1 [penwidth=3 color="#E78AC3"]
                out0 -> out1 [penwidth=3 color="#A6D854"]
                
            }
        }""")
        

        legend_string = "\n".join(legend_graph.to_string().split("\n")[1:-2])
        node_group_string = "\n".join(graphviz_graph.to_string().split("\n")[:-2] )
        
        full = node_group_string + legend_string + "\n}\n"
        
        graphviz_graph.from_string(full)
        
        graphviz_graph.layout( "fdp" ) 
        
        
        # save
        Path(directory).mkdir(parents=True, exist_ok=True)
        graphviz_graph.draw(filenames[number]) 
    




def add_genetic_edges(graph: object, dm: dict)-> None:
    """

    Args:
        subgraph (object): _description_
        dm (dict): _description_
    """
    
    # fill genetic graph with node_group nodes
    genetic_graph = nx.MultiGraph()
    genetic_graph.add_nodes_from(graph.nodes)
    
     
    genetic_nodes = [ node 
                        for node, data in graph.nodes(data=True)
                            if data["sample_id"] != ""]
    
    
    genetic_edges = []
    
    for u,v in combinations(genetic_nodes, 2):
        
        distance = dm["table"][graph.nodes[u]["dm_index"]][graph.nodes[v]["dm_index"]]
        
        
        # if the distance edge does not have a contact tracing edge at the same pos add 0.1 for mst
        weight = distance + 0.1
        if (u,v) in graph.edges() or (v,u) in graph.edges() :
            weight = distance

        
        genetic_edges.append([u,v, "_".join([u,v,"GeneticDistance"]), {
            "label" : distance,
            "weight" : weight,
            "type" : "GeneticDistance",
            "penwidth":"4",
            "dir":"none",            
            "color":EDGETYPE_TO_COLOR["GeneticDistance"]
        }])

        
        
    genetic_graph.add_edges_from(genetic_edges)
    
    mst = nx.minimum_spanning_tree(genetic_graph,weight='weight')


    graph.add_edges_from(mst.edges(keys=True, data=True))
    


def prepare_single_node_group(graph: object, node_labels: bool, edge_directions: bool) -> None:
    """

    Args:
        graph (object): _description_
    """
    
    id_to_cvdid = {}
    i = 1
    
    
    all_dates = [datetime.strptime(graph.nodes[n]["date"], "%d.%m.%Y") for n in graph.nodes]
    
    
    ###
    # set attributes for the nodes
    all_node_attributes = {}
    for node, data in graph.nodes(data=True):
        # print(node, data)
        
        if node_labels == "number":
            label = f"i"
        elif node_labels == "None":
            label = ""
        elif node_labels == "date":
            oldest_date = min(all_dates)
            datediff = (datetime.strptime(graph.nodes[node]['date'], '%d.%m.%Y') - oldest_date).days
            label = f"{ datediff }, {graph.nodes[node]['sample_id']}\n{node}"
        elif node_labels == "id":
            label = f"{node}"
            
            
            
        if node_labels not in ["date", "id"]:
            data.update({
                "fixedsize":"true",
                "width":"0.37",
                "shape": "circle"
            })
        else:
            data.update({
                "shape": "oval",
                "fontsize": "9pt"
            })
            
        
        data.update({
            
            "label": label,
            "penwidth": "1",
            "style": "filled",
            "color": "black",
            "fillcolor": "lightgray",
            
        })
        
    
        all_node_attributes[node] = data
        
        id_to_cvdid[i] = node
        i+=1
        
    
    nx.set_node_attributes(graph, all_node_attributes)
    
    ###
    # set attributes for the edges
    
    old_same_adrress_edges = [(u,v,k)
                                for u,v,k,d in graph.edges(keys=True, data=True) 
                                    if d["type"] == "SelbeAdresse"]
    graph.remove_edges_from(old_same_adrress_edges)
    
    
    seen_edges = []
    remove_otherway_edges = []
    
    all_edge_attributes = {}
    for u,v, key, data in graph.edges(keys=True, data=True):
        edge= (u,v, key)
        # print(edge, data)
        
        if (v, u, data["type"]) in seen_edges:
            all_edge_attributes[(v,u,f"{v}_{u}_{data['type']}")]["dir"] = "both"
            remove_otherway_edges.append( (u,v,key) )
            continue
        
        seen_edges.append( (u,v,data["type"]) )    
        
        
        data.update({
            "label" : "",
            "type" : data["type"],
            "penwidth":"4",
            "color":EDGETYPE_TO_COLOR[data["type"]],
    
        })
    
        all_edge_attributes[edge] = data
    
    # remove edge directions if not wanted
    if not edge_directions:
        for edge in all_edge_attributes:
            all_edge_attributes[edge]["dir"] = "none"
        
    
    graph.remove_edges_from(remove_otherway_edges)
    nx.set_edge_attributes(graph, all_edge_attributes)
    
    return id_to_cvdid