"""
    
"""




import sys

from datetime import datetime, timedelta
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import itertools
from collections import defaultdict

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *


GRAPH_PATH = sys.argv[1]
EDGES_PATH = sys.argv[2]
DISTANCES_PATH = sys.argv[3]

OUTPUT_GRAPH_PATHS = sys.argv[4]





def transform_to_directed(graph: object, direction_cutoff: int, edges_path: str) -> object:
    """
    create the directed case graph from the undirected cas graph

    Args:
        graph (object): nx Multigraph object with undirected edges
        direction_cutoff (int): value od date difference between nodes after which a direction can be assumed
        edges_path (str): ... 

    Returns:
        object: nx Multigraph object with directed edges
    """
    
    
    infected_by_pairs = []
    with open(edges_path, "r") as edge_file:
        # skip header
        for line in edge_file.readlines()[1:]:
            line =  line.split("\t")
            if line[2] == "SurvnetAngestecktBei":
                infected_by_pairs.append(line[:2])
    
    
    di_graph = nx.MultiDiGraph()
    
    # keep all cases and their attributes
    di_graph.add_nodes_from(graph.nodes(data=True))
    
    

    # collect directinal edge(s) for every edge in original graph
    add_edges = []
    edge_information = []
    for caseid1, caseid2, key, edge_data in graph.edges(data=True, keys=True):
        current_edgeinfo = []
        
        date1 = datetime.strptime(graph.nodes[caseid1]["date"], "%d.%m.%Y")
        date2 = datetime.strptime(graph.nodes[caseid2]["date"], "%d.%m.%Y")
        datediff = (date1-date2).days
        
        # for infected by cases we only assume one direction given by the context of the connection
        if edge_data["type"] == "SurvnetAngestecktBei":
            
            # flip edge direction to the way it is in the input csv file
            edge_direction = [caseid1, caseid2]
            if [caseid1, caseid2] not in infected_by_pairs:
                edge_direction = [caseid2, caseid1]
            
            add_edges.append([
                edge_direction[0],
                edge_direction[1],
                f"{edge_direction[0]}_{edge_direction[1]}_{edge_data['type']}",
                edge_data
            ])
            current_edgeinfo.append([
                edge_direction[0],
                edge_direction[1],
                edge_data["type"],
                abs(datediff)
            ])
        
        
        # Outbreak, SelbeAdresseNachname, SelbeAdresse, SurvnetKontaktperson
        else:
            
            # if a case has no symtopmstartsubtract this amount of days from the meldedatum
            avg_datediff_ga = 3
            
            # collect normal and or fake symptom start dates
            if graph.nodes[caseid1]["symptom_start"] != "":
                date1 = datetime.strptime(graph.nodes[caseid1]["symptom_start"], "%d.%m.%Y")
            else:
                date1 = datetime.strptime(graph.nodes[caseid1]["date"], "%d.%m.%Y") - timedelta(days=avg_datediff_ga)
            
            if graph.nodes[caseid2]["symptom_start"] != "":
                date2 = datetime.strptime(graph.nodes[caseid2]["symptom_start"], "%d.%m.%Y")
            else:
                date2 = datetime.strptime(graph.nodes[caseid2]["date"], "%d.%m.%Y") - timedelta(days=avg_datediff_ga)
                
            # calculate datediff
            datediff = (date1-date2).days
            


            # if case 2 was earlier than case 1 by enough
            if datediff > direction_cutoff:
                all_pairs = [ (caseid2, caseid1) ]
            # if case 1 was earlier than case 2 by enough
            elif datediff < - direction_cutoff:
                all_pairs = [ (caseid1, caseid2) ]
            # if no clear direction can be asserted from the date difference
            else:
                all_pairs = [ (caseid1, caseid2), (caseid2, caseid1) ]

            # add all wanted edges to list with proper key
            for c1, c2 in all_pairs:                
                add_edges.append([
                    c1,
                    c2,
                    f"{c1}_{c2}_{edge_data['type']}",
                    edge_data
                ])
                current_edgeinfo.append([
                    caseid1,
                    caseid2,
                    edge_data["type"],
                    datediff
                ])
        
        edge_information.append(current_edgeinfo)
        
    di_graph.add_edges_from(add_edges)
    
    print("undirected edgecount:", len(graph.edges))
    print("directed edgecount:", len(di_graph.edges))
    
    return di_graph, edge_information
    
    
    
    
    
    
    

def plot_data(edge_infos: list, direction_cutoff: int) -> None:
    """

    Args:
        edge_infos (list): _description_
        direction_cutoff (int): _description_
    """
    # TODO this should ultimately not be done. all the fixed edgetypes should be extracted into
    # a input conifg yaml or something
    german_to_english_name = {
        # "SelbeAdresse" : "Same adress",
        "True_SelbeAdresse" : "SameAddress - differentName",
        "SelbeAdresseNachname" : "SameAddress - sameName",
        "SurvnetAngestecktBei" : "Backward contact tracing",
        "SurvnetKontaktperson" : "Forward contact tracing",
        "Outbreak" : "Outbreak",
    }
        
    data = []
    for info in edge_infos:
        amount = len(info)
        for _, _, t, diff in info:
            if t in german_to_english_name:
                t = german_to_english_name[t]
                data.append( [t,amount, diff] )
    
    data = sorted(data, key=lambda x: x[0], reverse=True)
    
    ###############################
    # plot count added per edgetype
    
    #########################################TODO
    ######################################### used in paper
    plt.clf()
 
    sns.set_theme(style="whitegrid", font_scale=2.5) 
    
    fig, ax = plt.subplots(figsize=(12, 11))
       
 
    types_to_count = defaultdict(int)
    for t,amount, diff in data:
        if amount == 1:
            types_to_count[(t,amount)] += 1
        if amount == 2:
            types_to_count[(t,amount)] += .5
        
    
    types = [t for t,_ in types_to_count.keys()]
    counts = list(types_to_count.values())
    
    amount_edges = [a for _,a in types_to_count.keys()]
    amount_to_string = {
        1: "Directed edges",
        2: "Undirected edges"
    }
    
    
    ax = sns.barplot(x=types, y=counts, hue=[amount_to_string[a] for a in amount_edges],
                     palette=["#FC8D62", "#66C2A5"])#, color="skyblue")
    

    totals = defaultdict(int)
    for t,amount in types_to_count.keys():
        totals[t] += types_to_count[t, amount]
    
    labels_1 = []
    labels_2 = []
    
    for t,amount in types_to_count.keys():
        percent_val = f"{round(types_to_count[t, amount] / totals[t] * 100, 1)}"
    
        if amount == 1:
            labels_1.append( percent_val )
        if amount == 2:
            labels_2.append( percent_val )
    
    
    ax.bar_label(ax.containers[0], fontsize=20, labels=labels_1)
    ax.bar_label(ax.containers[1], fontsize=20, labels=labels_2)

    ax.figure.autofmt_xdate(rotation=45)
    plt.tight_layout(pad=2)   
    
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    
    # labels
    plt.title(f"Directionalisation cut-off > {direction_cutoff} days")
    
    plt.xlabel("Edge type")
    plt.ylabel("Count")

    plt.savefig(f"plots/directed_graph/types_to_added_with_cutoff_{direction_cutoff}.svg")
    




def plot_with_distances(graph: object, dm: dict, cutoff: int) -> None:
    """
    TODO

    Args:
        di_graph (object): _description_
        dm (dict): _description_
        cutoff (int): _description_
    """


    distance_to_oneway_count = defaultdict(int)
    distance_to_bothway_count = defaultdict(int)
    count = 0
    count2 = 0
    for u, v, data in graph.edges(data=True):
        
        if graph.nodes[u]["sample_id"] == "":
            continue
        if graph.nodes[v]["sample_id"] == "":
            continue
        
        index1 = graph.nodes[u]["dm_index"]
        index2 = graph.nodes[v]["dm_index"]
        
        distance = dm["table"][index1][index2]
        
        if graph.has_edge(v,u, f"{v}_{u}_{data['type']}"):
            
            count += 1
            distance_to_bothway_count[distance] += .5
        else:
            count2 += 1
            distance_to_oneway_count[distance] += 1
            
        
    bothway_keys = list(distance_to_bothway_count.keys())
    bothway_vals = [int(y) for x,y in distance_to_bothway_count.items()]
    
    
    #########
    # plot cases with vs without symptomstart (per month) cutoff at 15
    
    #########################################TODO
    ######################################### used in paper

    plt.clf()
    sns.set_theme(style="whitegrid", font_scale=2.6) 
    
    fig, ax = plt.subplots(figsize=(12, 8))
    

    over_7_bothway = sum([val for key, val in zip(bothway_keys, bothway_vals) if key >= 7 ])
    over_7_oneway = sum([val for key, val in list(distance_to_oneway_count.items()) if key >= 7 ])
        
    x_both = [key for key, val in zip(bothway_keys, bothway_vals) if key < 7 ] + [7]
    y_both = [val for key, val in zip(bothway_keys, bothway_vals) if key < 7 ] + [over_7_bothway]
    
        
    x_one = [key for key, val in list(distance_to_oneway_count.items()) if key < 7 ] + [7]
    y_one = [val for key, val in list(distance_to_oneway_count.items()) if key < 7 ]  + [over_7_oneway]
    
    
    ax = sns.barplot(x= x_one + x_both,
                    y= y_one + y_both,
                    hue = ["Directed edges"] * len(x_one) + ["Undirected edges"] * len(x_both),
                    palette=["#FC8D62", "#66C2A5"])


    ax.set_ylabel('Count')
    ax.set_xlabel('Genetic distance')
    plt.title(f"Directionalisation cut-off > {cutoff} days") 

    
    one_sorted = sorted(zip(x_one, y_one), key=lambda x:x[0])
    both_sorted = sorted(zip(x_both, y_both), key=lambda x:x[0])
    

    
    labels_1 = []
    labels_2 = []   
    for one, both in zip(one_sorted, both_sorted):
        print(one, both)
        one =  one[1]
        both =  both[1]
        labels_1.append( f"{round( one / (both+one) * 100, 1)}")
        labels_2.append(f"{round( both / (both+one) * 100, 1)}")
    
    ax.bar_label(ax.containers[0], fontsize=17, labels=labels_1)
    ax.bar_label(ax.containers[1], fontsize=17, labels=labels_2)


    # labels
    labels = [item.get_text() for item in ax.get_xticklabels()]
    labels[-1] = '>= 7'

    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    ax.set_xticklabels(labels)

    plt.savefig(f"plots/directed_graph/Directed_undirected_split_per_genetic_distance_{cutoff}_cutat7.svg")

    
    
    
    
    
    
if __name__ == "__main__":
    
    graph = load_graph(GRAPH_PATH)
    dm = load_dm(DISTANCES_PATH)
    Path(f"plots/directed_graph/").mkdir(parents=True, exist_ok=True)
    
    cutoff = int(OUTPUT_GRAPH_PATHS.split(".")[0].split("_")[-1])
        
    print(f"Creating directed graph with cutoff {cutoff}")
        
    di_graph, edge_infos = transform_to_directed(graph, cutoff, EDGES_PATH)
        
    plot_with_distances(di_graph, dm, cutoff)
    
    plot_data(edge_infos, cutoff)
    
    save_graph(di_graph, OUTPUT_GRAPH_PATHS)
    