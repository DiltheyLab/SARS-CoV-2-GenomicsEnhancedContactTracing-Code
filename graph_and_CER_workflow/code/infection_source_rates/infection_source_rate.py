"""
    
"""




import sys

import itertools
from datetime import datetime, timedelta

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *


if __name__ == "__main__":
    DM_PATH = sys.argv[1]
    GRAPH_PATH = sys.argv[2]
    CLUSTERS_PATH = sys.argv[3]

    CLUSTER_NAME = sys.argv[4]
    GRAPTH_TYPE = sys.argv[5]
    JUMP_DISTANCE = sys.argv[6]
    EDGE_TYPES = sys.argv[7]

    # OUTPUT_JUMP_PATH = sys.argv[8]
    OUTPUT_RATES_PATH = sys.argv[8]
    OUTPUT_GRAPH_INFECTIONSOURCES = sys.argv[9]
    OUTPUT_GRAPH_INFECTIONSOURCES_ADDGENETIC = sys.argv[10]

    
    

def remove_edges_not_supported_by_genetic_data(graph:object, dm: dict, distance_threshhold: int ) -> None:
    """
    Remove all contact edges without supporting genetic information
    
    Args:
        graph (object): _description_
        dm (dict): _description_
        distance_threshhold (int): _description_
    """
    
    # collect all edges to be removed
    remove_edges = []
    for edge in graph.edges(data=True, keys=True):
        

        # keep edge as it was already generated with this distance condition
        if edge[3]["type"] == "Jump_edge":
            continue
        
        # if one of the nodes does not have a sequence: remove the edge
        if graph.nodes[edge[0]]["sample_id"] == "" or graph.nodes[edge[1]]["sample_id"] == "":
            remove_edges.append( edge[0:3] )
            continue
        
        # if the distance is larger then wanted remove the edge
        index1 = graph.nodes[edge[0]]["dm_index"]
        index2 = graph.nodes[edge[1]]["dm_index"]
        if dm["table"][index1][index2] > distance_threshhold:
            remove_edges.append( edge[0:3] )
            
    # and remove them
    graph.remove_edges_from(remove_edges)
    
    
    
    
def remove_edges_for_infectionchains_greedy(graph: object) -> None:
    """

    Args:
        graph (object): _description_
    """
    
    
    temp_graph = graph.copy()

    keep_edges = []
    while_counter = 0 # just a precaution
    
    
    # remove the maximal component until the subgraph is empty
    while temp_graph.number_of_nodes() > 0:
    
        # filter unconnected nodes from graph for performance
        temp_graph.remove_nodes_from(list(nx.isolates(temp_graph)))
    
        # find current maximal component (tree)
        max_component = []
        for node in temp_graph.nodes:
            
            descendants = list(nx.descendants(temp_graph, node))
            current_component = descendants + [node]
            
            if len(current_component) > len(max_component):
                max_component = current_component
                
        ### Just count max component size minus 1? no because we want the edges
        
        subgraph = nx.induced_subgraph(temp_graph, max_component).copy()  
        tree_of_component = nx.algorithms.tree.branchings.Edmonds(subgraph).find_optimum(style="arborescence", kind="min", preserve_attrs=True)
        
        
        keep_edges += list(tree_of_component.edges(data=True, keys=True))
        
        # save and remove component
        temp_graph.remove_nodes_from(max_component)         
        
        
        # to prevent infinite loops when running snakemake. should not happen
        while_counter +=1
        if while_counter > 50000:
            print(f"Infinite loop!!!!!")
            sys.exit(1)
             
        
    before_count = len(graph.edges)
    
    # and remove them
    graph.remove_edges_from(list(graph.edges(keys=True)))
    graph.add_edges_from(keep_edges)
        
    print("Before removing multiple infection sources", before_count)
    print("After", len(graph.edges))
    print("Removed in total",before_count-len(graph.edges))

    
    
    
    
    
def calculate_infection_source_rate(infectionsource_graph: object, additionalinfections_graph: object, clusters: list, jump_edges_data: list) -> list:
    """
    Args:
        infectionsource_graph (object): nx Multigraph object (with only edges of input edge types)
        additionalinfections_graph (object): nx Multigraph object (with only edges of input edge types)
        clusters (list): list of clusters 
        jump_edges_data (list): .. 
        
    Returns:
        list: list of dicts with rates and metadata

    """
    # get all the nessecary values
    case_count = len(infectionsource_graph.nodes)
    
    sequenced_case_count = len([n for n in infectionsource_graph.nodes
                                    if infectionsource_graph.nodes[n]["sample_id"] != ""])
    
    cluster_case_count = sum([len(c) for c in clusters])
    
    nodes_with_infectionsource =  [infectionsource_graph.in_degree(n) for n in infectionsource_graph.nodes] 
    nodes_with_infectionsource_count = sum( nodes_with_infectionsource )
    
    
    nodes_with_infectionsource_addgenetic =  [additionalinfections_graph.in_degree(n) for n in additionalinfections_graph.nodes] 
    nodes_with_infectionsource_addgenetic_count = sum( nodes_with_infectionsource_addgenetic )
    
    jump_count = sum([len(edges) for edges in jump_edges_data])
    jump_count_used = len([u for u,v,data in infectionsource_graph.edges(data=True) if data["type"] == "Jump_edge"])
    
   
    
    # print(f"{nodes_with_infectionsource_addgenetic_count - nodes_with_infectionsource_count} additional cases with possible infection source")
    
    
    
    rates_per_cluster = []
    for cluster in clusters:
        tmp_graph = nx.induced_subgraph(infectionsource_graph, cluster).copy()
        
        edge_count = len(list(tmp_graph.edges()))
        
        rates_per_cluster.append(  edge_count / len(cluster) )
        
    # assemble all the values into a dictionary to save the results
    
    stats = {
        "case_count": case_count,
        "sequenced_case_count": sequenced_case_count,
        "cluster_case_count": cluster_case_count,
        
        "jump_edges_count" : jump_count,
        "jump_edges_used" : jump_count_used,
        
        "nodes_with_infectionsource" : nodes_with_infectionsource_count,
        "infsource_rate_per_all_cases":nodes_with_infectionsource_count / case_count,
        "infsource_rate_per_sequenced_cases":nodes_with_infectionsource_count / sequenced_case_count,
        "infsource_rate_per_cluster_cases":nodes_with_infectionsource_count / cluster_case_count,
        
        "additional_genectiv_cases_infsource" : nodes_with_infectionsource_addgenetic_count - nodes_with_infectionsource_count,
        "genetic_infectionsources": nodes_with_infectionsource_addgenetic_count,
        "genetic_infsource_rate_per_all_cases": nodes_with_infectionsource_addgenetic_count / case_count,
        "genetic_infsource_rate_per_sequenced_cases": nodes_with_infectionsource_addgenetic_count / sequenced_case_count,
        "genetic_infsource_rate_per_cluster_cases": nodes_with_infectionsource_addgenetic_count / cluster_case_count,
        
        "max_rate_per_cluster_cases" : (cluster_case_count - len(clusters)) / cluster_case_count,
        
        "rates_per_cluster" : rates_per_cluster,
    }
    
    
    
    return stats
    


def add_gentic_edges(graph: object, dm: dict, distance_threshhold: int, direction_cutoff: int) -> None:
    """

    Args:
        additionalinfections_graph (object): _description_
        dm (dict): "header" = list of sampleids, "table"= list of lists of distances
        distance_threshhold (int): ...
        direction_cutof (int): ...
    """

    all_sequenced_nodes = [n for n in graph.nodes if graph.nodes[n]["sample_id"] != ""]

    add_edges = []
    counter= 0

    # got through all pairs and add an edge if it is below the threshold and there is no contact edge    
    for n1 in all_sequenced_nodes:
        for n2 in all_sequenced_nodes:
            
            if n1 == n2:
                continue
            
            index1 = graph.nodes[n1]["dm_index"]
            index2 = graph.nodes[n2]["dm_index"]
            if dm["table"][index1][index2] > distance_threshhold:
                continue
            
            if graph.has_edge(n1, n2) or graph.has_edge(n2, n1):
                continue
      
            # aldo filter the genetic edges by symptom starts

            #if a case has no symtopmstartsubtract this amount of days from the meldedatum
            avg_datediff_ga = 3
            
            # collect normal and or fake symptom start dates
            if graph.nodes[n1]["symptom_start"] != "":
                date1 = datetime.strptime(graph.nodes[n1]["symptom_start"], "%d.%m.%Y")
            else:
                date1 = datetime.strptime(graph.nodes[n1]["date"], "%d.%m.%Y") - timedelta(days=avg_datediff_ga)
            
            if graph.nodes[n2]["symptom_start"] != "":
                date2 = datetime.strptime(graph.nodes[n2]["symptom_start"], "%d.%m.%Y")
            else:
                date2 = datetime.strptime(graph.nodes[n2]["date"], "%d.%m.%Y") - timedelta(days=avg_datediff_ga)
                
            # calculate datediff
            datediff = (date1-date2).days
            
            # we dont wont edges with large date differences
            if abs(datediff) > 14:
                counter+=1
                continue
            
            
            # if case 2 was earlier than case 1 by enough
            if datediff > direction_cutoff:
                all_pairs = [ (n2, n1) ]
            # if case 1 was earlier than case 2 by enough
            elif datediff < - direction_cutoff:
                all_pairs = [ (n1, n2) ]
            # if no clear direction can be asserted from the date difference
            else:
                all_pairs = [ (n1, n2), (n2, n1) ]

                
            
            # save the corresponding edges
            edge_data = {
                "type": "Genetic_edge",
                "distance" : dm["table"][index1][index2],
            }
            
            # collect edge data for graph
            add_edges += [
                list(pair) + [f"{n1}_{n2}_Genetic_edge", edge_data]
                    for pair in all_pairs
            ]

    print(f"Added {len(add_edges)} genetic edges below the threshold")
    graph.add_edges_from(add_edges)
            
    print("Edges not added because of large date difference:", counter)
            


    
    

def add_jump_distances(graph: object, dm: dict, clusters: list, jump_distance: int, distance_threshhold: int)-> list:
    """
    Add jump connections to the case graph
    

    Args:
        filtered_graph (object): nx Multigraph object (with only edges of input edge types)
        dm (dict): "header" = list of sampleids, "table"= list of lists of distances
        clusters (list): list of clusters 
        jump_distance (int): 0 = no edges added, add edges if jumpable trough one other node
        distance_threshhold (int): distance threshhold from cluster definition

    Returns:
        list: list of lists of edges added (one list of lists per cluster)
    """
    
    if jump_distance == 0:
        return []
    
    if jump_distance > 2:
        exit(1)

    add_edges = []
    jump_edges_data = [] # will be returned and saved

    # only calculate jumps for nodes in clusters
    for cluster_number,cluster in enumerate(clusters):
        all_combs = list(itertools.combinations(cluster, 2))
        jump_edges_data.append([])
        
        # # subgraph of the current cluster
        # subgraph = nx.induced_subgraph(graph, cluster).copy()
        
        # check vor every unique pair of nodes if they can be jumped
        for v,w in all_combs:
            
            # if there already was a node
            if graph.has_edge(v,w):
                continue
            
            # distance should be smaller then (jumpdistance +1) * cluster threshhold
            index1 = graph.nodes[v]["dm_index"]
            index2 = graph.nodes[w]["dm_index"]
            if dm["table"][index1][index2] > (jump_distance +1) * distance_threshhold:
                continue

            # check if there are paths between the nodes
            # try catch because if there is no path nx.all_shortest_paths raises NetworkXNoPath error 
            try:
                paths = list(nx.all_shortest_paths(graph, v, w))
            except:
                continue
        
        
            # look at all found paths until one fits and add that to add_edges
            for path in paths:
                # only accept paths of wanted lengths
                if len(path) > jump_distance+2:
                    continue

                # if a middle node is sequenced dont keep the path
                all_nodes_ok = True
                for node in path[1:-1]:
                    if graph.nodes[node]["sample_id"] != "":
                    # if node in cluster:
                        all_nodes_ok = False
                if not all_nodes_ok:
                    continue
                
                
                # collect all edge types used on the way
                edge_types = set()
                for i in range(len(path) - 1):
                    a,b = path[i: i + 2]
                    for info in dict(graph.adj[a][b]).values():
                        edge_types.add(info["type"])
                    
                
                # collect edge data for graph
                add_edges.append([
                    v,
                    w,
                    {
                        "type": "Jump_edge",
                        # "consists_of": list(edge_types)
                    }
                ])
                

                
                # collect edges data for later meta analysis 
                jump_edges_data[cluster_number].append([
                    v,w,
                    list(edge_types),
                    # is_new_node,
                    # path
                ])
                
                
                # once one path is found dont look for others between these nodes
                break


    print(f"Adding {len(add_edges)} jump edges to graph")
    graph.add_edges_from(add_edges)

    return jump_edges_data


def filter_edgetypes(graph: object, edge_types_string: str) -> None:
    """
    Remove edges from the graph that do not belong to the given edge types in edge_types_string (also parent dir)

    Args:
        graph (object): nx Multigraph object
        edge_types_string (str): String containing the shortforms of edge types

    Returns:
        
    """

    shortform_to_types = {
        "A" : "SelbeAdresse",
        "TA": "True_SelbeAdresse",
        "AN": "SelbeAdresseNachname",
        "IB": "SurvnetAngestecktBei",
        "O" : "Outbreak",
        "KP": "SurvnetKontaktperson",
    }
    
    # split the input string into a list of actual type names
    edge_types = [shortform_to_types[et] for et in edge_types_string.split("_")]

    # collect all edges to be removed
    remove_edges = []
    for edge in graph.edges:
    
        if graph.edges[edge]["type"] not in edge_types:
            remove_edges.append(edge)
            
    # and remove them
    graph.remove_edges_from(remove_edges)
    
    
if __name__ == "__main__":
    
    # load needed input files
    dm = load_dm(DM_PATH)
    graph = load_graph(GRAPH_PATH)
    clusters = load_clusters(CLUSTERS_PATH) 

    # clean up input parameters
    jump_distance = int(JUMP_DISTANCE[-1])
    distance_threshhold = 0
    if CLUSTER_NAME.startswith("connected_components_"):
        distance_threshhold = int(CLUSTER_NAME.split("_")[-1])

    # filter graph by edgetypes     
    filter_edgetypes(graph, EDGE_TYPES)
        
    # prepare further by adding jump edges and filtering by genetically supported edges
    jump_edges_data = add_jump_distances(graph, dm, clusters, jump_distance, distance_threshhold)
    remove_edges_not_supported_by_genetic_data(graph, dm, distance_threshhold)
    
    # creaty two copies of the preapared graph for the two analysis steps
    infectionsource_graph = graph.copy()
    additionalinfections_graph = graph.copy()
    
    
    # calculate infection source graph
    remove_edges_for_infectionchains_greedy(infectionsource_graph)
    
    
    # calculate graph to find additional infection sources with genetic data
    direction_cutoff = int(GRAPH_PATH.split(".")[0].split("_")[-1])
    add_gentic_edges(additionalinfections_graph, dm, distance_threshhold, direction_cutoff)
    
    remove_edges_for_infectionchains_greedy(additionalinfections_graph)
    
      
    # calculate both stats
    stats = calculate_infection_source_rate(infectionsource_graph, additionalinfections_graph, clusters, jump_edges_data)
        
    
    
    # save both graphs
    save_graph(infectionsource_graph, OUTPUT_GRAPH_INFECTIONSOURCES)
    save_graph(additionalinfections_graph, OUTPUT_GRAPH_INFECTIONSOURCES_ADDGENETIC)
    
    # save output stats 
    save_clusters(OUTPUT_RATES_PATH, stats)
    
    
    
    
    
    