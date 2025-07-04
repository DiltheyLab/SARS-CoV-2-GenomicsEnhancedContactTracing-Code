"""
    
"""




import sys
import numpy as np

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *
from datetime import datetime, timedelta


DM_PATH = sys.argv[1]
GRAPH_PATH = sys.argv[2]
OUTPUT_CLUSTERS_PATH = sys.argv[3]
OUTPUT_DATES_PATH = sys.argv[4]

    
    
    
    
def calculate_connected_components(dm: dict, graph: object, distance_limit: int) -> list:
    """
        Find clusters: 
    calculates cliques: set of cases that have all pairwise distance 0
    
    To do this create a graph containing only <= distance_limit distance edges
    -> date cutoff
    -> find connected components 
    -> return them in list

    Args:
        dm (dict): "header" = list of sampleids, "table"= list of lists of distances
        graph (object): nx Multigraph object
        distance_limit (int): which distances are considered? 

    Returns:
        list: list of clusters 

    """
    
    # mapping from sample id to node
    sid_to_caseid = {graph.nodes[node]["sample_id"]:node 
                        for node in graph.nodes 
                            if graph.nodes[node]["sample_id"] != ""}
    

    # for each node find all other nodes with fitting distance and add each pair to a list
    distance_edges = []
    for caseid in graph.nodes:
        # if node has no sequence ignore node
        if graph.nodes[caseid]["sample_id"] == "":
            continue
        
        index1 = graph.nodes[caseid]["dm_index"]
        date1 = graph.nodes[caseid]["date"]
        datetime1 = datetime.strptime(date1, "%d.%m.%Y")
        
        
        # find the indexes of all samples with distance <= distance_limit to current node
        line = np.array(dm["table"][index1])

        indexes_2 = list(np.where(line <= distance_limit)[0])

        # for each sample index check if it belongs to a caseid 
        # check date difference
        # and then add to list
        for index2 in indexes_2:
            sid = dm["header"][index2]
            
            # skip if not usale sid 
            if sid not in sid_to_caseid:
                continue
        
            # if sample not in daterange of each other
            date2 = graph.nodes[sid_to_caseid[sid]]["date"]
            datetime2 = datetime.strptime(date2, "%d.%m.%Y")

            if not (-14 <= (datetime1 - datetime2).days <= 14) :
                continue
            
            distance_edges.append( (caseid, sid_to_caseid[sid]) )
            
          
    # create graph of only those edges
    components_graph = nx.Graph()
    components_graph.add_edges_from(distance_edges)
    
    # calculate all components with nxgraph
    components = [list(c) for c in nx.connected_components(components_graph)]
        
    # only keep the components that have at least 2 nodes
    components = [c for c in components if len(c) > 1 ] 

    return components  
    



def average_times(graph: object, clusters: list) -> list:
    """
    For each cluster find the average time point

    Args:
        graph (object): nx Multigraph object
        clusters (int): list of clusters

    Returns:
        list: list of times (same ordering) 
    """
    times = []
    # just used in calculations
    any_reference_date = datetime.strptime("01.02.2021", "%d.%m.%Y")

    # every cluster gets one average time
    for cluster in clusters:
        
        # collect all times of one cluster
        times_in_cluster = []
        for node in cluster:
            
            date = graph.nodes[node]["date"]
            date_time = datetime.strptime(date, "%d.%m.%Y")
            
            times_in_cluster.append(date_time)
            
        # calculate the average time from them
        average_time = any_reference_date \
                       + sum([date - any_reference_date for date in times_in_cluster], timedelta()) \
                           / len(times_in_cluster)
        
        # save result
        times.append( average_time.strftime("%d.%m.%Y") )
    
    return times


if __name__ == "__main__":
    
    dm = load_dm(DM_PATH)
    graph = load_graph(GRAPH_PATH)
    
        
    if "connected_components_0.json" in OUTPUT_CLUSTERS_PATH:
        clusters = calculate_connected_components(dm, graph, 0)
    
    elif "connected_components_0_1.json" in OUTPUT_CLUSTERS_PATH:
        clusters = calculate_connected_components(dm, graph, 1)
    
    times = average_times(graph, clusters)
    
    save_clusters(OUTPUT_CLUSTERS_PATH, clusters)
    save_clusters(OUTPUT_DATES_PATH, times)
    
    