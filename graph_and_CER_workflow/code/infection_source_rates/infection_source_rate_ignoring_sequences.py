"""
    
"""




import sys

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from infection_source_rate import filter_edgetypes, remove_edges_for_infectionchains_greedy

from util_functions import *



GRAPH_PATH = sys.argv[1]

OUTPUT_JSON_PATH = sys.argv[2]
OUTPUT_GRAPH_PATH = sys.argv[3]




    


def write_results(graph: object, out_fpath: str) -> None :
    """

    Args:
        graph (object): _description_
        out_fpath (str): _description_
    """
    
    
    case_count = len(graph.nodes)
    
    nodes_with_infectionsource =  [graph.in_degree(n) for n in graph.nodes] 
    nodes_with_infectionsource_count = sum( nodes_with_infectionsource )
    
       
    
    stats = {
        "case_count": case_count,
        
        "nodes_with_infectionsource" : nodes_with_infectionsource_count,
        "infsource_rate_per_all_cases":nodes_with_infectionsource_count / case_count,
        
    }
    
    print(stats)
    save_clusters(out_fpath, stats)
    
    
    
if __name__ == "__main__":
    
    graph = load_graph(GRAPH_PATH)
    
    # filter graph by edgetypes     
    filter_edgetypes(graph, "TA_AN_IB_O_KP")    
    
    # calculate infection source graph
    remove_edges_for_infectionchains_greedy(graph)
    
    
    write_results(graph, OUTPUT_JSON_PATH)
    
    save_graph(graph, OUTPUT_GRAPH_PATH)
    
    
    
    
    