"""
    
"""




import sys

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *

code_dir = Path(__file__).resolve().parent.parent
import_dir = code_dir / "infection_source_rates"

sys.path.insert(0, str(import_dir))

from infection_source_rate import *
from downsampled_clusters import subsample_graph


DM_PATH = sys.argv[1]
SUBSAMPLING_PATH = sys.argv[2]
GRAPH_PATH = sys.argv[3]
CLUSTERS_PATH = sys.argv[4]

CLUSTER_NAME = sys.argv[5]

OUTPUT_RATES_PATH = sys.argv[6]

    
    
    
    
    
if __name__ == "__main__":
    
    # load needed input files
    dm = load_dm(DM_PATH)
    subsamples = load_subsamples(SUBSAMPLING_PATH)
    base_graph = load_graph(GRAPH_PATH)
    subsa_to_clusters = load_clusters(CLUSTERS_PATH) 

    distance_threshhold = int(CLUSTER_NAME[-1])
    
    
    
    subsa_to_cer = {}
    for key in subsamples:
        print(key)
        clusters = subsa_to_clusters[key]

        graph = base_graph.copy()
        
        subsample_graph(graph, subsamples[key])
            
        ## Currently only with jump 2 
        # prepare by adding jump edges and filtering by genetically supported edges
        jump_edges_data = add_jump_distances(graph, dm, clusters, 2, distance_threshhold)
        
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
        
    
        
        subsa_to_cer[key] = stats
        
        
        
        
    # save output stats 
    save_clusters(OUTPUT_RATES_PATH, subsa_to_cer)
    
    
    
    
    
    