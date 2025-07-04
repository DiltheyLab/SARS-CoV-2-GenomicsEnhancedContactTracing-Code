"""
    
"""




import sys

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *

code_dir = Path(__file__).resolve().parent.parent
cluster_dir = code_dir / "cluster"

# Add "code" or "cluster" to the path so we can import from it
sys.path.insert(0, str(cluster_dir))

from clusters import *





DM_PATH = sys.argv[1]
SUBSAMPLING_PATH = sys.argv[2]
GRAPH_PATH = sys.argv[3]

CLUSTER_TYPE = sys.argv[4]

OUTPUT_CLUSTERS_PATH = sys.argv[5]


def subsample_graph(tmp_graph: object, subsampling: dict) -> None:
    """

    Args:
        tmp_graph (object): _description_
        subsampling (dict): _description_
    """

    for node, data in tmp_graph.nodes(data=True):
        
        if "sample_id" in data and data["sample_id"] != "":
            
            if not subsampling[node]:
                
                tmp_graph.nodes[node]["sample_id"] = ""
    
                continue

    


if __name__ == "__main__":
    
    dm = load_dm(DM_PATH)
    subsamples = load_subsamples(SUBSAMPLING_PATH)
    graph = load_graph(GRAPH_PATH)
    
    distance_cutoff = int(CLUSTER_TYPE[-1])
    
    subsa_to_clusters = {}
    for key in subsamples:
        print(key)
        
        tmp_graph = graph.copy()
        
        subsample_graph(tmp_graph, subsamples[key])
            
        clusters = calculate_connected_components(dm, tmp_graph, distance_cutoff)
        
        subsa_to_clusters[key] = clusters
    
    
    save_clusters(OUTPUT_CLUSTERS_PATH, subsa_to_clusters)
    
    