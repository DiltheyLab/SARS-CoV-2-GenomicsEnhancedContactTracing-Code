"""

    
"""




import sys

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *
from draw_graphs_base_functions import *




CLUSTERS_PATH = sys.argv[1]
DATES_PATH = sys.argv[2]
GRAPH_PATH = sys.argv[3]
DM_PATH = sys.argv[4]

CLUSTER_NAME = sys.argv[5]

OUTPUT_TXT_PATH = sys.argv[6]




def cluster_stats(clusters: list, graph: object, name: str) -> str:
    """
    just to produce an output file for snakemake. avoid this...

    Args:
        clusters (list): list of lists with clusters
        graph (object): nx Multigraph object
        name (str): name of the cluster type

    Returns:
        str: String containing stats
    """
    
    output_str = f"\n\n{name} stats: \n"


    return output_str + "\n\n"




    
    



if __name__ == "__main__":
    
    graph = load_graph(GRAPH_PATH)
    clusters = load_clusters(CLUSTERS_PATH)
    dates = load_clusters(DATES_PATH)
    dm = load_dm(DM_PATH)
    
    
    cluster_names = [ f"Cluster {i+1}" 
                        for i, _ in enumerate(clusters)]
    
    directory = f"plots/graphs/{CLUSTER_NAME}/"
    filenames = [f"{directory}/cluster_{i+1}.svg" 
                    for i, _ in enumerate(clusters)]

    draw_each_node_group(graph, clusters, dm, cluster_names, directory, filenames, node_labels="None")
    
    

    # text based output 
    cluster_stats = cluster_stats(clusters, graph, CLUSTER_NAME)
    with open(OUTPUT_TXT_PATH, "w") as out_file:
        out_file.write(cluster_stats)

