"""  

Supplementary Table Clusters:  directed 3, alle Kanten typen 

    Connected components 0 / 1 

    Cluster number 

    Cluster size 

    CER (pro jump distance) 

    Cluster length
    
    
"""




from datetime import timedelta
import sys
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from collections import defaultdict

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *
from cluster_length_stats import get_cluster_lengths





CLUSTERS_PATH = sys.argv[1]
DATES_PATH = sys.argv[2]

CER_0_PATHS = sys.argv[3:6]
CER_3_PATHS = sys.argv[6:9]

GRAPH_PATH = sys.argv[9]

CLUSTER_TYPE = sys.argv[10]

OUTPUT_CSV_PATH = sys.argv[-1]




def fill_table(clusters: list, cluster_dates: list , cluster_lengths: list, cluster_type: str , graph: object, cer_0_paths: list, cer_3_paths: list) -> None:
    """
    

    Args:
        clusters (list): _description_
        cluster_dates (list): _description_
        cluster_lengths (list): _description_
        graph (object): _description_
    """
    
    
    table = []

    print(cer_0_paths) 
    print(cer_3_paths) 
    
    cers_0_0 = load_samples(cer_0_paths[0])["rates_per_cluster"]
    cers_0_1 = load_samples(cer_0_paths[1])["rates_per_cluster"]
    cers_0_2 = load_samples(cer_0_paths[2])["rates_per_cluster"]
    cers_3_0 = load_samples(cer_3_paths[0])["rates_per_cluster"]
    cers_3_1 = load_samples(cer_3_paths[1])["rates_per_cluster"]
    cers_3_2 = load_samples(cer_3_paths[2])["rates_per_cluster"]
        

    for i, cluster in enumerate(clusters):
        
        
        line = [
            # cluster_type[-1],             # cluster type (0 or 1)
            f"{cluster_type[-1]}_{i+1}",    # cluster number
            len(cluster),                   # cluster size
            cluster_lengths[i],             # cluster length
            cluster_dates[i],               # mean date
            cers_0_0[i],                         # CER jump 0 
            cers_0_1[i],                         # CER jump 1 
            cers_0_2[i],                         # CER jump 2 
            cers_3_0[i],                         # CER jump 0 
            cers_3_1[i],                         # CER jump 1 
            cers_3_2[i],                         # CER jump 2 
        ]

        table.append( [str(e) for e in line] )

    return table





if __name__ == "__main__":
    # # For testing
    
    graph = load_graph(GRAPH_PATH)
    
    clusters = load_clusters(CLUSTERS_PATH)
    cluster_dates = load_clusters(DATES_PATH)
    
    cluster_lengths = get_cluster_lengths(graph, clusters)
    
    
    table = fill_table(clusters, cluster_dates, cluster_lengths, CLUSTER_TYPE, graph, CER_0_PATHS, CER_3_PATHS)
    
    
    with open(OUTPUT_CSV_PATH, "w") as out_file:
        
        header = [
            # "Clusters max distance",
            "ID",
            "Size",
            "Length in days",
            "Mean case date",
            "CER with 0 intermediary non-sequenced cases (t_d = 0)",
            "CER with 1 intermediary non-sequenced cases (t_d = 0)",
            "CER with 2 intermediary non-sequenced cases (t_d = 0)",
            "CER with 0 intermediary non-sequenced cases (t_d = 3)",
            "CER with 1 intermediary non-sequenced cases (t_d = 3)",
            "CER with 2 intermediary non-sequenced cases (t_d = 3)",
        ]
        
        out_file.write("\t".join( header ) + "\n")
        
        out_file.write( "\n".join(["\t".join(line) for line in table]) )
        
        



