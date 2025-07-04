"""

    
"""




from datetime import timedelta
import sys
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from collections import defaultdict
from pathlib import Path

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *



CLUSTERS_PATH = sys.argv[1]
DATES_PATH = sys.argv[2]
GRAPH_PATH = sys.argv[3]

CLUSTER_NAME = sys.argv[4]

OUTPUT_TXT_PATH = sys.argv[5]


def cluster_stats(clusters: list, graph: object, name: str) -> str:
    """
    Collect stats about clusters and save them in text format

    Args:
        clusters (list): list of lists with clusters
        graph (object): nx Multigraph object
        name (str): name of the cluster type

    Returns:
        str: String containing stats
    """
    
    output_str = f"\n\n{name} stats: \n"
    
    sizes = [len(cluster) for cluster in clusters]
    cases_total = sum(sizes)
    sequenced_cases = sum([1 for node in graph.nodes
                                if graph.nodes[node]["sample_id"] != ""])

    
    # x clusters
    output_str += f"{len(clusters)} clusters\n" 
    
    # x cases total
    output_str += f"{cases_total} cases total \n" 
    
    # x% of all sequenced cases
    output_str += f"{cases_total/sequenced_cases} % of all sequenced cases {sequenced_cases}\n" 
    
    # x and y isolates, median and average per-cluster sizes
    output_str += f"x={np.median(sizes)} and y={np.mean(sizes)} isolates, median and average per-cluster sizes\n" 


    return output_str + "\n\n"



    
    

def clustersize_per_month_and_casenumbers(cluster: list, dates: list, name: str, graph: object)-> None:
    """
    TODO

    Args:
        cluster (list): _description_
        name (str): _description_
        graph (object): _description_
    """
    
    # gather data for sizes
    months_clusters = [datetime.strptime(date, "%d.%m.%Y").month for date in dates]
    sizes = [len(c) for c in clusters]
    
    
    # total_size_per_month = [sum(sizes_per_month[month]) for month in sizes_per_month]
    total_size_per_month = defaultdict(int)
    for cluster in clusters:
        for node in cluster:
            month = datetime.strptime(graph.nodes[node]["date"], "%d.%m.%Y").month
            total_size_per_month[month] += 1
    
    # total_size_per_month_list = [val for key, val in sorted(list(total_size_per_month.items()), key=lambda x: x[0])]
    
    # gather data for case numbers
    cases_per_month = defaultdict(int)
    seqcases_per_month = defaultdict(int)
    for node in graph.nodes:
        month = datetime.strptime(graph.nodes[node]["date"], "%d.%m.%Y").month
        
        cases_per_month[month] += 1 
        
        if graph.nodes[node]["sample_id"] != "":
            seqcases_per_month[month] += 1
    
    cases_per_month_list = [val for key, val in sorted(list(cases_per_month.items()), key=lambda x: x[0])]
    seqcases_per_month_list = [val for key, val in sorted(list(seqcases_per_month.items()), key=lambda x: x[0])]
    
 
    #########
    # plot mean cluster sizes per month with case numbers
    
    #########################################TODO
    ######################################### used in paper
    
    plt.clf()
    sns.set_theme(font_scale=2.2) 
    fig, ax = plt.subplots(figsize=(9, 6))
    
    
    
    # cluster sizes
    ax = sns.barplot(x=list(range(2,13)), y=cases_per_month_list, color="#FC8D62",width=.95, label = "Cases")

    # case numbers
    ax2 = ax.twinx()
    pl2 = sns.barplot(x=months_clusters, y=sizes, width=.6, color="#66C2A5", ax=ax2,  label="Mean cluster size")
    
    # set cluster sizes in foreground
    ax.set_zorder(9)
    ax2.set_zorder(10)
    
    ax.grid(True, axis='both')
    ax2.grid(False)
        
    # add legend
    ax.get_legend().remove()
    ax2.get_legend().remove()
    h1, l1 = ax.get_legend_handles_labels()
    h2, l2 = ax2.get_legend_handles_labels()
    ax2.legend(h1+h2, l1+l2, loc=2)
    
    ax.set_facecolor('#EAEAEA')
    
    # label stuff
    ax.set_xlabel("Month of 2021")
    ax2.set_ylabel("Mean cluster size")
    ax.set_ylabel("Cases")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    plt.tight_layout()
    
    # plt.title(name)#+",\n  Mean cluster sizes per month in context of case counts")
    
    # save normal
    # plt.savefig("plots/clusters/Clusters_"+name+"_mean_per_months.pdf")
    plt.savefig("plots/clusters/Clusters_"+name+"_mean_per_months.svg")

 


    
 

def basic_cluster_size_plots(clusters, name) -> None:
    """
    TODO

    Args:
        clusters (_type_): _description_
        name (_type_): _description_

    Returns:
        list: _description_
    """
    
    # collect data
    sizes = [len(c) for c in clusters]
    max_size = max(sizes)
    
    # everything equal and larger gets pooled together
    size_cutoff = 30
    
    # cut off the cluster sizes at size_cutoff and pool all larger ones together
    sizes_cutoff = [size_cutoff if size>=size_cutoff else size 
                        for size in sizes]
            
    
    
    #########
    # plot with size cutoff

    #########################################TODO
    ######################################### used in paper
    
    plt.clf()
    sns.set_theme(font_scale=2.2) 
    fig, ax = plt.subplots(figsize=(9, 6))
    
    
    
    
    x_positions = list(range(2, size_cutoff+1))
    y_vals = [sizes_cutoff.count(x) for x in x_positions]
    
    ax = sns.barplot(x=x_positions, y=y_vals, color="#595959")

    plt.xlabel("Cluster Size")
    plt.ylabel("Count")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 

    # rename last label
    labels = [item.get_text() for item in ax.get_xticklabels()]
    # labels = [l if int(l)%2 == 0 else "" for l in labels]
    labels[-1] = f"≥{size_cutoff}"

    ax.set_facecolor('#EAEAEA')
    ax.set_xticklabels(labels)
    
    
    # set label visibility
    for i,label in enumerate(ax.xaxis.get_ticklabels()):
        label.set_visible(False)
        if i % 2 == 0:  #keep the even ticks 
            label.set_visible(True)
    
    plt.tight_layout()

    plt.savefig("plots/clusters/Clusters_"+name+"_cutoff30.svg")
    




if __name__ == "__main__":
    
    graph = load_graph(GRAPH_PATH)
    clusters = load_clusters(CLUSTERS_PATH)
    dates = load_clusters(DATES_PATH)
    
    Path(f"plots/clusters/").mkdir(parents=True, exist_ok=True)
    
    # text based output
    cluster_stats = cluster_stats(clusters, graph, CLUSTER_NAME)
    with open(OUTPUT_TXT_PATH, "w") as out_file:
        out_file.write(cluster_stats)
    
    
    basic_cluster_size_plots(clusters, CLUSTER_NAME)
        
    clustersize_per_month_and_casenumbers(clusters, dates, CLUSTER_NAME, graph)
    
    
        
    