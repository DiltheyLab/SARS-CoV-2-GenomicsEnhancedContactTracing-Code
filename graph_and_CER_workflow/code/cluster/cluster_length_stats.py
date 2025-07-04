"""

    
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



CLUSTERS_PATH = sys.argv[1]
DATES_PATH = sys.argv[2]
GRAPH_PATH = sys.argv[3]
DM_PATH = sys.argv[4]

CLUSTER_NAME = sys.argv[5]

OUTPUT_TXT_PATH = sys.argv[6]




def cluster_stats(clusters: list, graph: object, name: str, cluster_lengths: list) -> str:
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

        
    biggest_cluster = cluster_lengths.index(max(cluster_lengths))
        
    for case in clusters[biggest_cluster]:
        output_str += "\t".join([ case, graph.nodes[case]["date"] ]) + "\n"
            
    
    
    with open(OUTPUT_TXT_PATH, "w") as out_file:
        out_file.write(output_str)
        




def get_cluster_lengths(graph: object, clusters: list)-> list:
    """
    TODO

    Args:
        graph (object): _description_
        clusters (list): _description_

    Returns:
        list: _description_
    """
    
    
    cluster_lengths = []
    for cluster in clusters:
        cluster_dates = []
        
        for node in cluster:
            date = datetime.strptime(graph.nodes[node]["date"], "%d.%m.%Y")
            cluster_dates.append(date)
            
        first_date = min(cluster_dates)
        last_date = max(cluster_dates)
        
        cluster_lengths.append( (last_date - first_date).days +1 )
            
    return cluster_lengths
    



def clusterlength_per_month_and_casenumbers(cluster: list, dates: list, name: str, graph: object, cluster_lengths: list)-> None:
    """
    TODO

    Args:
        cluster (list): _description_
        dates (list): _description_
        name (str): _description_
        graph (object): _description_
    """
    
    
    # gather data for sizes
    months_clusters = [datetime.strptime(date, "%d.%m.%Y").month for date in dates]
    sizes = [len(c) for c in clusters]
    
    sizes_cutoff = [c if c < 30 else 30 for c in sizes]
    

    
    
    #########
    # plot cluster lengths per month barplot
    
    plt.clf()
    sns.set_theme() 
    
    # cluster sizes
    ax = sns.barplot(x=months_clusters, y=cluster_lengths, color="#595959")
    
    # label stuff
    ax.set_xlabel("Month of 2021")
    # ax2.set_ylabel("Mean cluster size")
    ax.set_ylabel("Cluster length in days")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    ax.set_facecolor('#EAEAEA')
    
    # save normal
    # plt.savefig("plots/clusters/Clusters_"+name+"_mean_per_months.pdf")
    plt.savefig("plots/clusters/lengths/Clusters_"+name+"_length_per_month_bar.svg") #TODO same directory
    
    #########
    # plot cluster lengths per month boxplot
    
    plt.clf()
    sns.set_theme() 
    
    # cluster sizes
    ax = sns.boxplot(x=months_clusters, y=cluster_lengths, color="#595959")
    
    # label stuff
    ax.set_xlabel("Month of 2021")
    # ax2.set_ylabel("Mean cluster size")
    ax.set_ylabel("Cluster length in days")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    ax.set_facecolor('#EAEAEA')
    
    # save normal
    # plt.savefig("plots/clusters/Clusters_"+name+"_mean_per_months.pdf")
    plt.savefig("plots/clusters/lengths/Clusters_"+name+"_length_per_month_box.svg") #TODO same directory

     
    
    #########
    # plot cluster lengths per size barplot
    
    plt.clf()
    sns.set_theme() 
    fig, ax = plt.subplots(figsize=(11, 6))
    
    # cluster sizes
    ax = sns.barplot(x=sizes, y=cluster_lengths, color="#595959")
    
    # label stuff
    ax.set_xlabel("Cluster size")
    # ax2.set_ylabel("Mean cluster size")
    ax.set_ylabel("Cluster length in days")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    ax.set_facecolor('#EAEAEA')
    
    # save normal
    # plt.savefig("plots/clusters/Clusters_"+name+"_mean_per_months.pdf")
    plt.savefig("plots/clusters/lengths/Clusters_"+name+"_length_per_size_bar.svg") #TODO same directory
    
    #########
    # plot cluster lengths per size boxplot
    
    plt.clf()
    sns.set_theme() 
    
    # empty_sizes = [s for s in range(2, max(sizes)) if s not in sizes]
    
    # cluster sizes
    ax = sns.boxplot(x=sizes, y=cluster_lengths, color="#595959")
    # ax = sns.boxplot(x=sizes + empty_sizes, y=cluster_lengths + [0]*len(empty_sizes), color="#595959")
    
    # label stuff
    ax.set_xlabel("Cluster size")
    # ax2.set_ylabel("Mean cluster size")
    ax.set_ylabel("Cluster length in days")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    ax.set_facecolor('#EAEAEA')
    
    # save normal
    # plt.savefig("plots/clusters/Clusters_"+name+"_mean_per_months.pdf")
    plt.savefig("plots/clusters/lengths/Clusters_"+name+"_length_per_size_box.svg") #TODO same directory

     
     
    
    #########
    # plot cluster lengths per size barplot with cutoff
    
    

    
    plt.clf()
    sns.set_theme() 
    fig, ax = plt.subplots(figsize=(11, 6))
    
    # cluster sizes
    ax = sns.barplot(x=sizes_cutoff, y=cluster_lengths, color="#595959")
    
    # label stuff
    ax.set_xlabel("Cluster size")
    # ax2.set_ylabel("Mean cluster size")
    ax.set_ylabel("Cluster length in days")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    # rename last label
    labels = [item.get_text() for item in ax.get_xticklabels()]
    # labels = [l if int(l)%2 == 0 else "" for l in labels]
    labels[-1] = f"≥{30}"
    
    ax.set_facecolor('#EAEAEA')
    ax.set_xticklabels(labels)    
    
    # save normal
    # plt.savefig("plots/clusters/Clusters_"+name+"_mean_per_months.pdf")
    plt.savefig("plots/clusters/lengths/Clusters_"+name+"_length_per_size_bar_cutoff.svg") #TODO same directory
    
    
    
    
    #########
    # plot cluster lengths per size boxplot with cutoff
    
    plt.clf()
    sns.set_theme() 
    
    
    # cluster sizes
    ax = sns.boxplot(x=sizes_cutoff, y=cluster_lengths, color="#595959")
    
    # label stuff
    ax.set_xlabel("Cluster size")
    # ax2.set_ylabel("Mean cluster size")
    ax.set_ylabel("Cluster length in days")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    # rename last label
    labels = [item.get_text() for item in ax.get_xticklabels()]
    # labels = [l if int(l)%2 == 0 else "" for l in labels]
    labels[-1] = f"≥{30}"

    ax.set_xticklabels(labels)
    
    ax.set_facecolor('#EAEAEA')    
    
    # save normal
    # plt.savefig("plots/clusters/Clusters_"+name+"_mean_per_months.pdf")
    plt.savefig("plots/clusters/lengths/Clusters_"+name+"_length_per_size_box_cutoff.svg") #TODO same directory

    
        
    #########
    # plot cluster lengths as histogram
    
    plt.clf()
    sns.set_theme() 
    
    
    values, bins, bars = plt.hist(cluster_lengths, color="#595959", bins = max(cluster_lengths))
    
    # plt.bar_label(bars, labels=[f"{round(100*v/len(rates_list), 1)}%" for v in values], fontsize=10)

    # labels
    plt.xlabel("Cluster length in days")
    plt.ylabel("Count")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    ax.set_facecolor('#EAEAEA')
    
    #save 
    plt.savefig("plots/clusters/lengths/Clusters_"+name+"_histogram_lengths.svg") #TODO same directory
    
    
    

    #########
    # plot day count of a single cluster
    
    biggest_cluster = cluster_lengths.index(max(cluster_lengths))
    
    dates_big_cl = [datetime.strptime(graph.nodes[case]["date"], "%d.%m.%Y") for case in clusters[biggest_cluster]]
    dates_big_cl.sort()
    # dates_big_cl = [graph.nodes[case]["date"][:-5] for case in clusters[biggest_cluster]]
    
    date_set = set(dates_big_cl[0] + timedelta(x) for x in range((dates_big_cl[-1] - dates_big_cl[0]).days))
    missing = sorted(date_set - set(dates_big_cl))
    
    
    d_to_count = defaultdict(int)
    for d in dates_big_cl:
        d_to_count[d] += 1
        
    for m in missing:
        d_to_count[m] = 0    
    
    x,y = zip(*sorted(list(d_to_count.items()), key=lambda x: x[0]))
    x = [x.strftime("%m.%d") for x in x]
    
    ##
    plt.clf()
    sns.set_theme() 
    fig, ax = plt.subplots(figsize=(30, 10))
    
    
    ax = sns.barplot(x=x, y=y,  color="#595959")
    # ax = sns.barplot(x=list(d_to_count.keys()), y=list(d_to_count.values()),  color="#595959")
    ax.set_facecolor('#EAEAEA')
    
    # labels
    #plt.xlabel("Cluster length in days")
    plt.ylabel("Count")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 

    ax.figure.autofmt_xdate(rotation=45)
    plt.tight_layout(pad=2)   
    
    #save 
    plt.savefig("plots/clusters/lengths/Clusters_"+name+"_biggest_cluster_days_count.svg") #TODO same directory
    
    
    
def find_dm_of_biggest_cluster(cluster_lengths:list , clusters: list, graph: object, dm: dict)-> dict:
    """
    TODO

    Args:
        cluster_lengths (list): _description_
        clusters (list): _description_
        graph (object): _description_
        dm (dict): _description_

    Returns:
        dict: _description_
    """
    
    biggest_cluster = cluster_lengths.index(max(cluster_lengths))
    
    # sort the cases by date
    case_date_tuples = [ (case, datetime.strptime(graph.nodes[case]["date"], "%d.%m.%Y")) 
                            for case in clusters[biggest_cluster]]
    case_date_tuples.sort(key=lambda x: x[1])

    # collect all row/column indices
    dm_indexes = [graph.nodes[case]["dm_index"] 
                    for case,_ in case_date_tuples]

    # filter the wanted rows with numpy
    dm_np = np.array(dm["table"])
    dm_np = dm_np[dm_indexes,:][:, dm_indexes]

    case_date_strs = list(map(lambda x: f"{x[0]}, {x[1].strftime('%d.%m')}" , case_date_tuples))

    out_lines = ["\t".join([""]+case_date_strs) + "\n"]
    out_lines += [ case_date_strs[i] + "\t" + "\t".join(map(str, row)) +"\n"
                    for i,row in enumerate(dm_np)]
    
    
    with open("plots/clusters/lengths/dm_"+CLUSTER_NAME+"_biggest_cluster.tsv", "w") as out_file:
        out_file.writelines(out_lines)
    

    
    


if __name__ == "__main__":
    
    graph = load_graph(GRAPH_PATH)
    clusters = load_clusters(CLUSTERS_PATH)
    dates = load_clusters(DATES_PATH)
    dm = load_dm(DM_PATH)
    
    Path(f"plots/clusters/lengths/").mkdir(parents=True, exist_ok=True)
    
    cluster_lengths = get_cluster_lengths(graph, clusters)
    
    # text based output
    cluster_stats(clusters, graph, CLUSTER_NAME, cluster_lengths)


    clusterlength_per_month_and_casenumbers(clusters, dates, CLUSTER_NAME, graph, cluster_lengths)

    find_dm_of_biggest_cluster(cluster_lengths, clusters, graph, dm)
    
    
