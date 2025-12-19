"""
    
"""




import sys
from datetime import datetime
import matplotlib.pyplot as plt
import seaborn as sns
from collections import defaultdict


from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))


from util_functions import *



RATES_PATH = sys.argv[1]
INFECTIONSOURCE_GRAPH_PATH = sys.argv[2]
ADD_GENETIC_GRAPH_PATH = sys.argv[3]
CLUSTERS_PATH = sys.argv[4]
DATES_PATH = sys.argv[5]

CLUSTER_NAME = sys.argv[6]
JUMP_DISTANCE = sys.argv[7]
GRAPH_TYPE = sys.argv[8]

OUTPUT_PATH = sys.argv[9]





def plot_rates(rates: list, clusters:list, graph: object, dates: list, cluster_name: str, edge_types: str) -> None:
    """TODO

    Args:
        rates (list): _description_
        clusters (list): _description_
        graph (object): _description_
        dates (list): _description_
        cluster_name (str): _description_
        edge_types (str): _description_
    """
    
    # collect values
    
    # sort rates into cluster
    
    rates_list = rates["rates_per_cluster"]
        
    cluster_sizes = [len(c) for c in clusters]
        
    
    rates_per_size = defaultdict(list)
    for rate, cluster_size in zip(rates_list, cluster_sizes):
        rates_per_size[cluster_size].append(rate)
    
    
    # everything equal and larger gets pooled together
    size_cutoff = 14
    
    # cut off the cluster sizes at size_cutoff and pool all larger ones together
    sizes_cutoff = []
    for size, rate in zip(cluster_sizes, rates_list):
        if size >= size_cutoff:
            sizes_cutoff.append(size_cutoff+1)
        else:
            sizes_cutoff.append(size)
            
    # collect all cluster lengths        
    cluster_lengths = []
    for cluster in clusters:
        cluster_dates = []
        
        for node in cluster:
            date = datetime.strptime(graph.nodes[node]["date"], "%d.%m.%Y")
            cluster_dates.append(date)
            
        first_date = min(cluster_dates)
        last_date = max(cluster_dates)
        
        cluster_lengths.append( (last_date - first_date).days +1 )
                    
    
    
    ################################
    # plot rate as histogram
    
    #########################################TODO
    ######################################### used in paper
    
    plt.clf()
    sns.set_theme(style="whitegrid",font_scale=2.2) 
    fig, ax = plt.subplots(figsize=(9, 6))
    plt.tight_layout()
    
    
    values, bins, bars = plt.hist(rates_list, color="#595959")#, bins = 40)
    
    plt.bar_label(bars, labels=[f"{round(100*v/len(rates_list), 1)}%" for v in values], fontsize=18)
    print("Bars: ", bars)
    print(values)
    print(bins)

    plt.xlabel("Cluster explanation rate")
    plt.ylabel("Count")
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    plt.savefig(f"plots/inf_source_rate/{cluster_name}/{GRAPH_TYPE}/{edge_types}/cluster_explanation_rate_{JUMP_DISTANCE}.svg", bbox_inches='tight')
        

    
    ################################
    # plot rate per cluster_size
    
    
    #########################################TODO
    ######################################### used in paper
    
    
    plt.clf()
    sns.set_theme(style="whitegrid",font_scale=2.2) 
    fig, ax = plt.subplots(figsize=(9, 6))
    plt.tight_layout()
    
            
    # fill all gaps if there are any
    sizes_filled_cutoff = sizes_cutoff.copy()
    rates_filled = rates_list.copy()
    for i in range(0, size_cutoff+1):
        if i not in sizes_cutoff:
            sizes_filled_cutoff.append(i)
            rates_filled.append(0)



    # cluster sizes
    ax = sns.barplot(x=sizes_filled_cutoff, y=rates_filled, color="#595959", saturation=1)
    
    # lock y axis to 0-1
    ax.set_ylim([0, 0.7])

    # custom ticks
    ticks = list(range(2,size_cutoff))+[size_cutoff+1]
    ax.set_xticks(ticks)

    dic = { size_cutoff+1 : f"≥ {size_cutoff}"}
    # either the number or the custom tick in dic
    labels = [ticks[i] if t not in dic.keys() else dic[t] for i,t in enumerate(ticks)]
    ax.set_xticklabels(labels)
    
    # labels
    # plt.title("Average cluster explanation rates per cluster size")
    plt.xlabel("Cluster sizes")
    plt.ylabel("Cluster explanation rate")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 

    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    
    plt.savefig(f"plots/inf_source_rate/{cluster_name}/{GRAPH_TYPE}/{edge_types}/cluster_explanation_rate_per_cluster_size_{JUMP_DISTANCE}.svg", bbox_inches='tight')
    
    
    
    ################################
    # plot rate per month
    
    
    #########################################TODO
    ######################################### used in paper
    
    
    plt.clf()
    sns.set_theme(style="whitegrid",font_scale=2.2) 
    fig, ax = plt.subplots(figsize=(9, 6))
    plt.tight_layout()
    
    months_clusters = [datetime.strptime(date, "%d.%m.%Y").month for date in dates]

    ax = sns.barplot(x=months_clusters, y=rates_list, color="#595959", saturation=1)

    # lock y axis to 0-1
    ax.set_ylim([0, 0.7])
    
    # labels
    # plt.title(cluster_name+",\n  Average cluster explanation rates per month")
    plt.xlabel("Month of 2021")
    plt.ylabel("Cluster explanation rate")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    plt.savefig(f"plots/inf_source_rate/{cluster_name}/{GRAPH_TYPE}/{edge_types}/cluster_explanation_rate_per_month_{JUMP_DISTANCE}.svg", bbox_inches='tight')
    # plt.savefig(f"plots/cl_ex_rate/{cluster_name}/{GRAPH_TYPE}/{edge_types}/cluster_explanation_rate_per_month_{JUMP_DISTANCE}.pdf")
    
    


    
def count_edgetypes_per_component(rates: list, graph: object, edge_types: str) -> None:
    """
    TODO
    """
    
    
    edgetypes_to_count = defaultdict(int)
    
    for u,v,data in graph.edges(data=True):
        if data["type"] == "Jump_edge":
            continue
        edgetypes_to_count[data["type"]] += 1

    
    ################################
    # plot edgetype count
    
    #########################################TODO
    ######################################### used in paper
        
    plt.clf()
    sns.set_theme(style="whitegrid",font_scale=1.8) 
    fig, ax = plt.subplots(figsize=(11.5, 7))
    plt.tight_layout()
    
    # TODO this should ultimately not be done
    german_to_english_name = {
        "SelbeAdresse" : "Same adress",
        "True_SelbeAdresse" : "SameAddress\n-differentName",
        "SelbeAdresseNachname" : "SameAddress\n-sameName",
        "SurvnetAngestecktBei" : "Backward\ncontact tracing",
        "SurvnetKontaktperson" : "Forward\ncontact tracing",
        "Outbreak" : "Outbreak",
    }


    vals = [count for _, count in sorted(list(edgetypes_to_count.items()), key= lambda x: x[1])[::-1]]
    keys = [german_to_english_name[key] for key, _ in sorted(list(edgetypes_to_count.items()), key= lambda x: x[1])[::-1]]
    
    
    ax = sns.barplot(x=keys, y=vals, color="#595959", saturation=1)
    
    sum_edges = sum(list( edgetypes_to_count.values() ))
    percentage_strings = [ f"{ round(100 * v / sum_edges, 2) }%"  for v in vals ]
    
    ax.bar_label(ax.containers[0], labels=percentage_strings, fontsize=23, label_type='edge', padding=1)
    
    
    plt.tight_layout(pad=2)   
    
    # plt.xlabel("Edge type")
    plt.ylabel("Count")
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    
    plt.ylim(0,1000 if max(vals) < 1000 else 1200)
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 

    Path(f"plots/inf_source_rate/{CLUSTER_NAME}/{GRAPH_TYPE}/{edge_types}/").mkdir(parents=True, exist_ok=True)
    plt.savefig(f"plots/inf_source_rate/{CLUSTER_NAME}/{GRAPH_TYPE}/{edge_types}/Count_per_edgetype_from_connected_components_{JUMP_DISTANCE}.svg", bbox_inches='tight')



if __name__ == "__main__":
    
    graph = load_graph(INFECTIONSOURCE_GRAPH_PATH)
    rates = load_clusters(RATES_PATH)
    dates = load_clusters(DATES_PATH)
    clusters = load_clusters(CLUSTERS_PATH)
    
    edge_types = "TA_AN_IB_O_KP"
        
    # create plot directory if it did not exist yet
    Path(f"plots/cl_ex_rate/{CLUSTER_NAME}/{GRAPH_TYPE}/{edge_types}/").mkdir(parents=True, exist_ok=True)
    
    
    if CLUSTER_NAME.startswith("connected_components_"):
        count_edgetypes_per_component(rates, graph, edge_types)
    plot_rates(rates, clusters, graph, dates, CLUSTER_NAME, edge_types)
    
    
    with open(OUTPUT_PATH, "w") as out_file:
        out_file.write("Nothing to see here")