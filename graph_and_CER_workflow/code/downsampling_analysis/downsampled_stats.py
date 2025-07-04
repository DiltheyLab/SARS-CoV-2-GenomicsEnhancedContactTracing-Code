"""

    
"""




import sys
import matplotlib.pyplot as plt
import seaborn as sns
from collections import defaultdict
from pathlib import Path


from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *



CLUSTERS_PATH = sys.argv[1]
RATES_PATH = sys.argv[2]

CLUSTER_NAME = sys.argv[3]

OUTPUT_TXT_PATH = sys.argv[4]



# This should also be pulled into an input config yaml file or something of the sort
dataset_to_group = {
    "subsample_1pc_rep1": ("1%", "rep1" ),
    "subsample_1pc_rep2": ("1%", "rep2" ),
    "subsample_1pc_rep3": ("1%", "rep3" ),
    "subsample_1pc_rep4": ("1%", "rep4" ),
    "subsample_1pc_rep5": ("1%", "rep5" ),
    "subsample_2.5pc_rep1": ("2.5%", "rep1"),
    "subsample_2.5pc_rep2": ("2.5%", "rep2"),
    "subsample_2.5pc_rep3": ("2.5%", "rep3"),
    "subsample_2.5pc_rep4": ("2.5%", "rep4"),
    "subsample_2.5pc_rep5": ("2.5%", "rep5"),
    "subsample_5pc_rep1": ("5%", "rep1"),
    "subsample_5pc_rep2": ("5%", "rep2"),
    "subsample_5pc_rep3": ("5%", "rep3"),
    "subsample_5pc_rep4": ("5%", "rep4"),
    "subsample_5pc_rep5": ("5%", "rep5"),
    "subsample_10pc_rep1": ("10%", "rep1"),
    "subsample_10pc_rep2": ("10%", "rep2"),
    "subsample_10pc_rep3": ("10%", "rep3"),
    "subsample_10pc_rep4": ("10%", "rep4"),
    "subsample_10pc_rep5": ("10%", "rep5"),
    "subsample_15pc_rep2": ("15%", "rep2"),
    "subsample_15pc_rep1": ("15%", "rep1"),
    "subsample_15pc_rep3": ("15%", "rep3"),
    "subsample_15pc_rep4": ("15%", "rep4"),
    "subsample_15pc_rep5": ("15%", "rep5"),
    "subsample_20pc_rep1": ("20%", "rep1"),
    "subsample_20pc_rep2": ("20%", "rep2"),
    "subsample_20pc_rep3": ("20%", "rep3"),
    "subsample_20pc_rep4": ("20%", "rep4"),
    "subsample_20pc_rep5": ("20%", "rep5"),
    "all": ("all", "all"),
}
    
    
 

def basic_cluster_size_plots(clusters: dict, rates: dict, name: str) -> None:
    """

    Args:
        clusters (dict): _description_
        rates (dict): _description_
        name (str): _description_
    """
    
    # collect data    
    sizes = {key:[ len(c) for c in clusters[key]]
                for key in clusters}
    
    
    subsampling_to_clustercount= {dataset_to_group[key][0]:len(clusters[key]) for key in clusters }
    
    size_cutoff = 20
    
    plot_keys = []
    plot_sizes = []
    
    group_to_sizes_cutoff = defaultdict(list)
    
    violin_subsampling_pc = []
    violin_size = []
    for key, vals in sizes.items():
        # print(vals)
        for size in vals:
            plot_keys.append( dataset_to_group[key][0] )
            plot_sizes.append(size)
            
            
            group_to_sizes_cutoff[dataset_to_group[key]].append(size if size < size_cutoff else size_cutoff)
            
            violin_size.append(size if size < size_cutoff else size_cutoff)
            violin_subsampling_pc.append(dataset_to_group[key][0])


    
    #########
    # plot with size cutoff

    plt.clf()
    sns.set_theme(font_scale=2) 
    fig, ax = plt.subplots(figsize=(11, 7))
    
    
    size_and_group_to_count = defaultdict(lambda : defaultdict(int))
    
    for g, sizes in group_to_sizes_cutoff.items():
        for s in sizes:
            size_and_group_to_count[ (g[0], s) ][ g[1] ] += 1
            
    
        
    x = []
    y = []
    y_per_clustercases = []
    hue = []
    for k, v in size_and_group_to_count.items():
        g,s = k
        for k2, count in v.items():
            x.append(s)
            y.append(count)
            hue.append(g)
    
            y_per_clustercases.append(count / subsampling_to_clustercount[g])
    
    
    ax = sns.pointplot(x=x, y=y, hue=hue, palette=["#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494"])
    
    plt.xlabel("Cluster sizes")
    plt.ylabel("Count")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    plt.legend(title="Subsampling percentage")
    
    # rename last label
    labels = [item.get_text() for item in ax.get_xticklabels()]
    labels[-1] = f"≥{size_cutoff}"
    ax.set_xticklabels(labels)
    
    # ax.figure.autofmt_xdate(rotation=45)
    ax.set_facecolor('#EAEAEA') 
    
    plt.tight_layout()
    plt.savefig("plots/downsampling/Clusters_"+name+"_cluster_sizes_catplot.svg")
    
    
    
        
    #########
    # plot with size cutoff per cases in cluster

    plt.clf()
    sns.set_theme(font_scale=2) 
    fig, ax = plt.subplots(figsize=(19, 10))
    
    ax = sns.pointplot(x=x, y=y_per_clustercases, hue=hue, palette=["#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494"])
    
    plt.xlabel("Cluster sizes")
    plt.ylabel("Count / cases in clusters")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    plt.legend(title="Subsampling percentage")
    
    # rename last label
    labels = [item.get_text() for item in ax.get_xticklabels()]
    labels[-1] = f"≥{size_cutoff}"
    ax.set_xticklabels(labels)
    
    ax.set_facecolor('#EAEAEA')
    
    plt.tight_layout()

    plt.savefig("plots/downsampling/Clusters_"+name+"_cluster_sizes_per_clustercases_catplot.svg")
    
    plt.ylim(0,0.03)
    plt.savefig("plots/downsampling/Clusters_"+name+"_cluster_sizes_per_clustercases_more_detail_catplot.svg")
    
    
    
        
    #########
    # plot with size cutoff as violinplot

    plt.clf()
    sns.set_theme(font_scale=2) 
    fig, ax = plt.subplots(figsize=(19, 10))
    ax.set_facecolor('#EAEAEA')
    
    
    ax = sns.violinplot(x=violin_subsampling_pc, y=violin_size, inner="quart", color="#595959")
    
        
    plt.ylabel(f"Cluster sizes (cut-off {size_cutoff})")
    plt.xlabel("Subsampling percentage")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    plt.ylim(0,size_cutoff)
    
    plt.tight_layout()

    plt.savefig("plots/downsampling/Clusters_"+name+"_cluster_sizes_violinplot.svg")
    
    
    
    #########
    # plot Cases in cluster / sequenced cases

    plt.clf()
    sns.set_theme(font_scale=1.7) 
    fig, ax = plt.subplots(figsize=(9, 6))
    
    x = []
    y = []
    for subsample_name, rate_dict in list(rates.items())[::-1]:
        x.append(dataset_to_group[subsample_name][0])
        y.append(rate_dict["cluster_case_count"] / rate_dict["sequenced_case_count"])
    
    
    
    ax = sns.barplot(x=x, y=y, color="#595959")
    


    plt.xlabel("Subsampling percentage")
    plt.ylabel("Cases in clusters / sequenced cases")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    
    ax.set_facecolor('#EAEAEA')
    plt.tight_layout()

    plt.savefig("plots/downsampling/Clusters_"+name+"_cluster_cases_per_seq_cases.svg")
    
    
    


    
def cer_plots(rates: dict, name:str) -> None:        
    """

    Args:
        rates (dict): _description_
        name (str): _description_
    """
    
        
    x = []
    y_cer = []
    y_per_seq_cases = []
    y_gen_per_seq_cases = []
    y_infection_sources = []
    for subsample_name, rate_dict in list(rates.items())[::-1]:
        x.append(dataset_to_group[subsample_name][0])
        y_cer.append(rate_dict["infsource_rate_per_cluster_cases"])
        y_per_seq_cases.append(rate_dict["infsource_rate_per_sequenced_cases"])
        y_gen_per_seq_cases.append(rate_dict["genetic_infsource_rate_per_sequenced_cases"])
        y_infection_sources.append(rate_dict["nodes_with_infectionsource"])
    

    
    #########
    # plot CER
    
    plt.clf()
    sns.set_theme(font_scale=2.2) 
    fig, ax = plt.subplots(figsize=(10, 7))
    
    
    ax = sns.barplot(x=x, y=y_cer, color="#595959")
    plt.xlabel("Subsampling percentage")
    plt.ylabel("CER [%]")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    ax.set_facecolor('#EAEAEA')
    
    
    plt.tight_layout()

    plt.savefig("plots/downsampling/CER/infsources_per_clustercases_"+name+".svg")
    
    
    
    #########
    # plot infsources per seq cases

    plt.clf()
    sns.set_theme(font_scale=2.2) 
    fig, ax = plt.subplots(figsize=(10, 7))
    
    
    ax = sns.barplot(x=x, y=y_per_seq_cases, color="#595959")
    plt.xlabel("Subsampling percentage")
    
    plt.ylabel("Proportion sequenced cases with\nputative infection source [%]")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    plt.tight_layout()
    ax.set_facecolor('#EAEAEA')

    plt.savefig("plots/downsampling/CER/infsources_per_sequenced_cases_"+name+".svg")

    # with open("plots/downsampling/CER/infsources_per_sequenced_cases_"+name+".tsv", "w") as outfile:
    #     outfile.write("subsampling\tinf_sources_per_seq_cases\n")
    #     for x_val, y_val in zip(x, y_per_seq_cases):
    #         outfile.write(f"{x_val}\t{y_val}\n")
    
    
    #########
    # plot genetic_infsource_rate_per_sequenced_cases

    plt.clf()
    sns.set_theme(font_scale=2) 
    fig, ax = plt.subplots(figsize=(10, 7))
    
    
    ax = sns.barplot(x=x, y=y_gen_per_seq_cases, color="#595959")
    plt.xlabel("Subsampling percentage")
    
    plt.ylabel("Proportion sequenced cases with\nputative infection source including\ngenetic connections [%]")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    plt.tight_layout()
    ax.set_facecolor('#EAEAEA')

    plt.savefig("plots/downsampling/CER/genetic_infsources_per_sequenced_cases_"+name+".svg")
    
    
    #########
    # plot cases with infection source

    plt.clf()
    sns.set_theme(font_scale=2.2) 
    fig, ax = plt.subplots(figsize=(10, 7))
    
    
    ax = sns.barplot(x=x, y=y_infection_sources, color="#595959")
    plt.xlabel("Subsampling percentage")
    plt.ylabel("Cases with putative infection sources")
    
    title_symbol = "=" if CLUSTER_NAME[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_NAME[-1]}") 
    
    plt.tight_layout()
    ax.set_facecolor('#EAEAEA')

    plt.savefig("plots/downsampling/CER/infsources_"+name+".svg")
    



if __name__ == "__main__":
    
    clusters = load_clusters(CLUSTERS_PATH)
    rates = load_clusters(RATES_PATH)
    
    Path(f"plots/downsampling/").mkdir(parents=True, exist_ok=True)
    Path(f"plots/downsampling/CER").mkdir(parents=True, exist_ok=True)
    
    
    basic_cluster_size_plots(clusters, rates, CLUSTER_NAME)
    
    
    cer_plots(rates, CLUSTER_NAME)
    
    # create an output file for snakemake
    with open(OUTPUT_TXT_PATH, "w") as outfile:
        outfile.write("not needed")
    

