"""
    
"""




import sys
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))


from util_functions import *



RATES_STATS_PATHS = sys.argv[1:-4]

JUMP_DISTANCE = sys.argv[-4]
GRAPH_TYPE = sys.argv[-3]
CLUSTER_TYPE = sys.argv[-2]

OUTPUT_PATH = sys.argv[-1]



def combined_stats(file_paths: list) -> dict:
    """

    Args:
        file_paths (list): _description_

    Returns:
        dict: _description_
    """
    
    comb_to_stats = {}
    
    for stat_fname in file_paths:
        
        # read json input
        stats = load_clusters(stat_fname)
        
        type_comb = stat_fname.split("/")[-2]
        
        comb_to_stats[type_comb] = (
            stats["nodes_with_infectionsource"],
            stats["infsource_rate_per_all_cases"],
            stats["infsource_rate_per_sequenced_cases"],
            stats["infsource_rate_per_cluster_cases"],
            
            stats["additional_genectiv_cases_infsource"],
            stats["genetic_infectionsources"],
            stats["genetic_infsource_rate_per_all_cases"],
            stats["genetic_infsource_rate_per_sequenced_cases"],
            stats["genetic_infsource_rate_per_cluster_cases"],
            
            stats["jump_edges_count"],
            stats["jump_edges_used"],
            
        )
        
    return comb_to_stats





def write_all_combs(comb_to_stats, outpath):

    shortforms = ["TA", "AN", "IB", "O", "KP"]
    combs_ordering = [["x" if s in comb.split("_") else ""
                        for s in shortforms] 
                            for comb in comb_to_stats]
    out_lines = [
        "TrueSelbeAdresse\tSelbeAdresseNachname\tSurvnetAngestecktBei\tOutbreak\tSurvnetKontaktperson\t" +
        "nodes_with_infectionsource\t"+
        "infsource_rate_per_all_cases\t"+
        "infsource_rate_per_sequenced_cases\t"+
        "infsource_rate_per_cluster_cases\t"+
        "additional_genectiv_cases_infsource\t"+
        "genetic_infectionsources\t"+
        "genetic_infsource_rate_per_all_cases\t"+
        "genetic_infsource_rate_per_sequenced_cases\t"+
        "genetic_infsource_rate_per_cluster_cases\t"+
        "jump_edges_count\t"+
        "jump_edges_used\n"
    ]
    
    for ordering, key_val in zip(combs_ordering, comb_to_stats.items()):
        comb, stats = key_val
        
        # rate, jump_count, used_edges= stats
        print( ordering, comb, *stats)
        
        
        columns = [str(x) for x in ordering+list(stats)]
        
        out_lines.append("\t".join(columns) + "\n")


    with open(outpath, "w") as out_file:
        out_file.writelines(out_lines)








def plot_combined_stats(comb_to_stats, outpath):
    
        

    ################################
    # plot max rate without the edgetype for each edgetype
    
    #########################################TODO
    ######################################### used in paper
            
    plt.clf()
    sns.set_theme(style="whitegrid",font_scale=1.8) 
    fig, ax = plt.subplots(figsize=(10, 7))
    plt.tight_layout()
    
    
    max_comb = sorted(list(comb_to_stats.keys()), key=lambda x:len(x), reverse=True)[0]
    
    value = [comb_to_stats[max_comb][3], 
             max([comb_to_stats[key][3] for key in comb_to_stats if "IB" not in key.split("_") ]),
             max([comb_to_stats[key][3] for key in comb_to_stats if "O" not in key.split("_") ]),
             max([comb_to_stats[key][3] for key in comb_to_stats if "KP" not in key.split("_") ]),
             max([comb_to_stats[key][3] for key in comb_to_stats if "TA" not in key.split("_") ]),
             max([comb_to_stats[key][3] for key in comb_to_stats if "AN" not in key.split("_") ]),
             max([comb_to_stats[key][3] for key in comb_to_stats if "AN" not in key.split("_") and "TA" not in key.split("_") ]),
             ]
    label = [
        "All connections",
        "Without backward contact tracing",
        "Without outbreak",
        "Without forward contact tracing",
        "Without sameAddress-differentName",
        "Without sameAddress-sameName",
        "Without any sameAdress information" 
    ]
    
    
    ax = sns.barplot(x=label, y=value, color="#595959", saturation=1)
    
    
    bar_labels = [f"{round(v*100,2)}%" for v in value]
    
    ax.bar_label(ax.containers[0], labels=bar_labels, fontsize=19, label_type='edge', padding=1)
    
    ax.figure.autofmt_xdate(rotation=45)
    plt.tight_layout(pad=2)   
    
    plt.ylim(0,0.5)
    
    
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    
    plt.ylabel("cluster explanation rate")
    
    title_symbol = "=" if CLUSTER_TYPE[-1] == "0" else "≤"
    plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_TYPE[-1]}") 


    Path(f"plots/inf_source_rate/{CLUSTER_TYPE}/{GRAPH_TYPE}/").mkdir(parents=True, exist_ok=True)
    plt.savefig(f"plots/inf_source_rate/{CLUSTER_TYPE}/{GRAPH_TYPE}/max_rate_per_edgetype_{JUMP_DISTANCE}.svg", bbox_inches='tight')
    



if __name__ == "__main__":

    comb_to_stats = combined_stats(RATES_STATS_PATHS)
    
    write_all_combs(comb_to_stats, OUTPUT_PATH)
    
    plot_combined_stats(comb_to_stats, OUTPUT_PATH)