"""
    
"""




import sys
import seaborn as sns
import matplotlib as plt

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *



STATS_PATHS = sys.argv[1:-1]

OUTPUT_PATH = sys.argv[-1]




def combine_stat_tables(file_paths: list) -> list:
    """

    Args:
        file_paths (list): _description_

    Returns:
        list: _description_
    """
    
    combined_table = []
    
    
    for stat_fname in file_paths:
        with open(stat_fname,"r") as stat_file:
            
            path_parts = stat_fname.strip().split("/")
            smaller_parts = path_parts[-1].split(".")[0].split("_")
            
            cluster_type = path_parts[-2][-1]
            
            graph_type = smaller_parts[-4]
            jump_type = smaller_parts[-1]
            
            lines = stat_file.readlines()
            
            if combined_table == []:
                combined_table.append("Direction cutoff\tClusters max distance\tJump distance\t" + lines[0])
            
            for line in lines[1:]:
                combined_table.append(f"{graph_type}\t{cluster_type}\t{jump_type}\t" + line)
                
            
    for line in combined_table:
        print(line[:-1])
            
            
    return combined_table





def write_combined_table(table:list, outpath:str) -> None:

    with open(outpath, "w") as out_file:
        out_file.writelines(table)



def plot_cer_differences(table: list) -> None :
    """

    Args:
        table (list): _description_
    """

    # collect the CER values that should be plottet

    #           0               1                   2                   3                   4                       5                   6           7                          
    # Direction cutoff	Clusters max distance	Jump distance	TrueSelbeAdresse	SelbeAdresseNachname	SurvnetAngestecktBei	Outbreak	SurvnetKontaktperson	
    # 8 nodes_with_infectionsource
    # 9 infsource_rate_per_all_cases
    # 10 infsource_rate_per_sequenced_cases
    # 11 infsource_rate_per_cluster_cases   ### use this?
    # 12 additional_genectiv_cases_infsource
    # 13 genetic_infectionsources
    # 14 genetic_infsource_rate_per_all_cases
    # 15 genetic_infsource_rate_per_sequenced_cases
    # 16 genetic_infsource_rate_per_cluster_cases
    # 17 jump_edges_count
    # 18 jump_edges_used
    cer_dict = {}
  
    for line in table[1:]:
        parts = line.strip().split("\t")
        
        if parts[3:8] == ["x","x","x","x","x"]:
    
            cer_dict[f"{parts[0]}_{parts[1]}_{parts[2]}"] = [
                round(float(parts[11]) *100, 2), # per cluster cases
                round(float(parts[10]) *100, 2), # per seq cases
                round(float(parts[9]) *100, 2), # per all cases
            ]
                
            
    
    x = ["0"] *2 +  ["3"] *2
    y = [ cer_dict["0_0_0"][0], cer_dict["0_1_0"][0] ,
          cer_dict["3_0_0"][0], cer_dict["3_1_0"][0]]
    
    hues = [ "Genetic threshold for clustering = 0", "Genetic threshold for clustering ≤ 1"] *2 
    

    
    ################################
    # plot cer differences
    
    
    #########################################TODO
    ######################################### used in paper
    
    
    plt.clf()
    sns.set_theme(style="whitegrid",font_scale=2.4) 
    
    fig, ax = plt.subplots(figsize=(12, 8))
    
    
    ax = sns.barplot(x=x, y=y, hue=hues, palette=["#FC8D62", "#66C2A5"] , saturation=1)
    
    
    ax.bar_label(ax.containers[0], labels=[y[0], y[2]])
    ax.bar_label(ax.containers[1], labels=[y[1], y[3]])
    
    
    ax.set_ylim([0, 50])
    
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    
    plt.ylabel("Cluster explanation rate (in %)")
    plt.xlabel("Directionalization cutoff")

    plt.savefig(f"plots/inf_source_rate/cer_comparison_2.svg")




    ################################
    # plot cer differences
    
     
    plt.clf()
    sns.set_theme(style="whitegrid",font_scale=2.2) 
    
    fig, ax = plt.subplots(figsize=(12, 8))
    
    
    
    x = ["= 0"] *3 +  ["≤ 1"] *3  
    y = [ cer_dict["0_0_0"][2], cer_dict["0_0_0"][1], cer_dict["0_0_0"][0] ,
          cer_dict["0_1_0"][2], cer_dict["0_1_0"][1], cer_dict["0_1_0"][0]]
    
    hues = ["... of all cases", "... of sequenced cases", "... of cases in cluster (CER)"] *2 
    
    
    ax = sns.barplot(x=x, y=y, hue=hues, palette=["#FC8D62", "#66C2A5", "#8DA0CB"] , saturation=1)
    
    
    ax.bar_label(ax.containers[0], labels=[y[0], y[3]])
    ax.bar_label(ax.containers[1], labels=[y[1], y[4]])
    ax.bar_label(ax.containers[2], labels=[y[2], y[5]])
    
    
    ax.set_ylim([0, 50])
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    
    
    plt.ylabel("Cases with putative infection source [%]")
    plt.xlabel("Genetic threshold for clustering")

    plt.savefig(f"plots/inf_source_rate/cer_comparison.svg")




if __name__ == "__main__":
    
    table = combine_stat_tables(STATS_PATHS)
    
    write_combined_table(table, OUTPUT_PATH)
    
    plot_cer_differences(table)