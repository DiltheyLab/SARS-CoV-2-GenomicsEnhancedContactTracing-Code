"""
    
"""




import sys
from datetime import datetime
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from collections import Counter
from collections import defaultdict

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *

DM_PATH = sys.argv[1]
GRAPH_PATH = sys.argv[2]
NODES_PATH = sys.argv[3]
EDGES_PATH = sys.argv[4]

OUTPUT_PATH = sys.argv[5]

    


def check_symptom_starts_simple(in_fname: str, graph: object) -> None:
    """
    Plot how many cases have symtpmstarts per month
    and the distances between symptomstart date and meldedatum
    
    Args:
        in_fname (str): _description_
        graph (object): nx Multigraph object
    """
    
    with open(in_fname, "r") as in_file:
        lines = in_file.readlines()
    
    
    differences = []
    all_differences = []
    differences_2021 = []
    differences_2021_filtered = []
    
    # gather data for case numbers
    cases_per_month_total = defaultdict(int)
    cases_per_month_with = defaultdict(int)
    cases_per_month_total_graph = defaultdict(int)
    cases_per_month_with_graph = defaultdict(int)
    
    
    for line in lines[1:]:
        parts=line.split("\t")
        
        # date from health department
        meldedatum_str = parts[2].split()[0]
        meldedatum_date = datetime.strptime(meldedatum_str, "%d.%m.%Y")
        
        cases_per_month_total[f"{meldedatum_date.year}_{meldedatum_date.month}"] +=1
        if parts[0] in graph.nodes:
            cases_per_month_total_graph[f"{meldedatum_date.year}_{meldedatum_date.month}"] +=1
            
        
        symptomstart_str = parts[12].strip()
        
        if symptomstart_str == "":
            continue
        symptomstart = datetime.strptime(symptomstart_str, "%d.%m.%Y")
        
        diff = (meldedatum_date - symptomstart).days
        all_differences.append(diff)
        
        if parts[0].startswith("CVD2021"):
            differences_2021.append(diff)
            
        if abs(diff) > 14:
            continue
        
        differences.append( diff )
        
        if parts[0] in graph.nodes:
            differences_2021_filtered.append(diff)
            cases_per_month_with_graph[f"{meldedatum_date.year}_{meldedatum_date.month}"] +=1
        
        cases_per_month_with[f"{meldedatum_date.year}_{meldedatum_date.month}"] +=1
    

    
    
    ################################
    # plot differences_2021_filtered
    
    
    
    ################################################################### TODO paper
    plt.clf()
    sns.set_theme(font_scale=2) 
    fig, ax = plt.subplots(figsize=(9, 7))
       
    

    diffs = []
    counts = []
    counter = Counter(differences_2021_filtered)
    for x in range(min(differences_2021_filtered), max(differences_2021_filtered)):
        diffs.append(x)
        if x in counter:        
            counts.append(counter[x])
        else:
            counts.append(0)
            

    ax = sns.barplot(x=diffs, y=counts, color="#595959")
    
    for i,label in enumerate(ax.xaxis.get_ticklabels()):
        label.set_visible(False)
        if (i) % 2 == 0:
            label.set_visible(True)


    mean_diff = np.mean(differences_2021_filtered)
    std_diff = np.std(differences_2021_filtered)
    plt.title(f"mean: {round(mean_diff, 2)}, std: {round(std_diff, 2)}, cases: {len(differences_2021_filtered)} of {len(graph.nodes)}")


    ax.set_facecolor('#EAEAEA')
    
    # labels
    plt.xlabel("Date difference in days \n(registration date - symptom onset date) ")
    plt.ylabel("Count")
    plt.tight_layout()

    

    plt.savefig(f"plots/symptom_start/ga_dates_vs_symptomstart_graph.svg")
    


def check_pairwise_datediffs(graph: object, dm: dict, edges_path: str) -> None :
    """
    TODO

    Args:
        graph (object): _description_
        dm (dict): _description_
        edges_path (str): path to edges files -> used to get Infected by direction
    """
    
    infected_by_pairs = []
    with open(edges_path, "r") as edge_file:
        # skip header
        for line in edge_file.readlines()[1:]:
            line =  line.split("\t")
            if line[2] == "SurvnetAngestecktBei":
                infected_by_pairs.append(line[:2])
    
    
    
    edgetype_to_distances_mel = defaultdict(list)
    edgetype_to_distances_sym = defaultdict(list)
    
    for edge in graph.edges(data=True):
        
        # if a node has no sequence ignore the edge
        if graph.nodes[edge[0]]["sample_id"] == "":
            continue
        if graph.nodes[edge[1]]["sample_id"] == "":
            continue
                
        # if the edge has a distance higher then 0 ignore it
        idx_1 = graph.nodes[edge[0]]["dm_index"]
        idx_2 = graph.nodes[edge[1]]["dm_index"]
        if dm["table"][idx_1][idx_2] > 1:
            continue
    
        
        # collect date differences
        
        # get date strings
        meldedatum_str_1 = graph.nodes[edge[0]]["date"]
        meldedatum_str_2 = graph.nodes[edge[1]]["date"]
        symptomstart_str_1 = graph.nodes[edge[0]]["symptom_start"]
        symptomstart_str_2 = graph.nodes[edge[1]]["symptom_start"]
        
        # to date object
        meldedatum_date_1 = datetime.strptime(meldedatum_str_1, "%d.%m.%Y")
        meldedatum_date_2 = datetime.strptime(meldedatum_str_2, "%d.%m.%Y")
                
        # differences
        diff_mel = abs( (meldedatum_date_1 - meldedatum_date_2).days ) 
        diff_sym = None
        
        # if the edge has no symtomstart then keep difference as -1. these will be filtered out later
        if symptomstart_str_1 != ""  and symptomstart_str_2 != "":
            # date object 
            symptomstart_date_1 = datetime.strptime(symptomstart_str_1, "%d.%m.%Y")
            symptomstart_date_2 = datetime.strptime(symptomstart_str_2, "%d.%m.%Y")
            
            # date difference
            diff_sym = abs( (symptomstart_date_1 - symptomstart_date_2).days)
            
            # dont do abs for Infected by edges
            # use the direction given by the input data
            if edge[2]["type"] == "SurvnetAngestecktBei":
                
                if edge[:2] in infected_by_pairs:
                    diff_sym = (symptomstart_date_1 - symptomstart_date_2).days
                else:
                    diff_sym = (symptomstart_date_2 - symptomstart_date_1).days
            
            
        
        # add differences of basic edge types
        edgetype_to_distances_mel[edge[2]["type"]].append(diff_mel)
        edgetype_to_distances_sym[edge[2]["type"]].append(diff_sym)
        
        
        not_wanted_edgetypes = ["SelbeAdresse", "SelbeAdresseNachname", "Outbreak"]
        
        # for KP and IB if there are no address or outbreak edges at this edge
        # then add to dict with own key
        if (edge[2]["type"] == "SurvnetKontaktperson"):
            # check if there is another edge of a not wanted type (everythin except KP and IB)
            for e in graph[edge[0]][edge[1]].values():
                if e["type"] in not_wanted_edgetypes:
                    break
            else: # gets called if it did not break
                edgetype_to_distances_mel["SurvnetKontaktperson_strict"].append(diff_mel)
                edgetype_to_distances_sym["SurvnetKontaktperson_strict"].append(diff_sym)
                
                
        if edge[2]["type"] == "SurvnetAngestecktBei":
            # check if there is another edge of a not wanted type (everythin except KP and IB)
            for e in graph[edge[0]][edge[1]].values():
                if e["type"] in not_wanted_edgetypes:
                    break
            else: # gets called if it did not break
                edgetype_to_distances_mel["SurvnetAngestecktBei_strict"].append(diff_mel)
                edgetype_to_distances_sym["SurvnetAngestecktBei_strict"].append(diff_sym)
            
            
        # SelbeAdresseNachname edges that stand alone (have no neighboring AN or A edges)
        if edge[2]["type"] == "SelbeAdresseNachname":
            # for all neighboring edges
            for e in graph.edges(edge[0:2], data=True):
                # ignore the edges between the query nodes
                if (e[:2] == edge[:2]) or (e[:2] == edge[:2][::-1]):
                    continue
                    
                # if there is a nother same aress edge connecting from the nodes break and dont use edge
                if e[2]["type"] in ["SelbeAdresse", "SelbeAdresseNachname"]:
                    break
            else: # gets called if it did not break
                edgetype_to_distances_mel["SelbeAdresseNachname_strict"].append(diff_mel)
                edgetype_to_distances_sym["SelbeAdresseNachname_strict"].append(diff_sym)
            
    
    # filter out None vals from symptomstart
    edgetype_to_distances_sym = {
        key: [
            val for val in vals
                    if val != None
        ] for key, vals in edgetype_to_distances_sym.items()
    }
    
    
    
    

    
    
    
    
    
    ################################
    # plot differences symptomonset kde plot
    
    
    ################################################################### TODO paper
    plt.clf()
    sns.set_theme(font_scale=2) 
    fig, ax = plt.subplots(figsize=(9, 6))
    
    diffs_ib = edgetype_to_distances_sym["SurvnetAngestecktBei"]
    occurrances = [
        diffs_ib.count(i)
            for i in range(min(diffs_ib), max(diffs_ib)+1 )
    ]

    x_diffs = list(range(min(diffs_ib), max(diffs_ib)+1 ))

    ax = sns.barplot(x=x_diffs, y=occurrances, color="#595959")
    
    
    # labels
    plt.xlabel("Symptom onset date differences (days)")
    plt.ylabel("Count")
    

    mean_diff = np.mean(diffs_ib)
    std_diff = np.std(diffs_ib)

    ax.set_facecolor('#EAEAEA')
    
    plt.tight_layout()

    plt.title(f"mean: {round(mean_diff, 2)}, std: {round(std_diff, 2)}, total edges: {len(diffs_ib)} ")

    plt.savefig(f"plots/symptom_start/symptomonsets_infectedby.svg")
    
    
    
    
    
        
    ################################
    # plot symptomonset probailities
    
    
    ################################################################### TODO paper
    
    ### Calculate probabilities
    

    
    occ_dict = defaultdict(int)
    for x, occ in zip(x_diffs, occurrances):
        occ_dict[x] = occ
    
    total_occs = len(diffs_ib)
    print(occ_dict)
    print(x_diffs)
    print(occurrances)
    
    probs = []
    for x in x_diffs:
        
        # P(a → b│diff_symptom_onset(b,a))
        # =
        # P(diff_symptom_onset(b,a)│a → b)×Pr⁡(a→b)
        # / P(diff_symptom_onset(b,a)│a→b)×Pr(a→b) + P(-1 × diff_symptom_onset(a,b)│b→a)×Pr⁡(b→a)
        
        # =>
        
        #   P(diff_symptom_onset(b,a)│a → b) × 0.5
        # / P(diff_symptom_onset(b,a)│a → b) × 0.5 + P(-1 × diff_symptom_onset(a,b)│b → a) x 0.5
        
        P_ba_a_to_b = occ_dict[x] / total_occs
        
        P_ba_b_to_a = occ_dict[-x] / total_occs
        
        print(P_ba_a_to_b, P_ba_b_to_a, occ_dict[x], x)
        
        if P_ba_a_to_b == 0 and P_ba_b_to_a == 0:
            probs.append(0)
            continue
        
        probs.append( (0.5 * P_ba_a_to_b) / (0.5 * P_ba_a_to_b + 0.5 * P_ba_b_to_a ))
        
    
    
    plt.clf()
    sns.set_theme(font_scale=2.0) 
    fig, ax = plt.subplots(figsize=(9, 6))
    

    ax.plot(range(-4, 5), probs[:9],  color="#595959") # TODO this is set on our data. needs generalisaion
    
    
    plt.ylabel('P(b infected a)')  # we already handled the x-label with ax1
    plt.xlabel("Syptom onset case a - Symptom onset case b (days)")
    
    plt.xlim((-4,4))
    plt.ylim((0,1))
    plt.tight_layout()
    ax.set_facecolor('#EAEAEA')
    
    plt.savefig(f"plots/symptom_start/symptomonsets_infectedby_probability.svg")
    
    




def save_resuls(path: str) -> None:
    """
    currently just makes an empty file to satisfy snakemake 

    Args:
        path (str): _description_
    """
    with open(path, "w") as out_file:
        out_file.write("")




if __name__ == "__main__":
    
    dm = load_dm(DM_PATH)
    graph = load_graph(GRAPH_PATH)
    
    Path(f"plots/symptom_start/").mkdir(parents=True, exist_ok=True)
    
    check_symptom_starts_simple(NODES_PATH, graph)
    
    check_pairwise_datediffs(graph, dm, EDGES_PATH)
    
    save_resuls(OUTPUT_PATH)
    
    