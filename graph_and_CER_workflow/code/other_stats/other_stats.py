"""
    
"""




import sys
from datetime import datetime
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *



DM_PATH = sys.argv[1]
GRAPH_PATH = sys.argv[2]
SAMPLES_PATH = sys.argv[3]
OUTPUT_PATH = sys.argv[4]

    
    
    
    
def check_different_dates(samples: dict, graph: object) -> list:
    """
    check date from healthdepartment (Meldedatum) vs sampling date from genetic data

    Args:
        samples (dict): key= sampleid, val= sample metadata
        graph (object): nx Multigraph object

    Returns:
        (list): ...

    """
    dates = []

    # for each case collect the dates
    for node in graph.nodes:
        # regard only cases with sequences
        sample_id =  graph.nodes[node]["sample_id"]
        if sample_id == "":
            continue
            
        # date from health department
        ga_date_str = graph.nodes[node]["date"]
        ga_date = datetime.strptime(ga_date_str, "%d.%m.%Y")
        
        # date from sequence
        sampling_date_str = samples[sample_id]["date"]
        sampling_date = datetime.strptime(sampling_date_str, "%d.%m.%Y")
        
        start_date = datetime.strptime("01.02.2021", "%d.%m.%Y")
        
        # collect information into dicts
        dates.append({
            "date_diff": (ga_date - sampling_date).days,
            "caseid":node,
            "sample_id":sample_id,
            "ga_date":ga_date_str,
            "sampling_date":sampling_date_str,
            "ga_date_int": (ga_date - start_date).days,
            "sampling_date_int": (sampling_date - start_date).days,
        })
        
    # recollect values for plots
    date_diffs = [d["date_diff"] for d in dates]
    date_diffs_cutoff = [d for d in date_diffs if abs(d) <40]
    
    ga_dates = [d["ga_date_int"] for d in dates]
    sampling_dates = [d["sampling_date_int"] for d in dates]
    max_date = max( max(ga_dates), max(sampling_dates))
    var = np.var(date_diffs)
    
    ################################
    # plot dates as histogram (with cutoff)
    
    plt.clf()
    
    plt.hist(date_diffs_cutoff, bins = 40)
    
    # labeling
    plt.xlabel("Date difference in days (health department date - sampling date)")
    plt.ylabel("Count")
    plt.title(f"Extreme differences: {max(date_diffs)}, {min(date_diffs)}, variance: {var}\n")
          
    #plt.show()
    plt.savefig("plots/other_stats/ga_dates_vs_sampling_dates.pdf")
    
    ################################
    # plot dates against each other as scatter with variance
    
    plt.clf()
    
    # this actually no longer works, as we already filter samples for too large of a date difference in find_case_to_sample_mapping.py
    # date points
    plt.scatter(ga_dates, sampling_dates)
    
    # middle black line between dates
    plt.plot(range(max_date+1), range(max_date+1), c="black")

    # variance lines (red)
    plt.plot([i+var for i in range(max_date+1-int(var))], range(max_date+1-int(var)), c="red")
    plt.plot(range(max_date+1-int(var)), [i+var for i in range(max_date+1-int(var))], c="red")

    # labeling
    plt.xlabel("Health department dates")
    plt.ylabel("Sampling dates")
    plt.title(f"Extreme differences: {max(date_diffs)}, {min(date_diffs)}, variance (red line): {var}")
              
    plt.savefig("plots/other_stats/ga_dates_vs_sampling_dates_scatter.pdf")
    
    
    return dates
    




def report_graph_data(graph: object) -> list:

    lines = []
    
    lines.append( f"Undirected graph data:" )
    lines.append( f"Graph nodes: {len(graph.nodes)}" )
    lines.append( f"Graph edges: {len(graph.edges)}\n" )
    
    
    # check edge type counts
    edge_types = set([data["type"] for _,_,data in list(graph.edges(data=True))])
    edge_types = sorted(list(edge_types))
    
    for edge_type in edge_types:
        tmp_graph = graph.copy()
        
        # collect all edges to be removed
        remove_edges = []
        
        for edge in tmp_graph.edges:
            
            if tmp_graph.edges[edge]["type"] != edge_type:
                remove_edges.append(edge)
                
        # and remove them
        tmp_graph.remove_edges_from(remove_edges)
        
        lines.append( f"{edge_type} edges: {len(tmp_graph.edges)}" )
    
    
    return lines



def save_resuls(path: str, lines: list) -> None:
    

    with open(path, "w") as out_file:
        
        out_file.write("")
        
        for line in lines:
            out_file.write(line+"\n")
            



def plot_graph_edge_distribution(graph: object) -> None:
    """

    Args:
        graph (object): _description_
    """
    
    
        
    edgetypes_to_count = defaultdict(int)
    
    for u,v,data in graph.edges(data=True):
        if data["type"] == "SelbeAdresse":
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
    
    # this should ultimately not be done
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
    
    
    ax = sns.barplot(x=keys, y=vals, color="#595959")
    
    sum_edges = sum(list( edgetypes_to_count.values() ))
    percentage_strings = [ f"{ round(100 * v / sum_edges, 2) }%"  for v in vals ]
    
    ax.bar_label(ax.containers[0], labels=percentage_strings, fontsize=23, label_type='edge', padding=1)
    
    
    plt.tight_layout(pad=2)   
    
    plt.ylabel("Count")
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    
    plt.ylim(0, max( 15000, max(vals) ))
    
    plt.savefig("plots/other_stats/edge_distribution_undirected.svg")
    
    

    
    


if __name__ == "__main__":
    
    graph = load_graph(GRAPH_PATH)
    samples = load_samples(SAMPLES_PATH)
    
    Path(f"plots/other_stats/").mkdir(parents=True, exist_ok=True)
    
    _ = check_different_dates(samples, graph)
    
    lines = report_graph_data(graph)
    
    plot_graph_edge_distribution(graph)
        
    save_resuls(OUTPUT_PATH, lines)
    
    