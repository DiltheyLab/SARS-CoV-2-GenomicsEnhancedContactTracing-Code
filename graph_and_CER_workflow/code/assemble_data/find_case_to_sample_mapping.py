"""

1. Load all data

2. caseid to sampleid by ambig char count
    - take sample with smallest ambig count


3. caseid to sampleid by count of 0 1 edges
    

"""

import sys
import numpy as np
import matplotlib.pyplot as plt
import itertools

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *



GRAPH_PATH = sys.argv[1]
DM_PATH = sys.argv[2]
SAMPLES_PATH = sys.argv[3]
OUTPUT_CSV_PATH = sys.argv[4]
OUTPUT_GRAPH_PATH = sys.argv[5]



def find_mapping(samples: dict, graph: object, dm: dict) -> tuple[dict, dict]:
    """
    Find mapping from cases (caseids) to samples (sample ids)
    For cases with multiple samples:
    
    - check if node has a pairwise distance >= t
    - if false: choose sample by lowest amount of ambig characters
    - if true:  choose sample by highest 0 and 1 edges in the case graph that are supported by contact data
    
    Args:
        samples (dict): key= sampleid, val= sample metadata
        graph (object): nx Multigraph object
        dm (dict): "header" = list of sampleids, "table"= list of lists of distances

    Returns:
        tuple[dict, dict]: 
            caseid_to_sid: key=caseid, val=sampleid
            plot_data: save pairwise distances for some plots
    """
    
    caseid_to_sid = {}
    plot_data = {  }
    
    print("number of sequences before:", len(dm["header"]))
    # plot_data = { node:{} for node in graph.nodes }
    
    cases_more_sequences_after_filter = 0
    sids_filtered_by_datediff = 0
    
    # For every node: find the one fitting sample
    for node in graph.nodes:
        sids = graph.nodes[node]["sample_id"].split(",")
        
        # 1. handle cases that only have one sample (in the parameters of the analysis)
        
        # only keep the samples that are in the analysis parameters
        sids = [sid for sid in sids if sid in samples]
        
        
        # if the sampling date and the date from the health departemt differ to much
        amount = len(sids)
        case_date = datetime.strptime(graph.nodes[node]["date"], "%d.%m.%Y")
        sid_count_before = len(sids)
        
        # these are values from the other_stats.py ga_dates_vs_sampling_dates_scatter.pdf file (old version)
        sids = [sid for sid in sids
                        if -21 <= (case_date - datetime.strptime(samples[sid]["date"], "%d.%m.%Y") ).days <= 7   ]
        sid_count_after = len(sids)
        
        sids_filtered_by_datediff += sid_count_before - sid_count_after
        
        
        # if the case only has one or no samples add that to the dict and continue
        if sids == [] :
            caseid_to_sid[node] = ""
            continue
        if len(sids) == 1 :
            caseid_to_sid[node] = sids[0]
            continue 
                
        
        cases_more_sequences_after_filter += 1    
        
        # 2. check the paiswise distances
        
        all_pairs = list(itertools.combinations(sids, 2))
        
        plot_data[node] = {}
        distances = []
        # for every pair of sampleids of a node
        for sid_1, sid_2 in all_pairs:
            index_1 = dm["header"].index(sid_1)
            index_2 = dm["header"].index(sid_2)
            
            distance = dm["table"][index_1][index_2]
            
            distances.append(distance)
            
            plot_data[node][sid_1+"-"+sid_2] = distance
            
        # 3. proceed by either ambig count or neighbor count

        # 3.1  if the distances seem reasonable choose by lowest ambic char count
        if max(distances) < 2: 
            
            # sid to ambig count mapping
            sids_ambigs = [
                (sid, int(samples[sid]["ambig"])) 
                    for sid in sids
                        if sid in samples
            ]
            # smallest ambig count first
            sids_ambigs = sorted(sids_ambigs, key=lambda tup: tup[1])
            
            caseid_to_sid[node] = sids_ambigs[0][0]
            
            
        # 3.2  if at least one distance is to high we might expect that a sample was switched by accident:
        # -> choose the sample that is the most supported by the contact data
        else:
            
            # all other cases (neighbors) connected by one contact edge (from survnet)
            neighbor_ids = nx.all_neighbors(graph, node)
            neighbor_sids = {
                cvdid : graph.nodes[cvdid]["sample_id"].split(",") 
                    for cvdid in neighbor_ids
                        if graph.nodes[cvdid]["sample_id"] != ""
            }
            
               
            # counts = {}
            max_count = -1
            max_sid = ""
            #for each sample of the current case
            for sid in sids:
                count = 0
                # for every neighboring case
                for cvdid, samples_2 in neighbor_sids.items():
                    # for every sample of that case
                    for sid_2 in samples_2:
                        
                        if sid_2 not in samples:
                            continue
                        
                        index_1 = dm["header"].index(sid)
                        index_2 = dm["header"].index(sid_2)
                        
                        if dm["table"][index_1][index_2] < 2:
                            count += 1
                        
                # keep the highest count
                # counts[sid] = count
                if count > max_count:
                    max_count = count
                    max_sid = sid
            
            # finally keep highest sample
            caseid_to_sid[node] = max_sid
               
    print("Sequences removed by date filtering:", sids_filtered_by_datediff)
        
    print("Cases with more than one sequence after date filter:",cases_more_sequences_after_filter)
    
    return caseid_to_sid, plot_data


def plot_pairwise_distances(plot_data: dict ):
    """
    plot some data

    Args:
        plot_data (dict): save pairwise distances for some plots

    """
    
    # collect all distances
    distances = [plot_data[node][sid_pair]
                    for node in plot_data
                        for sid_pair in plot_data[node]]
    
    mean_distances = np.mean(distances)
    
    
    plt.clf()

    plt.hist(distances, bins = max(distances))
    plt.xlabel("Pairwise distances of single nodes count")
    plt.ylabel("Count")
    plt.title("Mean:"+str(mean_distances))
    plt.axvline(x=mean_distances, color="r")

    plt.savefig("plots/case_matching_pairwise_distances.pdf")
    
    
    # without big numbers
    
    # collect all distances
    distances = [plot_data[node][sid_pair]
                    for node in plot_data
                        for sid_pair in plot_data[node]
                            if plot_data[node][sid_pair] < 50]
    
    mean_distances = np.mean(distances)
    
    
    plt.clf()

    plt.hist(distances, bins = max(distances))#max(distances_list))
    plt.xlabel("Pairwise distances of single nodes count")
    plt.ylabel("Count")
    plt.title("Mean:"+str(mean_distances))
    plt.axvline(x=mean_distances, color="r")

    #plt.show()
    plt.savefig("plots/case_matching_pairwise_distances_cutoutliers.pdf")
    
        
    # without big numbers
    
    # collect all distances
    distances = [plot_data[node][sid_pair]
                    for node in plot_data
                        for sid_pair in plot_data[node]
                            if plot_data[node][sid_pair] < 30]
    
    mean_distances = np.mean(distances)
    
    
    plt.clf()

    plt.hist(distances, bins = max(distances))#max(distances_list))
    plt.xlabel("Pairwise distances of single nodes count")
    plt.ylabel("Count")
    plt.title("Mean:"+str(mean_distances))
    plt.axvline(x=mean_distances, color="r")

    #plt.show()
    plt.savefig("plots/case_matching_pairwise_distances_cutoutliersmore.pdf")
    
    
    
    
def count_sample_to_caseid(samples, graph):
    
    samples_to_caseid = 0
    
    
    # For every node: find the one fitting sample
    for node in graph.nodes:
        sids = graph.nodes[node]["sample_id"].split(",")
        
        # only keep the samples that are in the analysis parameters
        sids = [sid for sid in sids if sid in samples]
        
        samples_to_caseid += len(sids)
        
    print("all samples:", len(samples))
    print("all samples linking to caseid:", samples_to_caseid)



def add_samples_to_graph(graph : object, caseid_sid: dict, dm: dict) -> None:
    """
    Change sampleids to one real sampleid

    Args:
        graph (object): nx Multigraph object
        caseid_sid (dict): key=caseid, val=sampleid
        dm (dict): "header" = list of sampleids, "table"= list of lists of distances
    """
    
    print("Add to graph...")
    
    
    for node in graph.nodes:
        graph.nodes[node]["sample_id"] = caseid_sid[node]
        if caseid_sid[node] != "":
            graph.nodes[node]["dm_index"] = dm["header"].index( caseid_sid[node] )
            
    
    


if __name__ == "__main__":
    
    samples = load_samples(SAMPLES_PATH)
    dm = load_dm(DM_PATH)
    graph = load_graph(GRAPH_PATH)
    
    caseid_to_sid, plotdata = find_mapping(samples, graph, dm)
        
    save_caseid_mapping(OUTPUT_CSV_PATH, caseid_to_sid)
    
    plot_pairwise_distances(plotdata)
    
    add_samples_to_graph(graph, caseid_to_sid, dm)
    
    
    count_sample_to_caseid(samples, graph)
    save_graph(graph, OUTPUT_GRAPH_PATH)

    