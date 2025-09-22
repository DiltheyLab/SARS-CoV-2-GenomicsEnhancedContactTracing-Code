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
import pandas as pd


parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))
from util_functions import *


code_dir = Path(__file__).resolve().parent.parent
import_dir = code_dir / "assemble_data"
sys.path.insert(0, str(import_dir))
from add_outbreaks_to_graph import load_outbreaks


code_dir = Path(__file__).resolve().parent.parent
import_dir = code_dir / "infection_source_rates"
sys.path.insert(0, str(import_dir))
from infection_source_rate import filter_edgetypes, remove_edges_not_supported_by_genetic_data, remove_edges_for_infectionchains_greedy, add_gentic_edges



DM_PATH = sys.argv[1]
GRAPH_PATH = sys.argv[2]
CLUSTERS_PATH = sys.argv[3]
OUTBREAKS_PATH = sys.argv[4]
OUTBREAKS_CONTEXT_PATH = sys.argv[5]

CLUSTER_TYPE = sys.argv[6]

OUTPUT_PATH = sys.argv[7]






def look_at_genetically_not_supported_edges(dm: dict, graph: object) -> dict :
    """

    Args:
        dm (dict): _description_
        graph (object): _description_

    Returns:
        dict: _description_
    """
    
    german_to_english_name = {
        "SelbeAdresse" : "Same adress",
        "True_SelbeAdresse" : "SameAddress - differentName",
        "SelbeAdresseNachname" : "SameAddress - sameName",
        "SurvnetAngestecktBei" : "Backward contact tracing",
        "SurvnetKontaktperson" : "Forward contact tracing",
        "Outbreak" : "Outbreak",
    }
    
    graph = graph.copy()
    distance_threshold = int(CLUSTER_TYPE[-1])
    filter_edgetypes(graph, "TA_AN_IB_O_KP")
    
    # only look at sequenced cases
    to_remove = [n for n in graph.nodes() if graph.nodes[n]["sample_id"] == ""]
    graph.remove_nodes_from(to_remove)
    
    
    # collect all edges that are not supported by genetic information (distance to big)
    not_supp_edges_by_type = defaultdict(list)
    for u, v, key, data in graph.edges(data=True, keys=True):
        
        
        # if one of the nodes does not have a sequence
        if graph.nodes[u]["sample_id"] == "" or graph.nodes[v]["sample_id"] == "":
            # not_supp_edges_by_type.append( edge[0:3] ) # only edges with a distance
            continue
        
        # if the distance is larger then the clusterdistance
        index1 = graph.nodes[u]["dm_index"]
        index2 = graph.nodes[v]["dm_index"]
        if dm["table"][index1][index2] > distance_threshold:
            
            not_supp_edges_by_type[ german_to_english_name[data["type"]] ].append( dm["table"][index1][index2] )
            
    
    
    cutoff = 30    

    
    for key in sorted(not_supp_edges_by_type.keys()):
        values = not_supp_edges_by_type[key]


        # bucket values >= cutoff into a single category "cutoff+"
        grouped = [f"{cutoff}+" if v >= cutoff else int(v) for v in values]

        # count frequencies
        counts = Counter(grouped)

        # build full category list (all numbers up to cutoff-1 + cutoff+)
        categories = list(range(0, cutoff)) + [f"{cutoff}+"]
        percentages = [(counts.get(c, 0) / len(values)) * 100 for c in categories]


        # plot barplot
        # plt.clf()
        sns.set_theme(font_scale=1.8) 
        fig, ax = plt.subplots(figsize=(11.5, 7))
    
        # plot barplot
        sns.barplot(x=[str(c) for c in categories], y=percentages, color="#595959", ax=ax)
        plt.title(key)
        plt.xlabel("Genetic distance")
        plt.ylabel("Percentage (%)")
        # plt.tight_layout()

        plt.ylim((0,30))

        # show every 2nd x tick
        for i, label in enumerate(ax.get_xticklabels()):
            if i % 2 != 0:  # hide odd ticks
                label.set_visible(False)

        
        plt.savefig(f"plots/changes/{CLUSTER_TYPE}/{key}_distance_plot.svg")
    
    
    
    
        
        
        
    output_dict = {
        f"total not supported edges of type {typ}" : len(distances)
            for typ, distances in not_supp_edges_by_type.items()
    }
    print(output_dict)
    return output_dict




    

def compare_infectionsource_counts(dm: dict, graph: object) -> dict:
    """

    Args:
        dm (dict): _description_
        graph (object): _description_

    Returns:
        dict: _description_
    """
    
    graph = graph.copy()
    
    distance_threshold = int(CLUSTER_TYPE[-1])
    
    #filter graph by edgetypes     
    filter_edgetypes(graph, "TA_AN_IB_O_KP")
    
    # only look at sequenced cases
    to_remove = [n for n in graph.nodes() if graph.nodes[n]["sample_id"] == ""]
    graph.remove_nodes_from(to_remove)
        
        
    # prepare variables
    graph_wo_genetic = graph.copy()
    graph_with_genetic_support = graph.copy()
    graph_with_genetic_edges = graph.copy()
    
    # only genetically supported contact tracing edges are kept
    remove_edges_not_supported_by_genetic_data(graph_with_genetic_support, dm, distance_threshold)
    remove_edges_not_supported_by_genetic_data(graph_with_genetic_edges, dm, distance_threshold)
    
    # keep for later for the plots
    graph_wo_genetic_copy = graph_wo_genetic.copy()
    graph_with_genetic_support_copy = graph_with_genetic_support.copy()
    
    # calculate graph to find additional infection sources with genetic data
    direction_cutoff = 0 #int(GRAPH_PATH.split(".")[0].split("_")[-1])
    add_gentic_edges(graph_with_genetic_edges, dm, distance_threshold, direction_cutoff)
    
    
    # calculate infection source graph
    remove_edges_for_infectionchains_greedy(graph_wo_genetic)
    remove_edges_for_infectionchains_greedy(graph_with_genetic_support)
    remove_edges_for_infectionchains_greedy(graph_with_genetic_edges)
    
        
      
    # calculate final stats
    case_count = len(graph_wo_genetic.nodes)
    
    # sanity check
    if case_count != len(graph_with_genetic_support.nodes) or case_count != len(graph_with_genetic_edges.nodes):
        print("CASES WERE TAMPERED WITH")
        exit(1)
    
    
    # save prelim results
    output_dict = {
        "Sequenced cases count": case_count,
        "Infectionsources, disregarding genetic distances": sum( [graph_wo_genetic.in_degree(n) for n in graph_wo_genetic.nodes]  ),
        "Infectionsources, with genetically supported edges": sum( [graph_with_genetic_support.in_degree(n) for n in graph_with_genetic_support.nodes]  ),
        "Infectionsources, with genetically clonal edges": sum( [graph_with_genetic_edges.in_degree(n) for n in graph_with_genetic_edges.nodes]  ),
        
    }
    
    
    
    # sanity check
    for node in graph_wo_genetic:
        if graph_wo_genetic_copy.in_degree(node) > 0:
            if graph_wo_genetic.in_degree(node) == 0:
                print("################################")
                print(node)
                exit(1)
    
    
    
    #######################
    # now count normalised per edgetype and plot it
    
    incoming_edges_wo_genetic = {node: list(graph_wo_genetic_copy.in_edges(node, keys=True, data=True)) 
                      for node in graph_wo_genetic_copy.nodes()}
    
    incoming_edges_with_gen_sup = {node: list(graph_with_genetic_support_copy.in_edges(node, keys=True, data=True)) 
                      for node in graph_with_genetic_support_copy.nodes()}
    
    plot_parameters = [
        (incoming_edges_wo_genetic, graph_wo_genetic_copy, "all_contact_edges"),
        (incoming_edges_with_gen_sup, graph_with_genetic_support_copy, "genetically_supported_edges"),
    ]
    german_to_english_name = {
        "SelbeAdresse" : "Same adress",
        "True_SelbeAdresse" : "SameAddress\n-differentName",
        "SelbeAdresseNachname" : "SameAddress\n-sameName",
        "SurvnetAngestecktBei" : "Backward\ncontact tracing",
        "SurvnetKontaktperson" : "Forward\ncontact tracing",
        "Outbreak" : "Outbreak",
    }
    
    for incoming_edges, graph_loop, filename_part in plot_parameters:
        
        # collect data into the right format
        type_sums = defaultdict(float)  
        for node, edges in incoming_edges.items():
            total = len(edges)
            
            for u, v, k, data in edges:
                edge_type = data["type"]
                # edge_type = graph_loop[u][v][0]["type"]
                
                type_sums[edge_type] += 1 / total
        
        type_sums_sorted = sorted(list(type_sums.items()) , key = lambda x: x[1], reverse=True)
        keys = [german_to_english_name[x] for x,_ in type_sums_sorted]
        vals = [x for _,x in type_sums_sorted]
        
        # start plotting it
        # plt.clf()
        sns.set_theme(font_scale=1.8) 
        fig, ax = plt.subplots(figsize=(11.5, 7))
        plt.tight_layout()
        


        
        
        ax = sns.barplot(x=keys, y=vals, color="#595959")
        
        sum_edges = sum( vals )
        percentage_strings = [ f"{ round(100 * v / sum_edges, 2) }%"  for v in vals ]
        
        ax.bar_label(ax.containers[0], labels=percentage_strings, fontsize=23, label_type='edge', padding=1)
        
        
        # ax.figure.autofmt_xdate(rotation=45)
        plt.tight_layout(pad=2)   
        
        plt.ylabel("Count")
        ax.set_facecolor('#EAEAEA')
        
        # plt.ylim(0,1000 if max(vals) < 1000 else 1200)
        
        title_symbol = "=" if CLUSTER_TYPE[-1] == "0" else "≤"
        plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_TYPE[-1]}\n{filename_part}") 


        plt.savefig(f"plots/changes/{CLUSTER_TYPE}/edge_type_usage_{filename_part}.svg")
        
        output_dict[f"Used edgetypes for {filename_part}"] = str(type_sums_sorted)

        


    print(output_dict)
    
    return output_dict
    
    
    
    
def check_edge_certanty(dm: dict, graph: object) -> dict:
    """

    Args:
        dm (dict): _description_
        graph (object): _description_

    Returns:
        dict: _description_
    """
    
    
    graph = graph.copy()
    
    distance_threshold = int(CLUSTER_TYPE[-1])
    
    #filter graph by edgetypes     
    filter_edgetypes(graph, "TA_AN_IB_O_KP")
    
    # only look at sequenced cases
    to_remove = [n for n in graph.nodes() if graph.nodes[n]["sample_id"] == ""]
    graph.remove_nodes_from(to_remove)
    
    
    
    # only genetically supported contact tracing edges are kept
    graph_only_genetic_supp = graph.copy()
    remove_edges_not_supported_by_genetic_data(graph_only_genetic_supp, dm, distance_threshold)

    # collect all degrees in dictionaries
    in_degrees = dict(graph.in_degree())
    out_degrees = dict(graph.out_degree())
    
    in_degrees_gen = dict(graph_only_genetic_supp.in_degree())
    out_degrees_gen = dict(graph_only_genetic_supp.out_degree())
    
    
    # calculate the difference in degrees by subtracting the degrees of only supported edges
    indeg_diff = [in_degrees[n] - in_degrees_gen[n]  for n in graph.nodes() if in_degrees[n] > 0]
    outdeg_diff = [out_degrees[n] - out_degrees_gen[n]  for n in graph.nodes() if out_degrees[n] > 0] 
    
    # count the occurences of differences into a dict (sorte for readybility)
    indeg_diff_counts = dict(sorted(dict(Counter(indeg_diff)).items()))
    outdeg_diff_counts = dict(sorted(dict(Counter(outdeg_diff)).items()))
    
    # sum up the total of cases where at least one edge could be disregarded
    indeg_diff_total = sum([ indeg_diff_counts[i] for i in indeg_diff_counts if i > 0 ])
    outdeg_diff_total = sum([ outdeg_diff_counts[i] for i in outdeg_diff_counts if i > 0 ])
    

    output_dict = {
        "in degree differences counts" : indeg_diff_counts,
        "in degree >1 could be disregarded count" : indeg_diff_total,
        "out degree differences counts" : outdeg_diff_counts,
        "out degree >1 could be disregarded count" : outdeg_diff_total,
    }
    
    print(output_dict)
    
    return output_dict
    


def check_timemapping_outbreaks(graph: object, clusters: list, outbreaks: dict, outbreak_to_context: dict) -> dict:
    """

    Args:
        graph (object): _description_
        clusters (list): _description_
        outbreaks (dict): _description_
        outbreak_to_context (dict): _description_

    Returns:
        dict: _description_
    """
    
    # graph = graph.copy()
    # #filter graph by edgetypes     
    # filter_edgetypes(graph, "TA_AN_IB_O_KP")
    # # only look at sequenced cases
    # to_remove = [n for n in graph.nodes() if graph.nodes[n]["sample_id"] == ""]
    # graph.remove_nodes_from(to_remove)
    
    distance_threshold = int(CLUSTER_TYPE[-1])
    
    # Filter the outbreaks to only contain cases that are kept in the graph
    outbreaks = {
        outbreak_name: [case for case in outbreak_cases
                            if case in graph.nodes()] 
            for outbreak_name, outbreak_cases in outbreaks.items()
                if any(case in graph.nodes() for case in outbreak_cases) # do not keep empty outbreaks (somewhat inefficient)
    }
    
    #############################################################
    #### actually not doing cluster to outbreak but rather only genetically connected to cluster
    # outbreak_to_cluster = defaultdict(list)
    # for outbreak_name in outbreaks:        
    #     for case_list in clusters:
    
    #         for outbreak_case in outbreaks[outbreak_name]:
    #             if outbreak_case in case_list:
    #                 outbreak_to_cluster[outbreak_name].append(case_list)
    #                 break    
    # # remove all cases that belong to the outbreak itself, and flatten all cluster lists for each outbreak
    # outbreak_to_cluster = {
    #     outbreak_name: [case for cluster_list in val 
    #                             for case in cluster_list 
    #                                 if case not in outbreaks[outbreak_name]]
    #         for outbreak_name, val in outbreak_to_cluster.items()
    # }

    # prepare them before the loop
    sequenced_cases = [
        n for n, data in graph.nodes(data=True)
            if data["sample_id"] != ""
    ]
    
    # find all genetically connected cases for each outbreak
    outbreak_to_cases_gen = defaultdict(list)
    for outbreak_name in outbreaks:        
        for outbreak_case in outbreaks[outbreak_name]:
            
            if graph.nodes[outbreak_case]["sample_id"] == "":
                continue
            
            outbreak_index = graph.nodes[outbreak_case]["dm_index"]
            
            for seq_case in sequenced_cases:
                if seq_case in outbreaks[outbreak_name]:
                    continue
                
                index2 = graph.nodes[seq_case]["dm_index"]
                if dm["table"][outbreak_index][index2] <= distance_threshold:
                    outbreak_to_cases_gen[outbreak_name].append(seq_case)
        
                
                


    # collect all dates of outbreak cases
    outbreak_dates = {
        outbreak_name: [graph.nodes[case]["date"] for case in outbreaks[outbreak_name]]
            for outbreak_name in outbreaks
    }
    
    # filter the dates into start and end date
    outbreak_start_end_dates = {
        outbreak_name: {
            "start": min(dates, key=lambda d: datetime.strptime(d, "%d.%m.%Y")),
            "end": max(dates, key=lambda d: datetime.strptime(d, "%d.%m.%Y"))
        }
        for outbreak_name, dates in outbreak_dates.items()
    }
    

    # calculate the time diffs to get a feeling    
    # outbreak_timespans = Counter([(datetime.strptime(val["end"], "%d.%m.%Y") - datetime.strptime(val["start"], "%d.%m.%Y")).days
    #         for outbreak_name, val in outbreak_start_end_dates.items()])
    # print( sorted(list(dict(outbreak_timespans).items()), key=lambda x:x[0]))


    outbreaks_starttime_diffs = {
        outbreak_name: [
            
            # case date - start outbreak date  -> days after the start date
            (datetime.strptime(graph.nodes[conn_case]['date'], "%d.%m.%Y") - datetime.strptime(outbreak_start_end_dates[outbreak_name]["start"], "%d.%m.%Y")).days
            
                for conn_case in outbreak_to_cases_gen[outbreak_name]
            
            
        ] for outbreak_name in outbreak_dates
    }

    # print(outbreaks_starttime_diffs)
    
    for outbreak_name, outbreak_timediffs in outbreaks_starttime_diffs.items():
        
        if len(outbreak_timediffs) == 0:
            continue
        
        outbreak_context = ""
        if outbreak_name in outbreak_to_context:
            outbreak_context = outbreak_to_context[outbreak_name]
        
        # plt.clf()
        sns.set_theme(font_scale=2) 
        # sns.set_theme(font_scale=2.2) 
        fig, ax = plt.subplots(figsize=(19, 10))
        ax.set_facecolor('#EAEAEA')
        
        
        ax = sns.violinplot(x=outbreak_timediffs, inner="point", split=True,  color="#595959")
        #density_norm="count",
        plt.axvline(x=0, color="r")
        end_timediff = (datetime.strptime(outbreak_start_end_dates[outbreak_name]["end"], "%d.%m.%Y") - datetime.strptime(outbreak_start_end_dates[outbreak_name]["start"], "%d.%m.%Y")).days
        plt.axvline(x=end_timediff, color="r")
        
            
        plt.ylabel("Count/Density")
        plt.xlabel("Time difference to outbreak start date")
        
        outbreak_name = outbreak_name.replace('/', '-')
        title_symbol = "=" if CLUSTER_TYPE[-1] == "0" else "≤"
        plt.title(f"Genetic threshold for clustering {title_symbol} {CLUSTER_TYPE[-1]}\n{outbreak_name} - context:{outbreak_context}") 
        
        plt.savefig(f"plots/changes/{CLUSTER_TYPE}/outbreak_timediffs/Timediffs_for_{outbreak_name}.svg")
        
            
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    output_dict = {
        
    }
    
    return output_dict
    
    
    
    
    
    
    
    
    
    
    



if __name__ == "__main__":
    
    # load all input data
    dm = load_dm(DM_PATH)
    graph = load_graph(GRAPH_PATH)
    clusters = load_clusters(CLUSTERS_PATH)
    outbreaks = load_outbreaks(OUTBREAKS_PATH)
    outbreak_to_context = {}
    with open(OUTBREAKS_CONTEXT_PATH, "r") as context_infile:
        
        lines = context_infile.readlines()[1:]

        for line in lines:
            parts = line.strip().split("\t")
            outbreak_to_context[parts[0]] = parts[1]
    
    
    Path(f"plots/changes/{CLUSTER_TYPE}/outbreak_timediffs").mkdir(parents=True, exist_ok=True)
    
    # prepare output dict in case of commenting out functions
    output_dict = {}
    
    #1
    # output_dict = compare_infectionsource_counts(dm, graph)
    
    #2
    # look at genetically not supported edges
    # output_dict_tmp = look_at_genetically_not_supported_edges(dm, graph)
    # output_dict.update(output_dict_tmp)
    
    #3
    # # cases with more than two connections, for how many can some be decided against
    # # TODO do this for directed and undirected
    # output_dict_tmp = check_edge_certanty(dm, graph)
    # output_dict.update(output_dict_tmp)
    
    
    #4
    # how do they fit on the timescale    
    output_dict_tmp = check_timemapping_outbreaks(graph, clusters, outbreaks, outbreak_to_context)
    output_dict.update(output_dict_tmp)
    
    
    
    print("Save results")
    save_clusters(OUTPUT_PATH, output_dict)