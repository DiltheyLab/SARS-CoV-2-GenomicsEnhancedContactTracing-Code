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
import os
import csv


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
SUBSAMPLING_PATH = sys.argv[6]

CLUSTER_TYPE = sys.argv[7]

OUTPUT_PATH = sys.argv[8]






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
        sns.set_theme(style="whitegrid",font_scale=1.8) 
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
        sns.set_theme(style="whitegrid",font_scale=1.8) 
        fig, ax = plt.subplots(figsize=(11.5, 7))
        plt.tight_layout()
        


        
        
        ax = sns.barplot(x=keys, y=vals, color="#595959", saturation=1)
        
        sum_edges = sum( vals )
        percentage_strings = [ f"{ round(100 * v / sum_edges, 2) }%"  for v in vals ]
        
        ax.bar_label(ax.containers[0], labels=percentage_strings, fontsize=23, label_type='edge', padding=1)
        
        
        # ax.figure.autofmt_xdate(rotation=45)
        plt.tight_layout(pad=2)   
        
        plt.ylabel("Count")
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)
        ax.spines['left'].set_visible(False)
        ax.spines['bottom'].set_visible(False)
        
        
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
    


def before_during_after_outbreaks(graph: object, outbreaks: dict, outbreak_to_context: dict, plot_name: str, case_to_lineage_date:dict, downsample_dir="", downsample_filename="") -> dict:
    """

    Args:
        graph (object): _description_
        outbreaks (dict): _description_
        outbreak_to_context (dict): _description_
        plot_name (str): _description_
        case_to_lineage_date (dict): _description_

    Returns:
        dict: _description_
    """
    

    graph = graph.copy()
    #filter graph by edgetypes     
    filter_edgetypes(graph, "TA_AN_IB_O_KP")
    
    
    distance_threshold = int(CLUSTER_TYPE[-1])
    unfiltered_outbreaks = outbreaks
    
    
    
    
    ################################
    # prepare data (outbreaks, outbreak dates, sequenced cases)
    
    # Filter the outbreaks to only contain cases that are kept in the graph
    outbreaks = {
        outbreak_name: [case for case in outbreak_cases
                            if case in graph.nodes()] 
            for outbreak_name, outbreak_cases in outbreaks.items()
                if any(case in graph.nodes() for case in outbreak_cases) # do not keep empty outbreaks (somewhat inefficient)
                    and outbreak_name != "CVD-DÜS-COCTAILBAR/ALTSTADT-2021-0086" # skip unusable outbreak
                    and outbreak_name in outbreak_to_context
    }
    
    
    #######
    # filter the outbreaks so that none remain that contain cases of rising liniages
    # i.e. remove all outbreaks with omicron cases, and all outbreaks with Delta cases before the 29th week (19.07.2021)
    print("before outbreak variant filter",len(outbreaks))
    
    if case_to_lineage_date != {}:
        filtered_outbreaks = {}
        delta_cutoffdate = datetime.strptime("19.07.2021", "%d.%m.%Y")
        for outbreak_name, outbreak_cases in outbreaks.items():
            
            keep_outbeak = True
            
            for case in outbreak_cases:
                if case not in case_to_lineage_date:
                    continue
                
                lineage, case_date = case_to_lineage_date[case]
                # print(lineage, panolinglineage_to_variant(lineage))
                # Omicron
                if panolinglineage_to_variant(lineage) == "Omicron":
                    # print("skip outbreak omicron", outbreak_name)
                    keep_outbeak = False
                
                if panolinglineage_to_variant(lineage) == "Delta":
                    if delta_cutoffdate > case_date: # if the cutoff date is after the date of the current case
                        # print("skip outbreak delta", outbreak_name)
                        keep_outbeak = False
                        
                    
            if keep_outbeak:
                filtered_outbreaks[outbreak_name] = outbreak_cases
                
        # skip outbreak delta CVD-DÜS-PRIV-2021-0085
        # skip outbreak delta CVD-DÜS-FREIZEIT-2021-0087
        # skip outbreak delta CVD-DÜS-BAR/ALTSTADT-2021-0090
        # skip outbreak delta CVD-DÜS-CAFFE/ALTSTADT2021-0089
        # skip outbreak omicron CVD-DÜS-ARBEIT-2021-35950
        # skip outbreak omicron CVD-DÜS-SCHULE-2021-0312
        # skip outbreak omicron CVD-DÜS-SCHULE-2021-0237
        # skip outbreak omicron CVD-DÜS-HEIM-2021-0223

        outbreaks = filtered_outbreaks

    print("after outbreak variant filter",len(outbreaks))
    
    
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

    
    # prepare the sequenced cases before the loop
    sequenced_cases = [
        n for n, data in graph.nodes(data=True)
            if data["sample_id"] != ""
    ]
    
    
    
    ################################
    # For each outbreak find the genetically connected cases
    # for now only with date cutoff
    
    # find all genetically connected cases for each outbreak
    outbreak_to_cases_gen_cutoff = defaultdict(list)
    
    # outbreak_to_cases_gen = defaultdict(list)
    
    # key: outbreak_context, val: cases already seen
    duplicate_protection_contextwide = defaultdict(list)
    
    for outbreak_name in outbreaks:        
        for outbreak_case in outbreaks[outbreak_name]:
            
            # ignore not sequenced outbreak cases
            if graph.nodes[outbreak_case]["sample_id"] == "":
                continue
            
            
            outbreak_index = graph.nodes[outbreak_case]["dm_index"]
            outbreak_date = datetime.strptime(graph.nodes[outbreak_case]["date"], "%d.%m.%Y")
            
            
            # check every sequenced case to see the distance
            for seq_case in sequenced_cases:
                
                # skip cases that belong to this outbreak
                if seq_case in outbreaks[outbreak_name]:
                    continue
                
                # duplicate protection
                if seq_case in duplicate_protection_contextwide[outbreak_to_context[outbreak_name]]:
                    continue
                
                # check if the case is in the date cutoff
                
                case_date = datetime.strptime(graph.nodes[seq_case]["date"], "%d.%m.%Y")
                diff_start_date = (case_date - datetime.strptime( outbreak_start_end_dates[outbreak_name]["start"], "%d.%m.%Y")).days
                diff_end_date = (case_date - datetime.strptime( outbreak_start_end_dates[outbreak_name]["end"], "%d.%m.%Y")).days
                if (-14 <= diff_start_date) and (diff_end_date <= 14):
                # if abs((outbreak_date - case_date).days) <= 14: # do it with distance to the current case, not the outbreak start and end times
                
                    # if so then check if the case is in the distance cutoff
                    index2 = graph.nodes[seq_case]["dm_index"]
                    if dm["table"][outbreak_index][index2] <= distance_threshold:
                        
                        # outbreak_to_cases_gen[outbreak_name].append(seq_case)
                        
                        # Save connected case and add to the duplicate protection
                        duplicate_protection_contextwide[outbreak_to_context[outbreak_name]].append(seq_case)
                        outbreak_to_cases_gen_cutoff[outbreak_name].append(seq_case)
                        
                      
                        
    


    # outbreaks_starttime_diffs = {
    #     outbreak_name: [
    #         # case date - start outbreak date  -> days after the start date
    #         #tupel of timediff and normal case name
    #         (datetime.strptime(graph.nodes[conn_case]['date'], "%d.%m.%Y") - datetime.strptime(outbreak_start_end_dates[outbreak_name]["start"], "%d.%m.%Y")).days
    #         # ((datetime.strptime(graph.nodes[conn_case]['date'], "%d.%m.%Y") - datetime.strptime(outbreak_start_end_dates[outbreak_name]["start"], "%d.%m.%Y")).days, conn_case)
    #             for conn_case in outbreak_to_cases_gen[outbreak_name]
    #     ] for outbreak_name in outbreak_dates
    # }
    
    
    ################################
    # save the difference of each date of the connected cases to the start date of the outbreak
    # this is later used to sort into before, during, after
    
    outbreaks_starttime_diffs_cutoff = {
        outbreak_name: [
            # case date - start outbreak date  -> days after the start date
            #tupel of timediff and normal case name
            (datetime.strptime(graph.nodes[conn_case]['date'], "%d.%m.%Y") - datetime.strptime(outbreak_start_end_dates[outbreak_name]["start"], "%d.%m.%Y")).days
        
                for conn_case in outbreak_to_cases_gen_cutoff[outbreak_name]
        ] for outbreak_name in outbreak_dates
    }
    
    




    
    
    ################################
    # Prepare date for before, during, after differentiation
    
    
    # count the total sizes per outbreak_context
    # used later to normalize #TODO might remove this after all
    context_to_outbreakcases_size = defaultdict(int)
    for outbreak_name, outbreak_cases in outbreaks.items():
        if outbreak_name in outbreak_to_context:
            context_to_outbreakcases_size[outbreak_to_context[outbreak_name]] += len(outbreak_cases)
            
    # context_to_outbreakcases_size_noseq_req = defaultdict(int)
    # for outbreak_name, outbreak_cases in unfiltered_outbreaks.items():
    #     if outbreak_name in outbreak_to_context:
    #         if outbreak_name != "CVD-DÜS-COCTAILBAR/ALTSTADT-2021-0086":
    #             context_to_outbreakcases_size_noseq_req[outbreak_to_context[outbreak_name]] += len(outbreak_cases)
        
    # print(context_to_outbreakcases_size)
    # print(context_to_outbreakcases_size_noseq_req)
    # defaultdict(<class 'int'>, {'Recreational context': 44, 'Work': 42, 'Hospital': 103, 'Other': 4, 'Nightlife': 45, 'Kindergarten/daycare': 121, 'School': 227, 'Residential Care Facility': 74, 'Refugee Accommodation': 60, 'Care home': 161})
    # defaultdict(<class 'int'>, {'Recreational context': 44, 'Work': 42, 'Hospital': 103, 'Other': 4, 'Nightlife': 45, 'Kindergarten/daycare': 121, 'School': 227, 'Residential Care Facility': 74, 'Refugee Accommodation': 60, 'Care home': 161})
    
    
    
    # before, during, after bins together in a dict
    context_to_bins_cutoff = {
        context: defaultdict(float)
            for context in set(outbreak_to_context.values())
    }
    # context_to_bins = {
    #     context: defaultdict(float)
    #         for context in set(outbreak_to_context.values())
    # }
    

    
    ################################
    # Iterate over the outbreaks and theirs connected cases and sort them into the bins
    
    outbreak_to_befduaf_cases = {}
    context_to_befduaf_cases = {}
    
    # do the single plots and also collect data for the combined plot
    for outbreak_name, conn_cases in outbreak_to_cases_gen_cutoff.items():
    # for outbreak_name, outbreak_timediffs in outbreaks_starttime_diffs_cutoff.items():
        
        # print( len(conn_cases), len(list(set(conn_cases))))
        outbreak_timediffs = outbreaks_starttime_diffs_cutoff[outbreak_name]
        
        if len(outbreak_timediffs) == 0:
            continue
        
        # ignore outbreaks without context
        if outbreak_name in outbreak_to_context:
            outbreak_context = outbreak_to_context[outbreak_name]
        else:
            continue
        
        outbreak_to_befduaf_cases[outbreak_name] = {
            "before" : 0,
            "during" : 0,
            "after" : 0,
        }
        if outbreak_context not in context_to_befduaf_cases:
            context_to_befduaf_cases[outbreak_context] = {
                "before" : 0,
                "during" : 0,
                "after" : 0,
            }
        
        # calculate the time diff to the outbreak end date
        # TODO mayb should move start time dif here too?
        end_timediff = (datetime.strptime(outbreak_start_end_dates[outbreak_name]["end"], "%d.%m.%Y") - datetime.strptime(outbreak_start_end_dates[outbreak_name]["start"], "%d.%m.%Y")).days
        

        # value to collect normalized by the outbreak context sizes 
        add_value = 1 / context_to_outbreakcases_size[outbreak_context]
        
        # if outbreak_name in outbreaks_starttime_diffs_cutoff: #TODO remove
        # actually sort into the bins
        for timediff in outbreaks_starttime_diffs_cutoff[outbreak_name]:
            
            if timediff < 0:
                context_to_bins_cutoff[outbreak_context]["before"] += add_value
                outbreak_to_befduaf_cases[outbreak_name]["before"] += 1
                context_to_befduaf_cases[outbreak_context]["before"] += 1
                
            elif timediff <= end_timediff:
                context_to_bins_cutoff[outbreak_context]["during"] += add_value
                outbreak_to_befduaf_cases[outbreak_name]["during"] += 1
                context_to_befduaf_cases[outbreak_context]["during"] += 1
                
            else:
                context_to_bins_cutoff[outbreak_context]["after"] += add_value
                outbreak_to_befduaf_cases[outbreak_name]["after"] += 1
                context_to_befduaf_cases[outbreak_context]["after"] += 1
                
            
        # for timediff in outbreak_timediffs:
        # # for timediff, case in outbreak_timediffs:
            
        #     if timediff < 0:
        #         context_to_bins[outbreak_context]["before"] += add_value
        #     elif timediff <= end_timediff:
        #         context_to_bins[outbreak_context]["during"] += add_value
        #     else:
        #         context_to_bins[outbreak_context]["after"] += add_value
                
    if downsample_dir == "":     
         
        out_lines = [["Outbreak", "before_count", "during_count", "after_count"]]
        for key, data in outbreak_to_befduaf_cases.items():
            out_lines.append([key, data["before"], data["during"], data["after"]])
        
        with open(f"data/before_during_after_cases_per_outbreak_{CLUSTER_TYPE}.csv", "w") as outfile:
            for line in out_lines:
                outfile.write("\t".join([str(l) for l in line]) + "\n")
        
         
        out_lines = [["Context", "before_count", "during_count", "after_count"]]
        for key, data in context_to_befduaf_cases.items():
            out_lines.append([key, data["before"], data["during"], data["after"]])
        
        with open(f"data/before_during_after_cases_per_context_{CLUSTER_TYPE}.csv", "w") as outfile:
            for line in out_lines:
                outfile.write("\t".join([str(l) for l in line]) + "\n")
        
        # save_clusters(f"data/before_during_after_cases_per_outbreak_{CLUSTER_TYPE}.json", outbreak_to_befduaf_cases)
        # save_clusters(f"data/before_during_after_cases_per_context_{CLUSTER_TYPE}.json", context_to_befduaf_cases)

    ################################
    # Plot the bin information
    
    
    # set the order and make pretty versions    
    phases = ["before", "during", "after"]
    
    contexts = [
        ['School', 'School'], 
        ['Refugee Accommodation', 'Refugee\nAccommodation'], 
        ['Kindergarten/daycare', 'Kindergarten/\ndaycare'],
        ['Care home', 'Care home'], 
        ['Nightlife', 'Nightlife'], 
        ['Recreational context', 'Recreational\ncontext'], 
        ['Hospital', 'Hospital'], 
        ['Work', 'Work'], 
        ['Residential Care Facility', 'Residential\nCare Facility'], 
        # ['Other', 'Other'], 
    ]

    # contexts = sorted(contexts, key= lambda x: (context_to_bins_cutoff[x[0]]["before"] + context_to_bins_cutoff[x[0]]["during"] + context_to_bins_cutoff[x[0]]["after"]) *context_to_outbreakcases_size[x[0]])[::-1]
    

    for data_dict, plotname in [[context_to_bins_cutoff, "with_cutoff"]]:#[[context_to_bins, "no_cutoff"], [context_to_bins_cutoff, "with_cutoff"]]:

        
        # Prepare lists for plot
        vals_contexts, vals_bins, vals_numbers, vals_numbers_abolut = [], [], [], []
        for context, cont_pretty in contexts:
            phase_dict = data_dict[context]
            for p in phases:
                vals_contexts.append(cont_pretty)
                vals_bins.append(f"... {p} the outbreak")
                vals_numbers.append(phase_dict.get(p, 0))
                vals_numbers_abolut.append(phase_dict.get(p, 0) * context_to_outbreakcases_size[context])



        # sns.set_theme(style="whitegrid",font_scale=1.5) 
        # # sns.set_theme(style="whitegrid",font_scale=2.2) 
        # fig, ax = plt.subplots(figsize=(12,6))
        # ax.set_facecolor("#FFFFFF")

        # # Define colors for the hues (before, during, after)
        # palette = ["#66C2A5", "#FC8D62", "#8DA0CB"]

        # # Plot with seaborn
        # # plt.figure(figsize=(12, 6))
        # sns.barplot(x=vals_contexts, y=vals_numbers, hue=vals_bins, palette=palette, saturation=1)
        
        # plt.ylim([0,3] if CLUSTER_TYPE[-1] == "1" else [0,1.5])
        
        # plt.xticks(rotation=45, ha="right")
        # plt.tight_layout()
        
        # plt.ylabel("Connected cases /\noutbreak cases in context")
        # # plt.xlabel("")
        
        # ax.legend(title="Registered ...")
        
        # outbreak_name = outbreak_name.replace('/', '-')
        # title_symbol = "=" if CLUSTER_TYPE[-1] == "0" else "≤"
        # plt.title(f"Genetic threshold {title_symbol} {CLUSTER_TYPE[-1]}") 
        
        # plt.savefig(f"plots/changes/{CLUSTER_TYPE}/{downsample_dir}{plot_name}_accumulated_{plotname}{downsample_filename}.svg")
        
        # plt.close()



        #############################

        sns.set_theme(style="whitegrid",font_scale=1.5) 
        # sns.set_theme(style="whitegrid",font_scale=2.2) 
        fig, ax = plt.subplots(figsize=(12, 6))
        # fig, ax = plt.subplots(figsize=(19, 15))
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)
        ax.spines['left'].set_visible(False)
        ax.spines['bottom'].set_visible(False)

        # Define colors for the hues (before, during, after)
        palette = ["#66C2A5", "#FC8D62", "#8DA0CB"]

        # Plot with seaborn
        # plt.figure(figsize=(12, 6))
        sns.barplot(x=vals_contexts, y=vals_numbers_abolut, hue=vals_bins, palette=palette, ax=ax, saturation=1)
        
        plt.ylim([0, 150] if CLUSTER_TYPE[-1] == "1" else [0,80])
        
        plt.xticks(rotation=45, ha="right")
        plt.tight_layout()
        
        plt.ylabel("Genetically connected community cases")
        # plt.xlabel("")
        
        # Compute sums for each context
        context_sums = {}
        for c, v in zip(vals_contexts, vals_numbers_abolut):
            context_sums[c] = context_sums.get(c, 0) + v

        # # For each category (context), place text above the group of bars
        # if downsample_filename != "":
        #     for i, context in enumerate(sorted(set(vals_contexts), key=lambda c: vals_contexts.index(c))):
        #         group_bars = [patch for patch in ax.patches if patch.get_x() <= i + 0.5 and patch.get_x() >= i - 0.5]
        #         if not group_bars:
        #             continue
        #         # Find the max bar height in the group to position text above
        #         max_height = max(bar.get_height() for bar in group_bars)
        #         ax.text(
        #             i,                        # x coordinate (center of group)
        #             max_height + 0.05,        # a little above tallest bar
        #             f"{int(context_sums[context])}",  # formatted sum
        #             ha="center", va="bottom", fontsize=12, fontweight="bold"
        #         )
        
        ax.legend(title="Registered ...")
        
        outbreak_name = outbreak_name.replace('/', '-')
        title_symbol = "=" if CLUSTER_TYPE[-1] == "0" else "≤"
        plt.title(f"Genetic threshold {title_symbol} {CLUSTER_TYPE[-1]}") 
        
        plt.savefig(f"plots/changes/{CLUSTER_TYPE}/{downsample_dir}{plot_name}_accumulated_absolut_{plotname}{downsample_filename}.svg")
        
        plt.close()

    
    
    
    
    
    
    output_dict = {
        
    }
    
    return output_dict
    
    
    
    

def panolinglineage_to_variant(lineage: str) -> str:
    """

    Args:
        lineage (str): _description_

    Returns:
        str: _description_
    """
    
    # content: list of ("panolin_lineage_part", "option of comparison", "variant")
    possibilities = [
        ("AY.", "startswith", "Delta"),
        ("B.1.617.2", "is", "Delta"),
        
        ("B.1.1.529", "is", "Omicron"),
        ("BA.1", "startswith", "Omicron"),
        ("BA.2", "startswith", "Omicron"),
        ("BA.3", "startswith", "Omicron"),
        ("BA.4", "startswith", "Omicron"),
        ("BA.5", "startswith", "Omicron"),
        ("BA.7", "startswith", "Omicron"),
        ("BA.2.75", "startswith", "Omicron"),
        ("BQ.1.1", "startswith", "Omicron"),
    ]
      
    # check all possibilities with the given lineage
    for lin_part, comparison, variant in possibilities:
        
        lineage = lineage.strip().replace(" ", "")
        
        if comparison == "is" and lineage == lin_part: 
            return variant
            
        if comparison == "startswith" and lineage.startswith(lin_part): 
            return variant
    
    # we only want to differentiate Omicron and Delta. All other are not relevant
    return "Not of interest"
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

if __name__ == "__main__":
    
    # load all input data
    dm = load_dm(DM_PATH)
    graph = load_graph(GRAPH_PATH)
    clusters = load_clusters(CLUSTERS_PATH)
    outbreaks = load_outbreaks(OUTBREAKS_PATH)
    
    subsamples = load_subsamples(SUBSAMPLING_PATH)
    
    
    outbreak_to_context = {}
    with open(OUTBREAKS_CONTEXT_PATH, "r") as context_infile:
        
        lines = context_infile.readlines()[1:]

        for line in lines:
            parts = line.strip().split("\t")
            outbreak_to_context[parts[0]] = parts[1]
    

    case_to_lineage_date = {}
    if os.path.exists("data/Supplementary_Table_Sequences.csv"): #TODO this is bad practice but works for now. Should b included in the snakemake
        
        sample_to_casid = {
            data["sample_id"] : node            
                for node, data in graph.nodes(data=True)
                    if "sample_id" in data
        }
        
        with open("data/Supplementary_Table_Sequences.csv") as f:
            reader = csv.DictReader(f, delimiter=';')
            for row in reader:
                sample_id = row["Sample_ID"]
                
                if sample_id in sample_to_casid:
                    case_to_lineage_date[sample_to_casid[sample_id]] = [ row["Pangolin_lineage"], datetime.strptime(row["Sampling_date"], "%d.%m.%Y")] 
            
        
        
    
    Path(f"plots/changes/{CLUSTER_TYPE}/outbreak_timediffs").mkdir(parents=True, exist_ok=True)
    Path(f"plots/changes/{CLUSTER_TYPE}/downsampling").mkdir(parents=True, exist_ok=True)
    
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
    output_dict_tmp = before_during_after_outbreaks(graph, outbreaks, outbreak_to_context, "Timediffs", case_to_lineage_date)
    output_dict.update(output_dict_tmp)
    
    
    
    ############ 4.5 create downsampled plots of the befor/during/after stuff
    # subsample_keys_wanted = [
    #     "subsample_1pc_rep1",
    #     "subsample_2.5pc_rep1",
    #     "subsample_5pc_rep1",
    #     "subsample_10pc_rep1",
    #     "subsample_15pc_rep1",
    #     "subsample_20pc_rep1",
    # ]
    
    # for key in subsample_keys_wanted:
    #     print(f"subsampling for {key}")
        
    #     # subsample the graph
    #     tmp_graph = graph.copy()
    #     for node, data in tmp_graph.nodes(data=True):
            
    #         if "sample_id" in data and data["sample_id"] != "":
                
    #             if not subsamples[key][node]:
                    
    #                 tmp_graph.nodes[node]["sample_id"] = ""
    #                 tmp_graph.nodes[node]["dm_index"] = ""
        
    #                 continue
        
        
        
    #     output_dict_tmp = before_during_after_outbreaks(tmp_graph, outbreaks, outbreak_to_context, "Timediffs", case_to_lineage_date,
    #                                                     downsample_dir="downsampling/", downsample_filename=f"_{key}")
        

    
    # # sanity check with random outbreak cases. Not really wated at the moment
    # if os.path.exists("data/graph/RandomOutbreakCases.csv"):
    #     outbreak_to_case_random = defaultdict(list)

    #     with open("data/graph/RandomOutbreakCases.csv") as f:
    #         reader = csv.DictReader(f, delimiter=';', quotechar='"')
    #         for row in reader:
    #             outbreak_to_case_random[row["originalOutbreak"]].append( row["caseID"] )

    #     output_dict_tmp = before_during_after_outbreaks(graph, outbreak_to_case_random, outbreak_to_context, "Timediffs_randomized", case_to_lineage_date)
    
    
    print("Save results")
    save_clusters(OUTPUT_PATH, output_dict)