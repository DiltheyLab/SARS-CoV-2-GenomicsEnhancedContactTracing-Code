"""
    
"""




import sys

from collections import defaultdict
from datetime import datetime
import re
 
from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *


if __name__ == "__main__":
    GRAPH_PATH = sys.argv[1]
    NODES_LIST_PATH = sys.argv[2]
    EDGES_LIST_PATH = sys.argv[3]

    OUTPUT_TXT = sys.argv[4]


def prefilter_nodes(nodes_all: list) -> list:
    """

    Args:
        nodes_all (list): _description_

    Returns:
        list: _description_
    """
    
    keep_nodes = []
    
    counter_after_date_filter = 0
    
    for line in nodes_all:

        # if sample not in daterange
        date = datetime.strptime(line[2].split()[0], "%d.%m.%Y")
        if not (START_DATE <= date <= END_DATE):
            continue
        
        counter_after_date_filter +=1

        # if the caseid does not follow the syntax
        if not re.match("CVD2021-[0-9]+", line[0]):
            continue
        
        keep_nodes.append(line)



    print(f"Total cases exported from SurvNet: {len(nodes_all)}")
    print(f"Cases after date filter (Feb-Dez 2021): {counter_after_date_filter}")
    print(f"Cases after case id regex filter (matches 'CVD2021-[0-9]+'): {len(keep_nodes)}")


    
    return keep_nodes






def compare_cpCVD_numbers(graph: object, cases_list: list, edges_list: list) -> None:
    """

    Args:
        graph (object): _description_
        cases_list (list): _description_
        edges_list (list): _description_
    """
    ######## cases_list column names
    # CaseID	
    # SeqID	
    # Meldedatum	
    # Impfung_Anzahl	
    # Impfung_Datum
    # Impfung_Impfstoff
    # Count_Kontaktpersonen     6
    # Count_Kontaktpersonen_Düsseldorf      7
    # AbsonderungVon
    # sv.Symptome
    # Lineage
    # Ns
    # Erkrankungsbeginn
    
    case_to_cp_count = {line[0]:int(line[6]) for line in cases_list}
    case_to_cp_count_d = {line[0]:int(line[7]) for line in cases_list}

    
    print("Sum of all contacts: ", sum(case_to_cp_count.values()))
    print("Sum of all düsseldorf contacts: ", sum(case_to_cp_count_d.values()))

    contact_edges = defaultdict(int)
    for u,v, key, data in graph.edges(data=True, keys=True):
        if data["type"] == "SurvnetKontaktperson":
            
            if [u,v] in edges_list:
                contact_edges[u] += 1
                edges_list.remove([u,v])
                
            elif [v,u] in edges_list:
                contact_edges[v] += 1
                edges_list.remove([v,u])
                
            # This did not happen
            else:
                print("ERROR: edge not found in the edge list")
                exit(1)
            
    
    
    print("Number of contact edges in the graph", sum(contact_edges.values()))
    
    c=0
    # This throws an error if something was wrong, otherwise nothing happens
    for case in case_to_cp_count:
        
        # this did not happen
        if case_to_cp_count[case] < case_to_cp_count_d[case]:
            print("ERROR: A cp count is smaller than a düsseldorf cp count.")
            exit(1)
            
        
        # this did not happen
        if case_to_cp_count[case] < contact_edges[case]:
            print("ERROR: A cp count is smaller than a number of contact edges.")
            exit(1)
            
            
        if case_to_cp_count_d[case] < contact_edges[case]:
            c+=1
            
    print("\nFor each case 'cp count >= cp düsseldorf count' was true")
    print("For each case 'cp count >= edges in graph' was true")
    print(f"For each case 'cp düsseldorf count >= edges in graph' was not true. For {c} cases")
            
        
        
        
        
"""
Output:


Total cases exported from SurvNet: 204111
Cases after date filter (Feb-Dez 2021): 32837
Cases after case id regex filter (matches 'CVD2021-[0-9]+'): 32830
Nodes in the graph: 32830

Sum of all contacts:  48131
Sum of all düsseldorf contacts:  39398
Number of contact edges in the graph 8187

For each case 'cp count >= cp düsseldorf count' was true
For each case 'cp count >= edges in graph' was true
For each case 'cp düsseldorf count >= edges in graph' was not true

"""
    
    
    


if __name__ == "__main__":
    
    # load needed input files
    graph = load_graph(GRAPH_PATH)
    
    #load nodes  and edges
    with open(NODES_LIST_PATH, "r") as innodes:
        nodes_all = [line.split("\t") for line in innodes.readlines()[1:]]
    
    # only keep index and cp cases so we know the edge directions
    with open(EDGES_LIST_PATH, "r") as inedges:
        
        lines = inedges.readlines()[1:]
        
        edges_list = [line.split("\t")[:2] 
                        for line in lines
                            if line.split("\t")[2] == "SurvnetKontaktperson"]
            


    cases_list = prefilter_nodes(nodes_all)
    print(f"Nodes in the graph: {len(graph.nodes())}\n")
    
    
    compare_cpCVD_numbers(graph, cases_list, edges_list)
    
    
    with open(OUTPUT_TXT, "w") as outfile:
        outfile.write("not needed")    
    
    