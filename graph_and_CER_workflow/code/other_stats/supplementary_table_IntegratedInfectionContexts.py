"""

    
"""




import sys
from collections import defaultdict

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *



INFSOURCE_GRAPH_PATH = sys.argv[1]
INFSOURCE_IGN_SEQ_GRAPH_PATH = sys.argv[2]

EDGE_CONTEXTS_PATH = sys.argv[3]
OUTBREAK_CONTEXTS_PATH = sys.argv[4]

OUTPUT_CSV_PATH = sys.argv[5]



def extract_edge_frequencies(rename_edgetypes_dict: dict, inf_graph: object, edge_to_contexts: dict, outbreak_to_context: dict) -> dict:
    """

    Args:
        rename_edgetypes_dict (dict): _description_
        inf_graph (object): _description_
        edge_to_contexts (dict): _description_
        outbreak_to_context (dict): _description_

    Returns:
        dict: _description_
    """
        
    inf_edges_dict = defaultdict(int)
    for u,v, data in inf_graph.edges(data=True):
        
        # if the edge is forward or backward and has a context
        if (v,u,data["type"]) in edge_to_contexts:
            header_name = rename_edgetypes_dict[data["type"]] + " - " + edge_to_contexts[(v,u,data["type"])]
            inf_edges_dict[header_name] += 1
            
        # if the edge is forward or backward and has a context
        elif (u,v,data["type"]) in edge_to_contexts:
            header_name = rename_edgetypes_dict[data["type"]] + " - " + edge_to_contexts[(u,v,data["type"])]
            inf_edges_dict[header_name] += 1
            
            
        # if the edge is outbreak and has a context
        elif data["type"] == "Outbreak" and data["outbreak"] in outbreak_to_context:
            header_name = rename_edgetypes_dict[data["type"]] + " - " + outbreak_to_context[data["outbreak"]]
            inf_edges_dict[header_name] += 1
            
        # other edges
        else:
            inf_edges_dict[rename_edgetypes_dict[data["type"]]] += 1
    
    return inf_edges_dict


def find_edgetype_dist(inf_graph: object, inf_graph_ignoring_seqs: object, edge_to_contexts: dict, outbreak_to_context: dict, out_path: str) -> None:
    """

    Args:
        inf_graph (object): _description_
        inf_graph_ingoring_seqs (object): _description_
        edge_to_contexts (dict): _description_
        outbreak_to_context (dict): _description_
        OUTPUT_CSV_PATH (str): _description_
    """
    
    # graph_edge name :  table header name
    rename_edgetypes_dict = {
        "True_SelbeAdresse":    "SameAddress-differentName",
        "SelbeAdresseNachname": "SameAddress-sameName",
        "SurvnetKontaktperson": "Forward",
        "SurvnetAngestecktBei": "Backward",
        "Outbreak":             "Outbreak",
    }
    
    table_header_to_sortnumber = {
        "SameAddress-sameName" : 1,
        "SameAddress-differentName" : 2,
        "Forward" : 3,
        "Backward" : 4,
        "Outbreak" : 5,
    }
    
    #############   For the normal CER
    inf_edges_dict = extract_edge_frequencies(rename_edgetypes_dict, inf_graph, edge_to_contexts, outbreak_to_context)
    
    
    #############   For the CER without genetic sequences
    inf_edges_ign_seqs_dict = extract_edge_frequencies(rename_edgetypes_dict, inf_graph_ignoring_seqs, edge_to_contexts, outbreak_to_context)


 
    infedges_ign_seqs_sorted = sorted(list(inf_edges_ign_seqs_dict.items()) , 
                             key=lambda x: (-table_header_to_sortnumber[x[0].split(" ")[0]], inf_edges_dict[x[0]], x[1]) , 
                             reverse=True)
    
    
    

    # create an output file for snakemake
    with open(out_path, "w") as outfile:
        
        outfile.write('\t'.join([x[0] for x in infedges_ign_seqs_sorted]) + "\n")
        
        outfile.write('\t'.join([str(inf_edges_dict[x[0]]) for x in infedges_ign_seqs_sorted]) + "\n") 
        
        outfile.write('\t'.join([str(x[1]) for x in infedges_ign_seqs_sorted]) + "\n") 
        
        
        

def load_contexts(context_edges_path: str, context_outbreaks_path: str) -> dict:
    """

    Args:
        context_file_path (str): _description_
        context_outbreaks_path (str): _description_

    Returns:
        dict: _description_
    """
    # CaseID_Index
    # CaseID_Kontaktperson
    # Typ
    # Kontaktperson_Ort
    # KontaktpersonenID
    # DatumsDifferenz
    # Genetic_Distance
    # index1
    # index2
    # indexSearch
    # reg
    # otherCase
    # indexSearch2
    # KontextSearchID
    # context
    # context_manual
    
    case_pairs_to_context = {}
    with open(context_edges_path, "r") as context_infile:
        
        lines = context_infile.readlines()[1:]

        for line in lines:
            parts = line.strip().split(";")
            parts = [p[1:-1] if p.startswith('"') else p for p in parts ]# get rid of the ""
            casepair = tuple(parts[0:3])
            
            
            if parts[-2] != "NA":
                case_pairs_to_context[casepair] = parts[-2]
            
            if parts[-1] != "NA":
                case_pairs_to_context[casepair] = parts[-1]
                
    
    
    outbreak_to_context = {}
    with open(context_outbreaks_path, "r") as context_infile:
        
        lines = context_infile.readlines()[1:]

        for line in lines:
            parts = line.strip().split("\t")
            outbreak_to_context[parts[0]] = parts[1]
    
    
    
                
    return case_pairs_to_context, outbreak_to_context
    

if __name__ == "__main__":
    
    inf_graph = load_graph(INFSOURCE_GRAPH_PATH)
    inf_graph_ignoring_seqs = load_graph(INFSOURCE_IGN_SEQ_GRAPH_PATH)

    edge_to_contexts, outbreak_to_context = load_contexts(EDGE_CONTEXTS_PATH, OUTBREAK_CONTEXTS_PATH)
    
    
    find_edgetype_dist(inf_graph, inf_graph_ignoring_seqs, edge_to_contexts, outbreak_to_context, OUTPUT_CSV_PATH)
    

