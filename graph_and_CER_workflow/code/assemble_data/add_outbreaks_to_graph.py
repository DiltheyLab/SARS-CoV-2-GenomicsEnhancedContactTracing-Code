"""

1. Collect all outbreaks from csv file

2. Load the graph

3. Add edges for each outbreak

4. Save to file


"""

from datetime import datetime
from collections import defaultdict
import itertools
import sys

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *



GRAPH_PATH = sys.argv[1]
OUTBREAKS_PATH = sys.argv[2]
OUTPUT_GRAPH_PATH = sys.argv[3]



def load_outbreaks(outbreak_path: str) -> dict:
    """
    Read the outbreak csv file

    Returns:
        dict: key = outbreak id, val = list of case ids
    """
    # Aktenzeichen	AlterBerechnet	AusbruchInfo_Aktenzeichen	AusbruchInfo_GuidRecord	
    # AusbruchInfo_NameGA	Geschlecht	Meldedatum
    with open(outbreak_path, "r") as infile:
        outbreak_data = [line.split(";") for line in infile.readlines()[1:]]
        
    outbreaks = defaultdict(list)
    for line in outbreak_data:
        
        # if date not in timeframe
        date = datetime.strptime(line[6].split()[0], "%d.%m.%Y")
        if not (START_DATE <= date <= END_DATE):
            continue
        
        outbreaks[ line[4].strip() ].append(line[0].strip())

    return outbreaks



def add_to_graph(outbreaks : dict, graph: object) -> None:
    """
    Add outbreaks to the graph with edges. 
    Every node pair that is in the same outbreak gets an edge of type "OUTBREAK".

    Args:
        outbreaks (dict): key = outbreak id, val = list of case ids
        graph (object): nx Multigraph object
    """
    
    edges_add = []
    # for every distinct outbreak id
    for outbreak_id in outbreaks:
        outbreak_cases = outbreaks[outbreak_id]
        
        # skip f not large enough
        if len(outbreak_cases) < 2:
            continue
        
        # for every case pair in the outbreak
        for case1, case2 in list(itertools.combinations(outbreak_cases, 2)):
            
            # sanity chack if all nodes from outbreaks are already in graph
            if not ( graph.has_node(case1) and graph.has_node(case2) ):
                print("ERROR case not in graph,", case1, case2)
                exit(1)
            
            # collect edge data for graph
            edges_add.append([
                case1,
                case2,
                f"{case1}_{case2}_Outbreak",
                {
                    "type": "Outbreak",
                    "outbreak": outbreak_id
                }
            ])
        
    # add to graph
    print("Adding", len(edges_add), "new outbreak edges to graph from", len(outbreaks), "different outbreaks.")
    graph.add_edges_from(edges_add)
    



if __name__ == "__main__":
    outbreaks = load_outbreaks(OUTBREAKS_PATH)

    graph = load_graph(GRAPH_PATH)
    add_to_graph(outbreaks, graph)

    save_graph(graph, OUTPUT_GRAPH_PATH)

