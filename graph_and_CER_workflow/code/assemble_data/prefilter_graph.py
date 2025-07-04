"""

1. Read the graph from csv files


2. Filter
    - by timeframe
    - by datedifference
    - by unique kp-id

3. Build nx Graph

5. Save to file 


"""

from datetime import datetime
import networkx as nx
import sys
import re

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *

NODES_PATH = sys.argv[1]
EDGES_PATH = sys.argv[2]
OUTPUT_GRAPH_PATH = sys.argv[3]



def filter_case_data() -> tuple[list, list] :
    """
    Filters nodes and edges from the case graph and collect them in lists
    Filter by timeframe, real case id, datediff, kp id

    Returns:
        tuple[list, list]: nodes_data, edges_data (Lists that can be added to an nx graph)
    """
    print("Filter data ...")

    # CaseID	SeqID	Meldedatum	Impfung_Anzahl	Impfung_Datum	Impfung_Impfstoff
    # Count_Kontaktpersonen	Count_Kontaktpersonen_Düsseldorf	AbsonderungVon	Symptome
    # Lineage	Ns
    with open(NODES_PATH, "r") as innodes:
        nodes_all = [line.split("\t") for line in innodes.readlines()[1:]]

    # CaseID_Index	CaseID_Kontaktperson	Typ	KontaktpersonenID	Kontaktperson_Ort
    # DatumsDifferenz	Genetic_Distance
    with open(EDGES_PATH, "r") as inedges:
        edges_all = [line.split("\t") for line in inedges.readlines()[1:]]


    # filter nodes by daterange and prepare the list to be added to graph
    keep_nodes = []
    nodes_data = []
    for line in nodes_all:

        # if sample not in daterange
        date = datetime.strptime(line[2].split()[0], "%d.%m.%Y")
        if not (START_DATE <= date <= END_DATE):
            continue

        # if the caseid does not follow the syntax
        if not re.match("CVD2021-[0-9]+", line[0]):
            continue

        keep_nodes.append(line[0])

        # collect data for graph
        nodes_data.append([
            line[0],
            {
                "sample_id" : line[1],
                "date" : line[2].split()[0],
                "symptom_start": line[12].strip()
                # "date" : date
            }
        ])

    print("nodes:", len(keep_nodes))

    # filter edges by nodes, datediff and kp-id uniqueness
    
    all_san_edges = [
        [line[0],line[1]]
            for line in edges_all
                if line[2] == "SelbeAdresseNachname"]
    
    
    edges_data = []
    kp_ids_seen = []
    for line in edges_all:

        # both nodes must exist (in timeframe)
        id1, id2 = line[:2]
        if not (id1 in keep_nodes and id2 in keep_nodes):
            continue

        # Datediff must be <= 14 days absolut
        if not (-14 <= int(line[5].strip()) <= 14):
            continue

        # check if kp id duplication
        if line[3] in kp_ids_seen:
            print("SOS kp id duplication")
            print(line)
            exit(1)
        if line[3] != "":
            kp_ids_seen.append(line[4])

        # collect data for graph
        edges_data.append([
            line[0],
            line[1],
            "_".join(line[0:3]),
            {
                "type": line[2],
                "datediff": int(line[5].strip())
            }
        ])
        
        # add an edge if it is only a same adress but not same last name
        # TODO this could be better
        if line[2] == "SelbeAdresse":
            if [line[0],line[1]] in all_san_edges or [line[1],line[0]] in all_san_edges:
                continue
            
            edges_data.append([
                line[0],
                line[1],
                "_".join(line[0:2]+["True_SelbeAdresse"]),
                {
                    "type": "True_SelbeAdresse",
                    "datediff": int(line[5].strip())
                }
            ])
        

    print("edges:", len(edges_data))

    return nodes_data, edges_data





def build_nxgraph(nodes_data : list, edges_data : list) -> object :
    """
    Build nx graph from lists

    Args:
        nodes_data (list)
        edges_data (list)

    Returns:
        object: nx Multigraph object
    """
    print("Build graph ...")

    case_graph = nx.MultiGraph()

    case_graph.add_nodes_from(nodes_data)
    case_graph.add_edges_from(edges_data)


    # create dir for plots here before any are created. 
    Path(f"plots/").mkdir(parents=True, exist_ok=True)

    return  case_graph




if __name__ == "__main__":
    nodes, edges = filter_case_data()

    graph = build_nxgraph(nodes, edges)

    save_graph(graph, OUTPUT_GRAPH_PATH)


