"""

    
"""




import sys

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *
from draw_graphs_base_functions import *

code_dir = Path(__file__).resolve().parent.parent
import_dir = code_dir / "assemble_data"

sys.path.insert(0, str(import_dir))

from add_outbreaks_to_graph import load_outbreaks



GRAPH_PATH = sys.argv[1]
DM_PATH = sys.argv[2]
OUTBREAKS_PATH = sys.argv[3]


OUTPUT_TXT_PATH = sys.argv[4]




def output_file_gen() -> None:
    """
    just to produce an output file for snakemake. TODO avoid this
    """
        
    # text based output
    with open(OUTPUT_TXT_PATH, "w") as out_file:
        out_file.write("")



def draw_graphs_months(graph: object, dm: dict) -> None:
    """

    Args:
        graph (object): _description_
        dm (dict): _description_
    """
    
    month_to_nodes = defaultdict(list)
    
    for node, data in graph.nodes(data=True):

        
        month = datetime.strptime(data["date"], "%d.%m.%Y").month
        
        month_to_nodes[month].append(node)
        
    
    # Only do month 4. 
    # month_to_nodes = {4:month_to_nodes[4]}
    
    
    months_names = [ f"Month {month} of 2021" 
                        for month, nodes in month_to_nodes.items()]
    
    directory = f"plots/graphs/months/"
    filenames = [f"{directory}/month_{month}.svg" 
                    for month, nodes in month_to_nodes.items()]

    draw_each_node_group(graph, list(month_to_nodes.values()), dm, months_names, directory, filenames,
                         edge_directions=True, node_labels="None")
    



def draw_outbreaks(graph: object, dm: dict, outbreaks: dict) -> None:
    """

    Args:
        graph (object): _description_
        dm (dict): _description_
        outbreaks (dict): _description_
    """
    # mapping from sample id to node
    sid_to_caseid = {graph.nodes[node]["sample_id"]:node 
                        for node in graph.nodes 
                            if graph.nodes[node]["sample_id"] != ""}
    
    
    filled_outbreaks = []
    
    # expand the subgraphs by neigbhoring nodes and genetic similarity
    
    for outbreak_name, nodes in outbreaks.items():
        
        undir_graph = graph.to_undirected()
        nodes = [n for n in nodes if n in graph.nodes]
        subgraph = set(nodes)
                
        # add all nodes that are connected by contact edges
        for node in nodes:
            con_comp = nx.node_connected_component(undir_graph, node)
            subgraph.update(con_comp)
            
        # now add all nodes that have a distance of 0 or 1
        for node in list(subgraph):
            if graph.nodes[node]["sample_id"] != "":
                    
                node_dm_index = graph.nodes[node]["dm_index"]
                                
                # find the indexes of all samples with distance 0 to current node
                line = np.array(dm["table"][node_dm_index])
                indexes_2 = list(np.where(line <= 1)[0])
                
                # for each sample index check if it belongs to a caseid 
                # and then add to set
                for index2 in indexes_2:
                    sid = dm["header"][index2]
                    
                    # skip if not usale sid 
                    if sid not in sid_to_caseid:
                        continue

                    subgraph.add( sid_to_caseid[sid] )
                    
                    
        # add case connections again
        for node in list(subgraph):
            con_comp = nx.node_connected_component(undir_graph, node)
            
            subgraph.update(con_comp)
            
        
        filled_outbreaks.append( list(subgraph) )
    
    
    
    # draw with surroundings

    # give collected data to the draw function
    title_names = [ f"Outbreak {outbreak_name.replace('/', '-')} " 
                        for outbreak_name in outbreaks]
    
    directory = f"plots/graphs/outbreaks/"
    filenames = [f"{directory}/Outbreak_{outbreak_name.replace('/', '-')}_with_surrounding_cases.svg" 
                    for outbreak_name in outbreaks]
    

    draw_each_node_group(graph, filled_outbreaks, dm, title_names, directory, filenames,
                         node_labels="None", edge_directions=True)
    
    
    # draw without surroundings
    
    final_outbreaks_nodes = []
    for nodes in outbreaks.values():
        final_outbreaks_nodes.append([
            n for n in nodes
                if n in graph.nodes
        ])
    
    # give collected data to the draw function
    title_names = [ f"Outbreak {outbreak_name.replace('/', '-')} " 
                        for outbreak_name in outbreaks]
    
    # directory = f"plots/graphs/outbreaks/"
    filenames = [f"{directory}/Outbreak_{outbreak_name.replace('/', '-')}.svg" 
                    for outbreak_name in outbreaks]

    
    





    


def draw_graphs_custom(graph: object, dm: dict) -> None:
    """

    Args:
        graph (object): _description_
        dm (dict): _description_
    """
    
    
    # mapping from sample id to node
    sid_to_caseid = {graph.nodes[node]["sample_id"]:node 
                        for node in graph.nodes 
                            if graph.nodes[node]["sample_id"] != ""}
    

    
    # define node groups by hand
    small_ones = []
    subgraph_groups = [
        # [91],
        # [251],
        # [1670],
        # [2260, 510, 1761],
        # [939, 1436, 86, 1516, 1695, 2004,     651],
        # [638 ],
        # [1813],
        # [1096],
        # [1199],
        # [3044],
        # [2872],
        [1419, 3536, 2340, 2210, 258, 1598, 1173, 3212, 3582], #+ small_ones,
        # [407],
        [412, 722, 873, 66 ],# + small_ones,
        []
        # small_ones
    ]
    
    
    # read json file and match the ids to caseids
    with open("plots/graphs/months/test_4.json", "r") as infile:
        id_to_node = json.load(infile)
        
    subgraph_groups = [[id_to_node[str(i)] for i in group ]
                            for group in subgraph_groups]
    
    
    
    # expand the subgraphs by neighboring nodes and genetic similarity and then neighboring again
    filled_subgraphs = []
    for group in subgraph_groups:
        undir_graph = graph.to_undirected()
        subgraph = set(group)
                
        # add all nodes that are connected by contact edges
        for node in group:
            con_comp = nx.node_connected_component(undir_graph, node)
            
            subgraph.update(con_comp)
            
    
        # now add all nodes that have a distance of 0 or 1
        for node in list(subgraph):
            if graph.nodes[node]["sample_id"] != "":
                
                
                node_dm_index = graph.nodes[node]["dm_index"]
                
                                
                # find the indexes of all samples with distance 0 to current node
                line = np.array(dm["table"][node_dm_index])

                indexes_2 = list(np.where(line <= 1)[0])
                
                # for each sample index check if it belongs to a caseid 
                # and then add to set
                for index2 in indexes_2:
                    sid = dm["header"][index2]
                    
                    # skip if not usale sid 
                    if sid not in sid_to_caseid:
                        continue
                

                    subgraph.add( sid_to_caseid[sid] )
                    
                    
        # add case connections again
        for node in list(subgraph):
            con_comp = nx.node_connected_component(undir_graph, node)
            
            subgraph.update(con_comp)
            
        
        filled_subgraphs.append( list(subgraph) )
    
    
    # # save graphs to files
    # for i, node_list in enumerate(filled_subgraphs):
    #     tmp_subgraph = nx.induced_subgraph(graph, node_list).copy()
        
    #     Path(f"plots/graphs/custom/gmls/").mkdir(parents=True, exist_ok=True)
    #     save_graph(tmp_subgraph, f"plots/graphs/custom/gmls/month_4_group_{i}.gml")
        
    
    small_subgraphs = filled_subgraphs[-1]    
    filled_subgraphs = filled_subgraphs[:-1]
    
    # give collected data to the draw function
    title_names = [ f"Custom graph {i} " 
                        for i, _ in enumerate(filled_subgraphs)]
    
    directory = f"plots/graphs/custom/"
    filenames = [f"{directory}/Custom_graph_{i}.svg" 
                    for i, _ in enumerate(filled_subgraphs)]

    draw_each_node_group(graph, filled_subgraphs, dm, title_names, directory, filenames,
                         node_labels="id", edge_directions=False, small_subgraphs=small_subgraphs)
                        #  node_labels="None", edge_directions=False, small_subgraphs=small_subgraphs)
    


    # save node names
    for i, nodes in enumerate(filled_subgraphs):
        with open(f"plots/graphs/node_exports/Custom_{i}.json", "w") as outfile:
            json.dump(nodes, outfile)
    
    



if __name__ == "__main__":
    
    graph = load_graph(GRAPH_PATH)
    dm = load_dm(DM_PATH)
    
    
    draw_graphs_months(graph, dm)

    
    outbreaks = load_outbreaks(OUTBREAKS_PATH)
    draw_outbreaks(graph, dm, outbreaks)

    draw_graphs_custom(graph, dm)    
    

    output_file_gen()

