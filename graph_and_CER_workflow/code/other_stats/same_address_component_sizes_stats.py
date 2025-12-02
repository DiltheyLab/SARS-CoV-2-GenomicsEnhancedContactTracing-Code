"""
    
"""




import sys
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *



GRAPH_PATH = sys.argv[1]

OUTPUT_PATH = sys.argv[2]


def plot_same_address_component_sizes(graph: object, edge_type: str) -> None :
    """

    Args:
        graph (object): _description_
        edge_type (str): _description_
    """
    
    
    # collect all edges to be kept
    edges = []
    for edge in graph.edges:
    
        if graph.edges[edge]["type"] == edge_type:
            
            edges.append(edge[:2])
            
    
    # isolate the components
    components_graph = nx.Graph()
    components_graph.add_edges_from(edges)
    components = [list(c) for c in nx.connected_components(components_graph)]
    
    
    # calculate sizes and counts
    lengths = [len(c) for c in components]
    
    occurrances = [
        lengths.count(i)
            for i in range(2, max(lengths)+1 )
    ]
    
    
    ################################
    # plot same address component sizes
    
    
    plt.clf()
    # sns.set_theme(style="whitegrid", font_scale=2) 
    sns.set_theme(style="whitegrid", font_scale=2) 
    fig, ax = plt.subplots(figsize=(11, 6))
    
    # sns.set_theme(style="whitegrid",style="white") 

    # ax.grid(True) 

    # sns.despine() 

    ax = sns.barplot(x=list(range(2, max(lengths)+1 )), y=occurrances, color="#595959", width=.95)

    fontsize = 18 if max(lengths) < 13 else 10
    ax.bar_label(ax.containers[0], labels=[str(count) if count!=0 else "" for count in occurrances], fontsize=fontsize)
    plt.tight_layout()
    
    # ax.set_facecolor('#EAEAEA')
    # fig.patch.set_facecolor('white')
    
    if max(lengths) > 12:
        
        for i,label in enumerate(ax.xaxis.get_ticklabels()):
            label.set_visible(False)
            if (i) % 3 == 0:
                label.set_visible(True)
    
    
    name_mapping = {
        "True_SelbeAdresse" : "sameAddress-differentName",
        "SelbeAdresseNachname" : "sameAddress-sameName"
    }
    plt.xlabel(f"Connected component sizes of {name_mapping[edge_type]} edges")
    plt.ylabel("Count")
    
    plt.title(f"Total: {len(lengths)}, Mean: {round(np.mean(lengths),2)}")
    
        
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    # ax.set_xticks([])
    # ax.set_yticks([])
    
    plt.savefig(f"plots/same_address_components/{edge_type}_component_sizes.svg")
        
    
    
    

def save_resuls(path: str) -> None:
    """
    currently just makes an empty file to satisfy snakemake 

    Args:
        path (str): _description_
    """
    with open(path, "w") as out_file:
        out_file.write("")




if __name__ == "__main__":
    
    graph = load_graph(GRAPH_PATH)
    
    Path(f"plots/same_address_components/").mkdir(parents=True, exist_ok=True)

    plot_same_address_component_sizes(graph, "True_SelbeAdresse")
        
    plot_same_address_component_sizes(graph, "SelbeAdresseNachname")


    save_resuls(OUTPUT_PATH)
    
    