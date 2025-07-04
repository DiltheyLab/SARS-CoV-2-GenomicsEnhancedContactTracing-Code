"""
Some general functions used in the SarsCov2 cluster analysis.

save_graph
load_graph

"""

from datetime import datetime
from collections import defaultdict
import networkx as nx
import json
import csv



#####################
##  set global default parameters

# matplotlib global params
import matplotlib.pyplot as plt
plt.rcParams['font.family'] = 'Arial'
plt.rcParams['font.size'] = 15


# use as global variables in other files
START_DATE = datetime.strptime("01.02.2021", "%d.%m.%Y")
END_DATE = datetime.strptime("31.12.2021", "%d.%m.%Y")    






#####################
##    Graph

def save_graph(graph: object, path: str) -> None:
    """
    Save graph to gml file

    Args:
        graph (object): nx Multigraph object
        path (str): filepath for gml file
    """
    print("Save graph", path, "...")
    nx.write_gml(graph, path)
    
    
def load_graph(path: str) -> object:
    """
    Load graph from gml file

    Args:
        path (str): Path to gml file

    Returns:
        object: nx Multigraph object
    """
    print("Load graph", path, "...")
    graph = nx.read_gml(path)
    
    return graph




#####################
##    load_subsampling csv

def load_subsamples(path: str) -> dict:
    """
    Load sequence subsampling from csv file

    Args:
        path (str): Path to csv file

    Returns:
        dict: key= subsampling_name, val= dict of cases to bools
    """
    print("Load subsampling", path, "...")
    
    subsampling = defaultdict(dict)
    with open(path, "r") as infile:
        
        csv_content = list(csv.reader(infile, delimiter=";"))
        
        subsa_names = csv_content[0][2:]
        
        for line in csv_content[1:]:
            case = line[0]
            for cell, name in zip(line[2:], subsa_names):
                subsampling[name][case] = cell == "1"
            
            subsampling["all"][case] = bool(line[1] )
        
        

    return subsampling

#####################
##    sample data

def load_samples(path: str) -> dict:
    """
    Load samples and metadata from json file

    Args:
        path (str): Path to json file

    Returns:
        dict: key= sample_id, val= sample metadata dict
    """
    print("Load samples", path, "...")
    with open(path, "r") as infile:
        samples_full = json.load(infile)

    return samples_full


#####################
##    caseid mapping data

def save_caseid_mapping(path: str, caseid_to_sid : dict) -> None:
    """
    Save mapping from caseids to sample ids (1 to 0-1)

    Args:
        path (str):  Path to csv file
        casid_to_sid (dict): key=caseid, val=sampleid
    """
    
    with open(path, "w") as outfile:    
        for case in caseid_to_sid:
            outfile.write(case + "," + caseid_to_sid[case] + "\n")
        

def load_caseid_mapping(path: str) -> dict:
    """
    Load mapping from caseids to sample ids (1 to 0-1)

    Args:
        path (str):  Path to csv file
        
    Returns:
        dict: key=caseid, val=sampleid
    """
    caseid_to_sid = {}
    with open(path, "r") as infile:    
        for line in infile:
            caseid, sid = line.strip().split(",")
            caseid_to_sid[caseid] = sid           
        
    return caseid_to_sid


#####################
##    clusters

def save_clusters(path: str, clusters : list) -> None:
    """
    Save clusters of one type to json file

    Args:
        path (str):  Path to json file
        clusters (list): list of cluster 
    """
    
    with open(path, "w") as outfile:        
        json.dump(clusters, outfile)
        

def load_clusters(path: str) -> dict:
    """
    Load clusters of one type from file

    Args:
        path (str):  Path to json file
        
    Returns:
        list :  list of clusters of one type
    """
    
    with open(path, "r") as infile:    
        clusters = json.load(infile)
    
    return clusters


#####################
##    distance data


def save_dm(path: str, dm : dict) -> None:
    """
    Save distancematrix to csv file

    Args:
        path (str):  Path to csv file
        dm (dict): "header" = list of sampleids, "table"= list of lists of distances
    """
    
    print("Writing dm ...")
    with open(path, "w") as outfile:
        # header: row and column names
        outfile.write("\t".join( [""] + dm["header"]) + "\n")
        
        # distances
        for i, row in enumerate(dm["table"]):
            outfile.write("\t".join( [dm["header"][i]] + list(map(str, row))) + "\n")
            
            
            
def load_dm(path: str) -> dict:
    """
    Load distancematrix from csv file

    Args:
        path (str):  Path to csv file
        
    Returns:
        dict: "header" = list of sampleids, "table"= list of lists of distances
    """
    
    dm = {}
    with open(path, "r") as infile:    
        lines = infile.readlines()
        
        dm["header"] = lines[0].strip().split()
        
        table = []
        for line in lines[1:]:
            
            line = line.strip().split()[1:]
            
            table.append( list(map(int, line)) )
            
        dm["table"] = table
        
    return dm
    
    