"""


1. Load genetic data: Distance matrix and all sample infos

2. Go through all samples and filter them. Keep:
    - Ns <= 3000
    - Feb.2021  <= Date <= End of December (?) 21

3. Go through DM and keep only selected samples
    - check if samples were kept but are not in the DM?

4. Save new distancematrix

"""

from datetime import datetime
import numpy as np
import json
import sys

from pathlib import Path
parent_dir = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(parent_dir))

from util_functions import *

DM_PATH = sys.argv[1]
SAMPLES_PATH = sys.argv[2]
OUTPUT_DM_PATH = sys.argv[3]
OUTPUT_SAMPLES_PATH = sys.argv[4]


def filter_samples():
    """
    Filter all samples by date and Ns
    """
    
    print("Loading samples ...")
    with open(SAMPLES_PATH, "r") as inf:
        samples_full = json.load(inf)["table"]


    print("Filtering samples ...")
    # apply basic filteres to all samples
    keep_samples = []
    samples_metadata = {}
    for sample in samples_full:

        # If not from the labs we want
        if not sample['pseudonym_prefix'] in ["Z","N","V"]:
            continue
        
        # if to many ambiguous characters
        if (not isinstance(sample["Ns"], int)) or sample["Ns"] > 3000:
            continue
        
        # If it does not fit in the timeframe of the analysis
        date = datetime.strptime(sample["id_samplingdate"], "%Y-%m-%d")
        if not (START_DATE <= date <= END_DATE):
            continue
        
        
        pseudo_id = str(sample['pseudonym_prefix']) + str(sample['pseudonym_numerical'])
    
        # sample ids for dm needed, pseudoids used in output
        keep_samples.append(pseudo_id)
        
        samples_metadata[pseudo_id] = {
            "date" : date.strftime("%d.%m.%Y"),
            "ambig": sample["Ns"]
        }
        

    return keep_samples, samples_metadata



def filter_dm(keep_samples : dict):
    """ 
    Filter the dm by keep_samples and create a reduced DM
    """
    
    print("Loading dm ...")
    dm_full = load_dm(DM_PATH)
    
    print("Filtering dm ...")
    
    # check if they are all in the dm
    not_in_dm = [s for s in keep_samples if s not in dm_full["header"]]
    if len(not_in_dm) > 0: # sanity check
        print(not_in_dm)
        print(len(not_in_dm))
        print("This should not happen!!!!")
        exit(1)
    
    
    # filter the dstance matrix with all keep samples    
    keep_indexes = [ dm_full["header"].index(s)  for s in keep_samples]
    
    np_dm = np.array(dm_full["table"])

    
    dm_small = np_dm[keep_indexes,:][:,keep_indexes]
    
    return dm_small



def save_results(keep_samples : dict, dm_small : list, samples_metadata : dict):
    """ 
    write resulting dm into csv file
    """    
    
    print("Samples after first filter step: ", len(keep_samples))
    
    if dm_small.shape[0] != len(keep_samples):
        print("dm shape not the same as sample count. This should not happen!!!!")
        exit(1)
    
    
    save_dm(OUTPUT_DM_PATH, {
            "header":keep_samples,
            "table": dm_small
        })   

            
    print("Writing samples data ...")
    with open(OUTPUT_SAMPLES_PATH, "w") as outfile:
        outfile.write(json.dumps(samples_metadata))




if __name__ == "__main__":
    
    keep_samples, samples_metadata = filter_samples()
    
    dm_small = filter_dm(keep_samples)
    
    save_results(keep_samples, dm_small, samples_metadata)




