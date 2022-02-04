# -*- coding: utf-8 -*-
"""
Created on Thu Feb  3 16:09:14 2022

@author: emmaa
"""

# Creates CSV file with data from NYC Open Data API
# https://data.cityofnewyork.us/

import pandas as pd
import yaml

# Load config file 
with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

# Get dataset, query, and filename from config
dataset = config['dataset']
query = config['query']
filename = config ['filename']

def nyc_open_to_csv(dataset, query, filename):
    
    # Concatenate URL
    url = 'https://data.cityofnewyork.us/resource/' + dataset + '.json' + query
    print('Using URL ' + url)
    
    # Get data with NYC Open Data API
    df = pd.read_json(url)
    
    # Export as csv
    df.to_csv(filename)
    print('Created ' + filename)
    
if __name__ == '__main__':
    nyc_open_to_csv(dataset, query, filename)
    



