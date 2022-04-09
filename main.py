import pickle
import numpy
import pandas as pd

with open('all_embeddings_20k.pkl', "rb") as fIn:
    stored_data = pickle.load(fIn)
    all_embeddings = stored_data['embeddings']

df = pd.DataFrame(all_embeddings)
df.to_csv(r'embeddings_20k.csv')