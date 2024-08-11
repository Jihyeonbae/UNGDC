import os
import numpy as np
import pandas as pd
import gensim.downloader as api
from fse.models import uSIF
from sentence_transformers import SentenceTransformer


glove = api.load("glove-wiki-gigaword-100")

csv_path = os.path.expanduser("~/Downloads/sentence.csv")  # Adjust the path as necessary
sentence_df = pd.read_csv(csv_path)

# Extract sentences from the DataFram
sentences = sentence_df['sentence'].dropna().tolist()  # Drop NaN values if any

# Load the SentenceTransformer model
model = SentenceTransformer('sentence-transformers/average_word_embeddings_glove.6B.300d')

# Generate embeddings for the sentences
embeddings = model.encode(sentences)
output_dir = os.path.expanduser("~/Desktop/UNGDC/output/embedding_sentence")
output_path = os.path.join(output_dir, "sentence_embeddings.csv")

embeddings_df = pd.DataFrame(embeddings)
embeddings_df.to_csv(output_path, index=False)

print(embeddings_df.head())  

model2 = SentenceTransformer("all-MiniLM-L6-v2")
embeddings2 = model2.encode(sentences)
print(embeddings2.shape)