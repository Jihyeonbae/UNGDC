# BERT topic modeling

BERT tokenizes not conventional words, but wordpieces. Less frequentlly used words are therefore split into pieces. 
	
## bert.ipynb
	
- basic model without any modifications
	

 
- plain version didn't generate semantically meaningful term matrices, which included stop words. Seeing that BERTopic tutorial also generated &amp as one of the most frequent terms, it might make sense to feed in clean data.
	
### For the below trials, execute [`bert_trials.ipynb`](https://github.com/Jihyeonbae/UNGDC/blob/main/notebooks/exploratory/bert.ipynb) and change parameters accordingly. 
 
## Trial 1

- diversity = 0.5
- data = removed stop words
- problem : too specific (referring to country names like kosovo, vietnam)

## Trial 2
- diversity = 0.2
- takes 1 hour to run the script
- data = removed stop words
- results are saved in [ ]
- using "light.csv" as input (no stop words)

## Trial 3
- diversity = 0.2
- data = not removing stop words
- using "cleansed.csv" as input (including stop words)
- results were too country-specific. Each topic seemed to be centered around one country. 


## Trial 4
- diversity = 0.2
- using "cleaned.csv" as input
- data = not removing stop words using code
- Although this has stop words, c_tf_idf_ is used. 
- Results were still too country specific. 
- Results were still very country-specific even after allowing n-grams from 1-3. 
- 

## Trial 5
- diversity = 0.2

 
## Dynamic Topic Modeling 1

## Things to try

- ngram_range parameter allows us to keep bigrams when needed
- `from sklearn.feature_extraction.text import CountVectorizer`
- `vectorizer_model = CountVectorizer(ngram_range=(1, 3), stop_words="english")`
- `topic_model.update_topics(docs, vectorizer_model=vectorizer_model)

- changed the minimum document frequency from 5 to 10. 
- When the document frequency floor was 5, the results were too specific. 
	 
# Questions
## Document length
`https://maartengr.github.io/BERTopic/getting_started/tips_and_tricks/tips_and_tricks.html` 

- As a default, we are using sentence-transformers to embed our documents. However, as the name implies, the embedding model works best for either sentences or paragraphs. This means that whenever you have a set of documents, where each documents contains several paragraphs, the document is truncated and the topic model is only trained on a small part of the data. One way to solve this issue is by splitting up longer documents into either sentences or paragraphs before embedding them. 

