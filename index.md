# United Nations General Debate Corpus (UNGDC)

## Data Description
This corpus contains 10568 English transcripts of speeches delivered by state representatives of the UN member states at the United Nations General Debate from 1946 until 2022. Text data was made available by Baturo et al. (2017). Original data is in a form of .txt file, each containing speech transcript from one member state in a given year. This RDS dataset was generated by executing “RDS_generator.R” file to read in all .txt files into a single dataframe. File size is 58.61 MB. Alternatively, run “json_convertor.R” file to transform plain texts into structured .json files.

Datasets can be found [`here`](https://drive.google.com/drive/u/0/folders/1SaDhXeWakkTRtrlg5drTV_vOVJjgbuwL).
Three identifying variables across all documents are: ccode_iso (ISO 3-letter character country code), session (session number of the given UNGD meeting), year (year of UNGD).

Refer to the [`description.Rmd`](data/processed/description.Rmd) file for an overview of the dataset. 

|File | Description|
|-----------|------------|
|`cleaned.csv`| 10568 observations. "text" is cleaned after removing white spaces(multiple spaces and tab), digits followed by a dot. This does not exclude any stop words. |
|`light.csv`| 10568 observations. "text" does not have any stop words. |
|`meta.csv` | 10568 observations with 110 variables. This data contains country-year level information for each speaker country. Refer to [`codebook`](docs/control_variables_codebook.docx) for more description on each feature.|
|`liwc_controls.csv` | 10568 observations with 222 variables. This data contains country-year level information for each speaker country as well as psychological, linguistic constructs generated by LIWC. |

## LDA Analysis
[`UNGDC_topic_modeling_updated.qmd'](docs/UNGDC_topic_modeling_updated.qmd) renders LDA results. 
This version replaced deprecated functions from the quanteda package. 
It also presents a workflow with the goal of handling dynamic nature of topic models. Using correlation, this script shows the overlap between topics, represented with different terms over time. 


## References
Slava Jankin, Alexander Baturo, and Niheer Dasandi. “Words to Unite Nations: The Complete UN General Debate Corpus, 1946-Present.” OSF working paper, https://osf.io/6kty4

Alexander Baturo, Niheer Dasandi, and Slava Mikhaylov, “Understanding State Preferences With Text As Data: Introducing the UN General Debate Corpus” Research & Politics, 2017.
