# Topic Modeling Results

## Data

- Data used for topic modeling: `cleaned.csv` (193.5MB)
- This version was generated after removing stop words, white spaces by using the `light.RDS` (105MB). 
- Each time window with a duration of 10 years is around 20 MB in size.


## Trial 1: duration 1945:2022, interval 10
This version did not get read of punctuations and numbers. Top 1-2 words across various topics included punctuations and numbers without any semantic significance.

Execute the file [`LDA_trial1.R`](https://github.com/Jihyeonbae/UNGDC/blob/main/notebooks/exploratory/LDA_trial1.R) 

Outputs are stored in [`output`](https://github.com/Jihyeonbae/UNGDC/blob/main/output/lda/decade_0112).

Average time took for one interval: 15 minutes

## Trial 2: duration 1945:2022, interval 10
This version removed punctuations and numbers. I tried to store results in .json format, but couldn’t read in the results successfully. 


Execute the file [`LDA_trial2.R`](https://github.com/Jihyeonbae/UNGDC/blob/main/notebooks/exploratory/LDA_trial2.R)

Outputs are stored in [`output`](https://github.com/Jihyeonbae/UNGDC/blob/main/output/lda/decade_0114)

|Window|Time taken|
|---|---|
|(1945,1955] | 336.201000000001 seconds|
|(1955,1965] | 679.948 seconds|
|(1965,1975] | 785.632999999998 seconds|
|(1975,1985] | 1193.288 seconds|
|(1985,1995] | 960.959999999999 seconds|
|(1995,2005] | 688.788 seconds|
|(2005,2015] | 795.713000000003 seconds|
|(2015,2022] | 643.760999999999 seconds|

## Trial 3: duration 1945:2022, interval 10

This version removed punctuations and numbers. I saved results in .RDS format. 

Execute the file [`LDA_trial3.R`](https://github.com/Jihyeonbae/UNGDC/blob/main/notebooks/exploratory/LDA_trial3.R)

Outputs are stored in [output](https://github.com/Jihyeonbae/UNGDC/blob/main/output/lda/decade_0115).


## Trial 4: duration 1945:2022, subset, interval 10
This version removed punctuations and numbers. I saved results in .RDS format. 
Filtered statements that mention the keyword of interest “sovereignty,” leaving 4898 speeches. 
Execute file name [`LDA_trial4.R`]("https://github.com/Jihyeonbae/UNGDC/blob/main/notebooks/exploratory/LDA_trial1.R).

Outputs are stored in [`output`](https://github.com/Jihyeonbae/UNGDC/blob/main/output/lda/decade_0115_2).


## Trial 5: duration 1945:2022, subset, interval 10, 5 topics
This version removed punctuations and numbers. I also filtered statements that mention the keyword of interest "sovereignty," leaving 4898 speeches. 

|Window|Time taken|
|---|---|
|(1945,1955] | 101.34600000002 seconds|
|(1955,1965] | 333.904999999999 seconds|
|(1965,1975] | 363.908999999985 seconds|
|(1975,1985] | 1172.17999999999 seconds|
|(1985,1995] | 467.669999999984 seconds|
|(1995,2005] | 223.121000000014 seconds|
|(2005,2015] | 172.70199999999 seconds|
|(2015,2022] | 162.228999999992 seconds|


## Trial 6: duration 1945:2022, subset, interval 10, 5 topics
This time, I removed infix hyphens. 

|Window|Time taken|
|---|---|
|(1945,1955] | 97.3159999999916 seconds|
|(1955,1965] | 282.342000000004 seconds|
|(1965,1975] | 383.630999999994 seconds|
|(1975,1985] | 650.744999999995 seconds|
|(1985,1995] | 397.396999999997 seconds|
|(1995,2005] | 202.176999999996 seconds|
|(2005,2015] | 157.535000000003 seconds|
|(2015,2022] | 173.133999999991 seconds|


## Trial 7: duration 1945: 2022, subset, interval 10, 5 topics

included tf_idf option

|Window|Time taken|
|---|---|
|(1945,1955] | 97.3159999999916 seconds|
|(1955,1965] | 282.342000000004 seconds|
|(1965,1975] | 746.72199999998 seconds|
|(1975,1985] | 1401.39600000001 seconds|
|(1985,1995] | 1017.20199999999 seconds|
|(1995,2005] | 613.669000000024 seconds|
|(2005,2015] | 347.376000000018 seconds|
|(2015,2022] | 328.239000000001 seconds|

## Trial 8: duration 1945: 2022, subset, interval 10, 10 topics

saved resulted in sov_0118
removed stop words, punctuations, stemming, numbers, changed to lower cases

## Trial 9: duration 1945:2022, entire corpus, interaval 10, 10 topics

saved resulted in 0120
removed stop words, punctuations, stemming, numbers, changed to lower cases
ran topic modeling across the entire corpus, not just sovereignty. 

## Trial 10: duration 1945:2022, entire corpus, interval 10, 10 topics
saved results in 0203

I removed additional stop words (Will and must)

## Trial 10
saved results in 0120_replicate
trying to update lda_generator with new functions replacing deprecated options from quanteda package. 

New function generated too specific terms for each topic. To debug the issue, this reran the same code from LDA_trial7.R
