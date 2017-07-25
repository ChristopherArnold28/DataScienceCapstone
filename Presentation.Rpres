Data Science Capstone
========================================================
author: Christopher Arnold

Pitch for word predicting [application](https://chrisrtopher28.shinyapps.io/WordPredictor/)

This application is the capstone project for the Coursera Data Science Specialization through Johns Hopkins Univseristy and SwiftKey



Overview and Objective
========================================================
The purpose of this capstone was multifaceted

- Create a functioning application deployed to the web that can predict the next word in a phrase

- Showcase understanding of data science techniques and practices

- Build an algorithm for a type of data that was relatively unfamiliar at the onset

- Demonstrate a practical use of Data science through a real world application like Natural Language Processing

Model and Background
========================================================
Model Background

The model is based on the techniques described as part of the [Markov Chain](https://en.wikipedia.org/wiki/Markov_chain) 
We can predict the outcome of a series of words by looking at the number of occurrences of that series of words in a clean
complete corpora and choose the final outcome that occurs most frequently.

The words for prediction are split into [n-grams](https://en.wikipedia.org/wiki/N-gram) from unigrams up to six-grams. 

Algorithm and Performance:
=======================================================

This application is a translation of the above model. We have employed a few adaptations to predict more accurately:
![alt text](./Presentation-figure/ErrorPlot.png)

***
- Merging common and uncommon english words to improve word diversity while limiting memory allocation

- Search on variable phrase lengths from a pair of words up to a string of 6 words to maintain meaning and phrase integrity

- Performance was calculated from a subsection of the data, checking the predicted word vs the actual word. Performance increased as phrase length increased


Use of Application
========================================================
![alt text](./Presentation-figure/AppPicture.png)
***
- With this application text is entered in the main dialogue box in the middle

- Once a word is finished being typed, 3 predictions will appear below the box with the most predicted word on the left.

- Details with more predicted words are supplied in the "Plots" tab of the application


Useful Information
=======================================================



