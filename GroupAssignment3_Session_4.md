# Data Collection Group Assignment 1

## *Group Name - Team Analytics*

### 71610025 - BASAVA ESWARA KRISHNA ALURI
### 71610076 - SHANTNU GUPTA
### 71610032 - KARTIK MAHESHWARI
### 71610038 - MOHIT KALSI
******

# Problem description and changed Code

In this article we will look how to fit a Latent Topic model and interpret the topics. For illustration, I will demonstrate topic analysis based on SEC 10-K filings of 20 randomly chosen firms in the Technology sector from the Fortune 1000. (Recall what is 10K. It was discussed in class. Else, please scan through this [wiki link](https://en.wikipedia.org/wiki/Form_10-K)) In this homework, we are primarily interested in 2 sections in the 10-K - Items 1 and 1A (Business Description and Risk Factors, respectively).  Again, for illustrative purposes, I'll topic-analyse on Item 1 data for these 20 random Tech firms. Even though we did not cover this in any detail in class, the specific model we will be using in R for topic-analysis is the famous [LDA model](https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation). Going through the link is encouraged for better understanding the model, but optional from an exam point of view.

So, let's begin our journey...

The first step in topic mining is to process the text data and create a Document Term Matrix. For your homework we will process the text data and will provide the Document Term Matrix and Text data as R object (.Rds) so that you can directly load them in your R work space. For fitting and visualising a Latent Topic Model, we will need three packages.


-	tm - for creating DocumentTermMatrix 
-	maptpx - for fitting Latent Topic Model
-	wordcloud - for plotting wordclouds
- igraph - for plotting terms co-occurrence graph


If you have not installed these packages, please install them using command install.packages("package-name"). 

Clear the workspace and invoke required libraries

```{r, message=FALSE}
rm(list=ls()) # Clear the workspace

library("tm")
library("wordcloud")
library("maptpx")
library("igraph")
```

<justify>
Now read the text data and DocumentTermMatrix (DTM) in your R session with readRDS function. textdata is a data frame with 2 columns. First column is Company name and second column is text of Item 1- Business from 10K filing. From second column of this text data, I created DTM
</justify>
```{r}
textdata = readRDS(file.choose())     # Select RF.Technology.Rds OR RF.Technology.Rds data set 
dtm1 = readRDS(file.choose())         # Select dtm1.BD.Rds or dtm1.RF.Rds
```

[Recall that in the LDA, we (i.e., the user) have to input the number of latent topics we think there are in the corpus.] Now, let's say we think there are maybe 2 topics in there.... So, we want to fit a 2 topic model. 

[Of course, a priori, we wouldn't know how many topics are really there. So we do trial-and-error. We choose K = 2, then 3, then 4 and so on till we think the results look reasonable and interpretable.]

[Changes done in the line number 58 for the calculation of TF-IDF instead of TF]
```{r, warning=F}
dtm1 = weightTfIdf(dtm1,normalize = TRUE) # changes for tF IDF

K = 4 # Choose number of topics in the model
simfit = topics(dtm1,  K = K, verb = 2) # Fit the K topic model
summary(simfit, nwrd = 12)  # Summary of simfit model
```

Suppose there are D documents and T term-tokens in the corpus. And we are fitting K topics. Then, the Topic model gives us mainly two outputs:

One, a $\theta$ matrix of term-probabilities - which tells us for each term, what is the probability that the term belongs to each topic. So its dimension is T x K.

Two, a $\omega$ document-composition matrix - which is probability mass distribution of topic proportions in document. So its dimension is D x K.

E.g., let's view the term probability matrix $\theta$:

```{r}
simfit$theta[1:10,]
```

Let's sort this matrix with decreasing order of total term probability and check the few top terms

```{r}
a0 = apply(simfit$theta, 1, sum); 
a01 = order(a0, decreasing = TRUE)
simfit$theta[a01[1:10],]
```

Here you can see product, services, applications etc have higher probability for topic 2. Similarly we can see the $\omega$ matrix for documents.

```{r}
simfit$omega[1:10,]
```

We can say Document 1 loads heavily on topic 2 whereas document 2 loads heavily on topic 1. Document 3 is mix of topic 1 and topic 2   

Some terms have high frequency, others have low frequency. We want to ensure that term frequency does not unduly influence topic weights. So we normalize term frequency in a metric called 'lift'. 

The lift of a term is topic membership probability normalized by occurrence probability of the term. If lift of a term for a topic is high, then we can say that, that term is useful in constructing that topic.

Since topics function doesn't return lift matrix for terms we can write a simple function to calculate lift of each term.

Based on the number of terms in DocumentTermMatrix lift calculation may take some time. In your case it should be completed in 2-3 minutes

```{r, warning=FALSE}
t = Sys.time()
theta = simfit$theta
lift = theta*0;  sum1 = sum(dtm1)
for (i in 1:nrow(theta)){  
  for (j in 1:ncol(theta)){
    ptermtopic = 0; pterm = 0;
    ptermtopic = theta[i, j]     # term i's probability of topic j membership
    pterm = sum(dtm1[,i])/sum1   # marginal probability of term i's occurrence in corpus
    lift[i, j] = ptermtopic/pterm   # so, lift is topic membership probability normalized by occurrence probability
      }
}
Sys.time() - t # Total time for calculating lift
```

let's print lift for first 10 terms. [Remember, Lift will have the same dimension as the $\theta$ matrix.]

```{r}
lift[1:10,]
```


Now we have lift and theta for each term and each topic. We can plot a wordcloud for each topic in which 
terms will be selected if lift is above 1 and size will be proportional to term-probability. This wordclod will give us an idea of the Latent Topic. Let's plot top 100 terms in each topic


```{r, warning=FALSE, fig.height = 8, fig.width = 12 }
for (i in 1:K){       # For each topic 
a0 = which(lift[,i] > 1) # terms with lift greator than 1 for topic i
freq = theta[a0,i] # Theta for terms greator than 1
freq = sort(freq,decreasing = T) # Terms with higher probilities for topic i

# Auto Correction -  Sometime terms in topic with lift above 1 are less than 100. So auto correction
n = ifelse(length(freq) >= 100, 100, length(freq))
top_word = as.matrix(freq[1:n])

# Plot wordcloud
wordcloud(rownames(top_word), top_word,  scale=c(4,0.5), 1,
          random.order=FALSE, random.color=FALSE, 
          colors=brewer.pal(8, "Dark2"))
mtext(paste("Latent Topic",i), side = 3, line = 2, cex=2)
}
```

As we did in class, from these wordclouds we can label and interpret topics. To get clearer picture of topics, let's plot top 20 terms co-occurrence graph. As we did in topic wordcloud, first we will find top terms for a topic and then we will construct a co-occurrence matrix. Once co-occurrence matrix is constructed, we can plot this matrix using graph.adjacency function from igraph package.  Note that for better readability we are censoring this matrix for top 2 edges.


```{r}

for (i in 1:K){       # For each topic 
a0 = which(lift[,i] > 1) # terms with lift greator than 1 for topic i
freq = theta[a0,i] # Theta for terms greator than 1
freq = sort(freq,decreasing = T) # Terms with higher probilities for topic i

# Auto Correction -  Sometime terms in topic with lift above 1 are less than 30. So auto correction
n = ifelse(length(freq) >= 20, 20, length(freq))
top_word = as.matrix(freq[1:n])

# now for top 30 words let's find Document Term Matrix
mat  = dtm1[,match(row.names(top_word),colnames(dtm1))]

mat = as.matrix(mat)
cmat  = t(mat) %*% (mat)
diag(cmat) = 0

# Let's limit number of connections to 2
for (p in 1:nrow(cmat)){
  vec = cmat[p,]
  cutoff = sort(vec, decreasing = T)[2]
  cmat[p,][cmat[p,] < cutoff] = 0
}

#cmat[cmat <  quantile(cmat,.80)] = 0

graph <- graph.adjacency(cmat, mode = "undirected",weighted=T)

plot(graph,			#the graph to be plotted
 layout=layout.fruchterman.reingold,	# the layout method. 
 vertex.frame.color='blue', 		#the color of the border of the dots 
 vertex.label.color='black',		#the color of the name labels
 vertex.label.font=1,			#the font of the name labels
 vertex.size = .00001,   # Dots size
 vertex.label.cex=1.3)
 mtext(paste("Topic",i), side = 3, line = 2, cex=2)
}
```

Now we have lift matrix and also we have DocumentTermMatrix. So we can create a weighing scheme for each document and each topic, which will give proportion of a topic in a document. Here first I am defining a function and later I am calling it to calculate topic proportion in documents.

```{r}
eta = function(mat, dtm) {

mat1 = mat/mean(mat);  terms1 = rownames(mat1);
eta.mat = matrix(0, 1, ncol(mat1))

for (i in 1:nrow(dtm)){
    a11 = as.data.frame(matrix(dtm[i,])); 
    rownames(a11) = colnames(dtm)
    a12 = as.matrix(a11[(a11>0),]);  
    rownames(a12) = rownames(a11)[(a11>0)]; 
    rownames(a12)[1:4]
    a13 = intersect(terms1, rownames(a12)); 
    a13[1:15];	length(a13)
    a14a = match(a13, terms1); 		# positions of matching terms in mat1 matrix
    a14b = match(a13, rownames(a12))		
    a15 = mat1[a14a,]*matrix(rep(a12[a14b,], 
                                 ncol(mat1)), 
                             ncol = ncol(mat1))
    eta.mat = rbind(eta.mat, apply(a15, 2, mean))	
    rm(a11, a12, a13, a14a, a14b, a15)
  }
  eta.mat = eta.mat[2:nrow(eta.mat), ] 	# remove top zeroes row
  row.names(eta.mat)=row.names(dtm)
  return(eta.mat)
}

twc = eta(lift, dtm1)
head(twc)
```

Now we have topic proportion in a Document, we can find the top documents loading on a topic and read them for better interpretation of topics. 

Here first I am defining a function which first sorts twc matrix in decreasing order and then picks top n (n = 5) documents name. Then I am calling this function with required arguments and printing the company names for each topic

```{r}
eta.file.name = function(mat,calib,n) {
  s = list()                   # Blank List
  for (i in  1: ncol(mat))     # For each topic
  {
    read_doc = mat[order(mat[,i], decreasing= T),]  # Sort document prop matrix (twc)
    read_names = row.names(read_doc[1:n,])          # docuemnt index for first n document
    s[[i]] = calib[as.numeric(read_names),1]     # Store first n companies name in list  
      }
  return(s)
}

temp1 = eta.file.name(twc,textdata,5)

for (i in 1:length(temp1)){
  print(paste('Companies loading heavily on topic',i,'are'))
  print(temp1[[i]])
  print('--------------------------')
}

```

Similarly we can find top text document. Since these documents are very large I am not printing them here. You can uncomment the code below and print the documents to read them as per your requirement.

```{r}
eta.file = function(mat,calib,n) {
  s = list()                   # Blank List
  for (i in  1: ncol(mat))     # For each topic
  {
    read_doc = mat[order(mat[,i], decreasing= T),]  # Sort document prop matrix (twc)
    read_names = row.names(read_doc[1:n,])          # docuemnt index for first n document
    s[[i]] = calib[as.numeric(read_names),2]     # Store first n documents in list  
      }
  return(s)
}

temp2 = eta.file(twc,textdata,5)

# for (i in 1:length(temp2)){
#   print(paste('Documents loading heavily on topic',i,'are'))
#   print(temp2[[i]])
#   print('--------------------------')
# }

```

## Reason of Selection of TFIDF instead of TF
* For the data mining pre-processes, the TFIDF is yielding cleaner results. The results seem to be more appropriate and relevant for interpretation.
 
* After doing hit and trial for different values of K with combination of TF & TFIDF, it is conclusive that the results for K=4 are more relevant and yields result which can contribute to decision making and help them anticipate the risks more accurately in the coming year.

## Intepretations of the topics

### Topic 1: Manufacturing Specific

#### Word Cloud and Co occurrence graph
<html>
<body>
<img src="https://github.com/eswarkrishna/GroupAssignment/blob/master/Session4/K4_CLOUD1.png?raw=true" alt="Hauz khas">
</body>
</html>
<html>
<body>
<img src="https://github.com/eswarkrishna/GroupAssignment/blob/master/Session4/K4_C1.png?raw=true" alt="Hauz khas">
</body>
</html>
#### Analysis
The latent topic covers the new manufacturing processes for Solar-power and obtain IP rights on the same which can risk their credibility in the market. The companies are also worries about their inventories and risks associated with them.
 
 *******
 
### Topic 2: The Company Client Relationship
#### Word Cloud and Co occurrence graph
<html>
<body>
<img src="https://github.com/eswarkrishna/GroupAssignment/blob/master/Session4/K4_CLOUD2.png?raw=true" alt="Hauz khas">
</body>
</html>
<html>
<body>
<img src="https://github.com/eswarkrishna/GroupAssignment/blob/master/Session4/K4_C2.png?raw=true" alt="Hauz khas">
</body>
</html>
#### Analysis

The latent topic captures the companyâs relationship risks with their clients. The companies are worried about their clients in Health care, financial services, and their co-relation with Net sales which is driven by these clients. And also, the companies are worried about the net-sales associated with their clients. Companies are also worried about the regulatory risks from federal government.
 
 ********
 
### Topic 3:  Financial Specific
#### Word Cloud and Co occurrence graph
<html>
<body>
<img src="https://github.com/eswarkrishna/GroupAssignment/blob/master/Session4/K4_CLOUD3.png?raw=true" alt="Hauz khas">
</body>
</html>
<html>
<body>
<img src="https://github.com/eswarkrishna/GroupAssignment/blob/master/Session4/K4_C3.png?raw=true" alt="Hauz khas">
</body>
</html>
#### Analysis
The latent topic captures the risks of indebtness of the data centers. The changes in the packaging in the semi-conductor industry will impact the companies in million. The companies have also mentioned the risk about their credit facilities due to senior-unsecured.
 
 *********
 
### Topic 4: Competitors Acquisitions and Merger
#### Word Cloud and Co occurrence graph
<html>
<body>
<img src="https://github.com/eswarkrishna/GroupAssignment/blob/master/Session4/K4_CLOUD4.png?raw=true" alt="Hauz khas">
</body>
</html>
<html>
<body>
<img src="https://github.com/eswarkrishna/GroupAssignment/blob/master/Session4/K4_C4.png?raw=true" alt="Hauz khas">
</body>
</html>
#### Analysis
The latent topic captures the risks of the mergers and acquisitions of different companies which creates difficult challenges to maintain their current market with the increase in number of subscribers. The companies also feel the risk of advertising and search revenues.


