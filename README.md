# benchmarking-study-clustering-analysis

This code is part of a paper selected for presentation in the IFCS 2019 Cluster Benchmarking Challenge (https://ifcs.gr/wp-content/uploads/2019/02/Second_call_challenge3.pdf).
The goal of the study is to investigate how Density-Based Spatial Clustering of Applications with Noise (DBSCAN) 
and Spectral Clustering perform on simulated and real data sets with different characteristics. 
We also compare their performances with one of the most common and simple clustering techniques, k-means.

We work with five simulated and three real datasets. Each R file in the repository contains our code of running the three clustering methods on one dataset. The simulated data are created in the R code itself, so each of these files can be opened separately. The R files of the five simulated datasets are: multishapes_Rcode, spiral_Rcode, gaussian_distributions_Rcode, mixed_distributions_1_Rcode, and mixed_distributions_2_Rcode.  

For the real data, each RData file contains the data corresponding to the respective R code file. Therefore, the RData should be loaded before running the code in the R file. The R (and RData) files of the three real datasets are: bike_data_RCode (customerTrends.RData), forest_fire_RCode (forestfire.RData), and airbnb_RCode (santaClaraAirbnb.RData). 
