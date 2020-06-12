# Benchmarking in cluster analysis: a study on Spectral Clustering, DBSCAN, and K-means

This repository includes the code and data which were used in our paper:

Nivedha Murugesan, Irene Cho, and Cristina Tortora (2020) Benchmarking in cluster analysis: a study on Spectral Clustering, DBSCAN, and K-means. IFCS 2019 conference proceedings, accepted.

Abstract: We perform a benchmarking study to identify the advantages and the drawbacks of three clustering algorithms: K-means, Density-Based Spatial Clustering of Applications with Noise (DBSCAN), and Spectral Clustering. The methods are performed on two simulated and three real data sets. The obtained clustering results are compared using Adjusted Rand Index (ARI) values, silhouette values, and run times. Although there is not one method that performs best on all types of data sets, we find that DBSCAN should generally be reserved for non-convex data with well-separated clusters or for data with many outliers. However, when it comes to data without any patterns, we believe that K-means or Spectral Clustering might be able to achieve better results.
