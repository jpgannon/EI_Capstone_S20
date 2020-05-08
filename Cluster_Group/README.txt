Water Table Cluster Analysis and Gap Filling Application

Authors: Robbie Coulter, Tri Le, Lily Chen, Lauren Boesch

###########Contents#############

###datacleaning.R###
Reads in raw water table data for all wells from HBEF and well info and location csv
Changes values of -99 to NA
Summarizes data to hourly and six hour intervals

###Cluster_Comparison.R###
Reads in hourly summary or six hour summary
Creates Normalized data set based on summary data
Creates list of list data structure needed for performing cluster analysis for summary and normalid data set
Iterates k-Shape and Dynamic Time Warping clustering algorithm with 3 clusters through 10 clusters
Calculates and records Internal Cluster Validity Indicies
Calculates external Cluster Validity Indicies to compare results with 6 clusters to HPU classes
Creates contingency table to view results comparing clusters to HPUs

###daily_precip_watershed_3.R###
Creates plots of precipitation in watershed 3, used for detecting events of high precipitation for use in cluster analysis

###heatmap.R###
Creates heatmap plot for visualizing comparison between a cluster analysis iteration and HPU classes

###gaps_app_test.R###
Creates synthetic gaps in data
Implements interpolation and linear regression gap filling on synthetic gaps
Calculates Root Mean Squared Error (RMSE) to test the validity of synthetic data

###finding_common_dates.R###
Generates plot to visualize data availability based on hourly summary of watershed 3 water tables

######App Folder#####

###app.R###
Script for running R shiny App

###data.zip###
Contains files:
clusters_with_HPU.csv: csv containing each well name along with the cluster it belongs to and HPU class
data_availability_chart.png: image demonstrating data availability based on the finding_common_dates.R script
dtw_results.csv: csv containing the cluster results from Dynamic Time Warping with 4 clusters based on the raw hourly summary evaluated over 1/1/2012 to 6/1/2012 to be loaded into application
gap_testing_clusters.csv: csv containing cluster results for use in gap filling analysis
gap_testing_data.csv: csv containing water table data for use in gap filling analysis
one_hour_summary.csv: csv of water table data to be loaded into application