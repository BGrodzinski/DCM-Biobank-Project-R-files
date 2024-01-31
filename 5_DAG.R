#Create a DAG for surgery -> DCM
#NB. code below was written for multiple DAGs, but this only uses one -- so there's a lot of loops of 1. This is unnecessary but shouldn't affect the outcome.
library(tidyverse)
library(dagitty)
library(ggdag)

csv<-read.csv("/Users/BenG/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/DAG/Biobank DAG.csv")

all_responses<-list(csv)

#Now only select edges and create a list of filtered dataframes (edges_all)
edges_all<-list()
for (res in 1:1) {
  temp<-all_responses[[res]]
  filtered<-temp %>% filter(!is.na(Line.Destination))
  edges_all[[res]]<-filtered
}

#Create a dictionary of id to text labels

labels_all<-list()

for (res in 1:1){
  temp<-all_responses[[res]]
  filtered<-temp %>% filter(is.na(Line.Destination))
  labels_all[[res]]<-filtered
}

labels_all<-bind_rows(labels_all)

labels_unique<-unique(labels_all$Text.Area.1)
labels_unique<- labels_unique[-1]
label_n<-seq(1:length(labels_unique))+1
label_dictionary<-data.frame(label_n, labels_unique)

##Now map edge dictionary against these labels to create a frame with it in text format
edge_index_text<-list()
for (i in 1:1) {
  temp<-edges_all[[i]]
  temp_out<-lapply(temp[,7:8], function(x) label_dictionary$labels_unique[match(x, label_dictionary$label_n)])
  edge_index_text[[i]]<-temp_out
}

#Now create a column containing formulae - this needs to be using the numbers rather than text
for (i in 1:1){
  temp<-edges_all[[i]]
  out<-within(temp, DAG_code<-paste(Line.Destination,Line.Source, sep=" ~ "))
  edges_all[[i]]<-out
}

dag_form<-list()
form_text<-list()
for (i in 1:1){
  temp<-edges_all[[i]]
  for (n in 1:length(temp$DAG_code))
  {
    form_text[[n]]<-as.formula(temp$DAG_code[n])
  }
  dag_form[[i]]<-form_text
}

#Then create another loop which collapses each list of edges into a single equation

dag_equations<-list()
for (i in 1:1){
  temp<-dag_form[[i]]
  dag_equations[[i]]<-paste(temp, collapse=" , ")
}
dag_equations#Paste these equations into the dagify command


labels<-list()
for (i in seq(1:length(label_dictionary$labels_unique)))
{labels[[i]]<-paste(label_dictionary$label_n[i], label_dictionary$labels_unique[i], sep = "\' =\'")
}
labels_final<-paste(labels, collapse="\', \'") #Paste these labels into the dagify labels command with the addition of a leading and trailing "'"
dag<-dagify(5 ~ 3 , 4 ~ 5 , 7 ~ 3 , 4 ~ 7 , 4 ~ 6 , 6 ~ 3 , 3 ~ 8 , 4 ~ 8 , 3 ~ 9 , 4 ~ 9 , 3 ~ 10 , 4 ~ 10 , 9 ~ 11 , 10 ~ 11 , 5 ~ 9 , 5 ~ 8 , 12 ~ 3 , 7 ~ 12 , 5 ~ 12 , 12 ~ 9 , 12 ~ 8 , 9 ~ 10 , 9 ~ 13 , 13 ~ 14 , 9 ~ 14 , 4 ~ 14 , 4 ~ 13 , 12 ~ 13 , 13 ~ 8 , 14 ~ 8 , 15 ~ 11 , 15 ~ 8 , 15 ~ 10 , 9 ~ 15 , 4 ~ 15 , 9 ~ 16 , 16 ~ 13 , 4 ~ 16 , 16 ~ 15 , 12 ~ 16,  labels=c('2' ='Page 1', '3' ='Surgery', '4' ='DCM', '5' ='Hypotension', '6' ='Positioning', '7' ='C spine manipulation', '8' ='Age', '9' ='Comorbidities', '10' ='Occupation', '11' ='Sex', '12' ='Mode of anaesthesia', '13' ='Smoking', '14' ='Alcohol', '15' ='Physical activity', '16' ='Obesity'),exposure="3", outcome="4")
ggdag(dag)+theme_dag()
ggdag(dag,use_labels="label")+theme_dag()
tidy_dag<-tidy_dagitty(dag)
ggdag_paths(tidy_dag, text=FALSE, use_labels="label")

ggdag_adjustment_set(dag, text = FALSE, use_labels = "label", shadow = TRUE)+theme_dag()

ggdag_adjustment_set(dag, text= FALSE, node_size = 4, use_labels="label",shadow=TRUE,expand_x = ggplot2::expansion(),expand_y=ggplot2::expansion())+theme_dag()

