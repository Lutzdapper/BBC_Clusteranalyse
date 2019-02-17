######################################## Multivariate Verfahren ############################################
######################################## Anwendung einer hierarchischen Clusteranalyse  auf Nachrichtentexte der BBC  zur Clusterung nach Themenfeldern  #######################################################

## Leeren der Umgebung
rm(list = ls())

## Laden der Packages cluster und stats fuer die Clusteranalyse
library(cluster)
library(stats)

## Einlesen der 2.225 Artikel
load("/home/.../MultivariateMethoden/Daten/word_count.RData")

## Konvertieren der Daten in einen Dataframe
word_count <- as.data.frame(word_count)


######################################## Deskriptive Analysen zum Datensatz ###################################

# Berechnung der Anzahl der Woerter im 1. bzw. 2. Artikel (als Beispiel)
sum(word_count[1,]) # 220 Woerter im 1. Artikel
sum(word_count[2,]) # 192 Woerter im 2. Artikel

# Maximales Auftauchen eines Wortes in einem Artikel (Ergebnis= 81)
max(word_count)

# Vorkommen des Wortes der 1. Spalte in allen Artikeln (Ergebnis = 16)
sum(word_count[,1])

# Summary des Datensatzes
summary(word_count)

# Struktur des Datensatzes
str(word_count)

## Output: 2.225 Zeilen stellen die Artikel dar, 
## Die Anzahl der Woerter, auf die die Textdateien untersucht werden, betraegt 9.634

# Pruefen, ob Datensatz NA enthaelt (Ergebnis: keine NA-Werte)
sum(is.na(word_count))

# Vorkommen der Woerter in allen Texten 
# (Berechne von jeder Spalte(2) des Datensatzes word_count die Summe)
word_appearance <- apply(word_count,2, sum)

# Haeufigkeit des am wenigsten genannten Wortes (Ergebnis = 3)
min(word_appearance) 

# Haeufigkeit des am oeftesten genannten Wortes 
# (Ergebnis = 2830 Mal das Wort: "Jahr" steht an der Stelle 9.580 von word_appearance)
max(word_appearance)
which(word_appearance == max(word_appearance))

# Textlaenge der Artikel
text_length <- apply(word_count,1, sum)

# Berechnung des laengsten Textes
max(text_length)

# Berechnung des kuerzesten Textes
min(text_length)

# Histogramm der Textlaengen
hist(text_length, main = "Histogramm der Textlaengen", xlab ="Testlaenge", ylab = "Haeufigkeit")

# Boxplot der Textlaengen
boxplot(text_length, main ="Boxplot der Textlaenge")

######################################## Erstellen der Clusteranalyse ########################################


# Erstellen der Distanzmatrix
# dmat <- dist(word_count, method="canberra")
# save(dmat, file="dmat.RData")

# Laden der Distanzmatrix
load("/home/.../MultivariateMethoden/Daten/dmat.RData")

# Berechnung der Cluster mithilfe der Ward-Methode
clusteranalyse <- hclust(dmat, method="ward.D2")

# Dendrogramm erstellen
plot(clusteranalyse)

# Horizontale Linie ueber das Dendrgramm ziehen um die Clusteranzahl zu bestimmen
abline(h = 27000, col = "red")


## Ergebnis: nach Ward-Methode bilden wir 5 Cluster
# Begrenzung auf 5 Cluster (k ist Anzahl der Cluster, h ist die Distanz)
cluster <-cutree(clusteranalyse, k = 5, h = NULL)

#Skalieren der Datenmatrix
scale <- cmdscale(dmat, k = 5) 

#Plotten der Cluster
plot(scale, main= "Hierarchical Clustering", col =as.factor(cluster))

######################################## Erstellen der Wordclouds ############################################

# Den Datensatz gemaeß der Cluster aufteilen 
agg <- aggregate(word_count,
                 by = list(cluster),
                 sum)
colnames(agg)[1] <- "Cluster"


## Laden der Packages Wordcloud, tm, slam und NLP zur Darstellung der Cluster als Wordwolke
library(wordcloud)
library(tm)
library(slam)
library(NLP)

# wordcloud fuer das Cluster 1 (Wirtschaft)
wordcloud(colnames(agg),
          freq = agg[1,],
          max.words = 15,
          colors = c("red", "orange","yellow",  "green", "blue"))


# wordcloud fuer das Cluster 2 (Politik)
wordcloud(colnames(agg),
          freq = agg[2,],
          max.words = 15,
          colors = c("red", "orange","yellow",  "green", "blue"))

# wordcloud fuer das Cluster 3 (Technologie)
wordcloud(colnames(agg),
          freq = agg[3,],
          max.words = 15,
          colors = c("red", "orange","yellow",  "green", "blue"))


# wordcloud fuer das Cluster 4 (Entertainment)
wordcloud(colnames(agg),
          freq = agg[4,],
          max.words = 15,
          colors = c("red", "orange","yellow",  "green", "blue"))

# wordcloud fuer das Cluster 5 (Sport)
wordcloud(colnames(agg),
          freq = agg[5,],
          max.words = 15,
          colors = c("red", "orange","yellow",  "green", "blue"))

## Die Farben spiegeln die absolute Haeufigkeiten der Woerter wieder
## Die blauen Woerter kommen am haeufigsten vor, also kommen die roten Woerter am seltensten vor


######################################## Deskriptive Analysen der Cluster #####################################

# Kopieren des Dataframe um Veraenderungen vorzunehmen
word_count_cluster <- word_count

# Erstellung einer neuen Spalte "cluster" im neuen Dataframe
word_count_cluster$cluster <- cluster

# Verteilung der Artikel auf die Cluster
anz_cluster <- table(word_count_cluster[,length(word_count_cluster)])
anz_cluster

# Bildung von subsets der einzelnen Cluster
cluster1 <- subset(word_count_cluster, word_count_cluster$cluster == 1)
cluster2 <- subset(word_count_cluster, word_count_cluster$cluster == 2)
cluster3 <- subset(word_count_cluster, word_count_cluster$cluster == 3)
cluster4 <- subset(word_count_cluster, word_count_cluster$cluster == 4)
cluster5 <- subset(word_count_cluster, word_count_cluster$cluster == 5)

# Durchschnittliche Textlaenge in der Rubrik Wirtschaft
text_length_1 <- apply(cluster1[ ,-length(word_count_cluster)],1, sum)
mean(text_length_1)

# Haeufigkeit der verschiedenen Textlaengen in der Rubrik Wirtschaft
hist(text_length_1, 20, main = "Histogramm Textlaengen Wirtschaft", xlab ="Testlaenge", ylab = "Haeufigkeit")

# Durchschnittliche Textlaenge in der Rubrik Politik
text_length_2 <- apply(cluster2[ ,-length(word_count_cluster)],1, sum)
mean(text_length_2)

# Haeufigkeit der verschiedenen Textlaengen in der Rubrik Politik
hist(text_length_2, 20, main = "Histogramm Textlaengen Politik", xlab ="Testlaenge", ylab = "Haeufigkeit")

# Durchschnittliche Textlaenge in der Rubrik Technologie
text_length_3 <- apply(cluster3[ ,-length(word_count_cluster)],1, sum)
mean(text_length_3)

# Haeufigkeit der verschiedenen Textlaengen in der Rubrik Technologie
hist(text_length_3, 20, main = "Histogramm Textlaengen Technologie", xlab ="Testlaenge", ylab = "Haeufigkeit")

# Durchschnittliche Textlaenge in der Rubrik Entertainment
text_length_4 <- apply(cluster4[ ,-length(word_count_cluster)],1, sum)
mean(text_length_4)

# Haeufigkeit der verschiedenen Textlaengen in der Rubrik Entertainment
hist(text_length_4, 20, main = "Histogramm Textlaengen Entertainment", xlab ="Testlaenge", ylab = "Haeufigkeit")

# Durchschnittliche Textlaenge in der Rubrik Sport
text_length_5 <- apply(cluster5[ ,-length(word_count_cluster)],1, sum)
mean(text_length_5)

# Haeufigkeit der verschiedenen Textlaengen in der Rubrik Sport
hist(text_length_5, 20, main = "Histogramm Textlaengen Sport", xlab ="Testlaenge", ylab = "Haeufigkeit")
