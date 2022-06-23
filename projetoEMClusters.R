#Joao Funenga
#MAEBD - 61635

library(dplyr)

#Load CSV Data
dataSuicidesRawClusters = read.csv("suicides.csv")
#Change column names for easier access afterwards
names(dataSuicidesRawClusters) = c('country','year', 'gender', 'age', 'numberSuicides', 'population', 'suicidesPer100k', 'countryYear', 'HDI_Year', 'GDP_Year', 'GDP_PerCapita', 'generation')
dataSuicidesRawClusters$age = factor(dataSuicidesRawClusters$age, ordered = TRUE, 
                             levels = c('5-14 years','15-24 years','25-34 years', 
                                        '35-54 years','55-74 years','75+ years'))

head(dataSuicidesRawClusters)

#Drop hdi column and generation column because i'm not going to use them
dataSuicidesRawClusters$HDI_Year = NULL
dataSuicidesRawClusters$generation = NULL
dataSuicidesRawClusters$countryYear = NULL
head(dataSuicidesRawClusters)



########## Gráfico evolucao suicidios 1985 - 2016 ########## 


#average global suicide rate 
global_mean = (sum(dataSuicidesRawClusters$numberSuicides) / sum(dataSuicidesRawClusters$population)) * 100000

# group all dataSuicidesRaw from the same year to make the plot
suicides_global = group_by(dataSuicidesRawClusters, year)
#suicide global has 1 line per year, and in each line ALL the suicides with ALL the population to make a graph with suicide numbers across the years
suicides_global = summarise(suicides_global, population = sum(population), suicides = sum(numberSuicides), suicides_per_100k = (suicides / population) * 100000)

plot(suicides_global$year, suicides_global$suicides_per_100k, lwd = 0.5, ylab = "Suicidios Globais por 100k", xlab="Anos", cex=1.2, xlim=c(1985,2016))
lines(suicides_global$year, suicides_global$suicides_per_100k, type = "l", lwd = 2, col="blue")

#Line representing the global average between 1985-2016
abline(h=global_mean, lty=2)

global_mean



#3 variaveis para construir clusters
#populacao, suicidio, pib
#Z <- scale(dados[,-1], center=TRUE, scale= TRUE); Z

library(cluster)
library(factoextra)
library(gridExtra)
dadosClustering = dataSuicidesRawClusters[,c(6,7,9)]

Z <- scale(dadosClustering, center=TRUE, scale= TRUE); Z
fviz_nbclust(Z, kmeans, nstart = 3, method = "silhouette")
#best nr of clusters = 4

set.seed(1)
fitkm <- kmeans(Z, centers = 4, nstart = 10); fitkm 

p2 <- fviz_cluster(fitkm, data = Z) + ggtitle("k=4")
p2
#https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting

aggregate(dadosClustering, by=list(cluster=fitkm$cluster), mean)

dd <- cbind(dataSuicidesRawClusters, cluster = fitkm$cluster)
head(dd)

#ir buscar os dados de cada um dos 4 clusters
dadosCluster1 = dd[dd$cluster==1, ]
dadosCluster2 = dd[dd$cluster==2, ]
dadosCluster3 = dd[dd$cluster==3, ]
dadosCluster4 = dd[dd$cluster==4, ]


#####   Cluster 1   #####
#calculo de medias
mediasCluster1 = colMeans(dadosCluster1[,c(6,7,9)])
#nr de paises e quais
dfCluster1AuxCountries = as.data.frame(table(dadosCluster1[,1]))
dfCluster1OrderedCountries = dfCluster1AuxCountries[order(dfCluster1AuxCountries$Freq, decreasing = TRUE), ]
#contar pessoas por faixa etaria
dfCluster1AuxAge = as.data.frame(table(dadosCluster1[,4]))
dfCluster1OrderedAge = dfCluster1AuxAge[order(dfCluster1AuxAge$Freq, decreasing = TRUE), ]
#contar pessoas por genero
dfCluster1AuxGender = as.data.frame(table(dadosCluster1[,3]))
dfCluster1OrderedGender = dfCluster1AuxGender[order(dfCluster1AuxGender$Freq, decreasing = TRUE), ]



#####   Cluster 2  #####
mediasCluster2 = colMeans(dadosCluster2[,c(6,7,9)])
#nr de paises e quais
dfCluster2AuxCountries = as.data.frame(table(dadosCluster2[,1]))
dfCluster2OrderedCountries = dfCluster2AuxCountries[order(dfCluster2AuxCountries$Freq, decreasing = TRUE), ]
#contar pessoas por faixa etaria
dfCluster2AuxAge = as.data.frame(table(dadosCluster2[,4]))
dfCluster2OrderedAge = dfCluster2AuxAge[order(dfCluster2AuxAge$Freq, decreasing = TRUE), ]
#contar pessoas por genero
dfCluster2AuxGender = as.data.frame(table(dadosCluster2[,3]))
dfCluster2OrderedGender = dfCluster2AuxGender[order(dfCluster2AuxGender$Freq, decreasing = TRUE), ]


#####   Cluster 3   #####
mediasCluster3 = colMeans(dadosCluster3[,c(6,7,9)])
#nr de paises e quais
dfCluster3AuxCountries = as.data.frame(table(dadosCluster3[,1]))
dfCluster3OrderedCountries = dfCluster3AuxCountries[order(dfCluster3AuxCountries$Freq, decreasing = TRUE), ]
#contar pessoas por faixa etaria
dfCluster3AuxAge = as.data.frame(table(dadosCluster3[,4]))
dfCluster3OrderedAge = dfCluster3AuxAge[order(dfCluster3AuxAge$Freq, decreasing = TRUE), ]
#contar pessoas por genero
dfCluster3AuxGender = as.data.frame(table(dadosCluster3[,3]))
dfCluster3OrderedGender = dfCluster3AuxGender[order(dfCluster3AuxGender$Freq, decreasing = TRUE), ]



#####   Cluster 4   #####
mediasCluster4 = colMeans(dadosCluster4[,c(6,7,9)])
#nr de paises e quais
dfCluster4AuxCountries = as.data.frame(table(dadosCluster4[,1]))
dfCluster4OrderedCountries = dfCluster4AuxCountries[order(dfCluster4AuxCountries$Freq, decreasing = TRUE), ]
#contar pessoas por faixa etaria
dfCluster4AuxAge = as.data.frame(table(dadosCluster4[,4]))
dfCluster4OrderedAge = dfCluster4AuxAge[order(dfCluster4AuxAge$Freq, decreasing = TRUE), ]
#contar pessoas por genero
dfCluster4AuxGender = as.data.frame(table(dadosCluster4[,3]))
dfCluster4OrderedGender = dfCluster4AuxGender[order(dfCluster4AuxGender$Freq, decreasing = TRUE), ]


#metricas dos clusters
fitkm$withinss
fitkm$size
fitkm$betweenss
fitkm$totss

