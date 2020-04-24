autism <- read.csv("~/Documents/Christophe/autismclean", sep="")

library(boot)
View(autism_clea)
# Nombre de mot
length(unique(autism_clea$word))

#J’arrive pas à faire fonctionner boot() avec un data frame de facteur donc je converti le dataframe en vecteur de chaîne de caractères
autism_clea = as.character(autism_clea$word)

# Nettoyage mots courants
library(tidytext)
data("stop_words")
stop_words[1150,1]="autism"
stop_words[1150,2]="onix"
autism.clea = autism_clea[!(autism_clea %in% stop_words$word)] #102732 éléments

### Première méthode de calcul du nombre d'occurence par mot unique (lente +++)
# Initialisation vecteur du nombre d'occurence par mot unique
#n = NULL
# Pour chaque mot unique, combien de fois il apparait dans la base totale
#for (i in unique(autismclean$word))
#{ n[i] =  sum(autismclean$word == unique(autismclean$word)[i])
#print(i)}

### Deuxième méthode de calcul du nombre d'occurence, plus rapide
# Initialisation vecteur des mots uniques
list.word = unique(autism.clea) #19 972
# Initialisation vecteur du nombre d'occurence par mot unique
n.word = rep(0,length(list.word))
# Initialisation compteur du nombre de tour de la boucle
ntour=0
# Pour chaque case de la base totale, j'augmente le compteur du mot correspondant de 1 (plus rapide)
for (i in autism.clea)
{n.word[which(list.word == i)] = n.word[which(list.word == i)] + 1
ntour = ntour+1 ; print(ntour)}


#On met le vecteur du nombre d'occurence dans un data frame avec la liste des mots uniques :
summary(n.word)
data.word = data.frame(list.word, n.word)
names(data.word)=c("word","n")

# On sélectionne les 50 mots les plus fréquents
# Calcul du seuil d'occurence correspondant aux 50 mots les plus fréquents

seuil.occurence = quantile(data.word$n,1-(50/dim(data.word)[1])) # 188 : On va donc garder tous les mots apparaissant plus de 188 fois
data.50 = data.word[data.word$n >= seuil.occurence ,] # 50 observations, ça a marché
data.50$word=as.character(data.50$word)
View(data.50)

# On enregistre data.50 pour gagner du temps si le bootsrap bug après
write.table(data.50, "data.50" , sep="\t")

# bootstrapping with 1000 replications



# Je définis la fonction occurence qui retourne le nombre d’occurence d’un word dans le vecteur data. Je laisse la ligne d =  data[indices] qui semble être nécessaire pour sélectionner les échantillons de data avec remise.
occurence <- function(data, indices, word) {
  d <- data[indices] # allows boot to select sample
  return(sum(d == word))
}
# initialisation d'un tableau de résultat vide
results=matrix(NA,1000,50)
results=as.data.frame(results)

# Pour chacun des 50 mots, on calcule les 1000 valeurs de bootsrap et on les mets dans une colonne du tableau de résultats
for (i in 1:50) {
  result <- boot(data=autism.clea, statistic=occurence, R=1000, word=data.50$word[i])
  results[,i]=result$t # On remplit une colonne du dataframe des résultats avec les 1000 valeurs de bootsrap
  names(results)[i]=data.50$word[i] # On donne le mot en nom de variable du dataframe
  print(i)
}

library(prettyR)
describe(results)
data.50$mean = apply(results,2,mean)
data.50$median = apply(results,2,median)
for (i in 1:50) data.50$ic.025[i] = quantile(results[,i],.025) #IC
for (i in 1:50) data.50$ic.975[i] = quantile(results[,i],.975) #IC

write.table(results, "results" ,sep="\t")
write.table(data.50, "data.50" , sep="\t")
write.csv(data.50, "results")
View(results)

##### Violin plot ################################################# Ca marche pas j’arrive pas à charger ggplots il me dit que ma bibliothèque est corrompue.

# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)

# dataset
data <- results

data <- data %>%
  gather(key="text", value="value") %>%
  mutate(value = round(as.numeric(value),0))

# Plot
p <- data %>%
  mutate(text = fct_reorder(text, value)) %>% # Reorder data
  ggplot( aes(x=text, y=value, fill=text, color=text)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Assigned Probability (%)")

p


############################ From https://gabrifc.shinyapps.io/raincloudplots/


## Load the required libraries
library("ggplot2") 
library("tidyr") 
library("dplyr") 
library("cowplot") 
library("RColorBrewer") 
library("ggpubr") 
library("Hmisc") 
source("halfViolinPlots.R") 

## Load the data
inputData <- read.delim2("Wordstwitter.csv",
                         header = TRUE,
                         sep = ';',
                         quote = '',
                         check.names = FALSE,
                         dec = ',')

## Select the columns and reorder the data if needed
inputData <- select(inputData,c("autismawareness","amp","people","education","spectrum","school","child","acceptance","understanding","love","time"))

## Reformat the data for ggplot
plotData <- gather(inputData,
                   condition,
                   value,
                   colnames(inputData),
                   factor_key = TRUE) %>%
  filter(value != "") 

## And plot the data
ggplot(plotData, aes(x = condition, y = value, fill = condition, color = condition)) +
  ggtitle("Main Plot Title") +
  ylab("y Axis Title") +
  xlab("x Axis Title") +
  theme_cowplot() +
  scale_shape_identity() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0, 
                                   hjust = 0,
                                   vjust = 0)) +
  scale_colour_brewer(palette = "Reds") +
  scale_fill_brewer(palette = "Reds") +
  geom_point(position = position_jitter(0.15), 
             size = 2, 
             alpha = 1, 
             aes(shape = 16)) +
  geom_flat_violin(position = position_nudge(x = 0.25, y = 0),
                   adjust = 2,
                   alpha = 0.6, 
                   trim = TRUE, 
                   scale = "width") +
  geom_boxplot(aes(x = as.numeric(condition) + 0.25, y = value), 
               notch = TRUE, 
               width = 0.1, 
               varwidth = FALSE, 
               outlier.shape = NA, 
               alpha = 0.3, 
               colour = "black", 
               show.legend = FALSE) +
  coord_flip()
