# term project


library(readxl)
features2 <- read_excel("features2.xlsx")

library(ggplot2)

ggplot(features2, aes(as.factor(Label), totalWordsCount)) + geom_boxplot() + 
  labs(title = "Boxplots of total word count by objective vs
        subjective", x = "Label") 
  
ggplot(features2, aes(x = FW, y = txtcomplexity, color = RB)) + geom_point() + 
  labs(title = "Frequency of Foreign Words Against Text Complexity Colored by Adverbs",
       x = "Foreign Words", y = "Text Complexity")




h <- hist(features2$semicolon, main = "Histogram of Number of Semicolons Used", 
          xlab = "Semicolon", col = "purple")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))



barplot(tapply(features2$pronouns1st, features2$Label, median), xlab = "Label",
        ylab = "Median first person pronouns", main = "Barplot of median first person pronouns for
      objective vs subjective", col = c("red", "blue"))


table(features2$NNP)
summary(features2$NNP)
# since NNP has not values we can remove that column


library(reshape) 
features2 <- features2[,-19]

cor.mat <- round(cor(features2[,7:20]), 2) 
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x=X1, y=X2, fill=value)) +
  geom_tile() + xlab("") + ylab("") + ggtitle("Heatmap for word and grammar predictors") +
  scale_fill_distiller(palette="RdBu", limits=c(-1, 1))


g <- hist(features2$fullstops, main = "Histogram of number of full stops used", 
          xlab = "Full stops", col = "brown")
text(g$mids,g$counts,labels=g$counts, adj=c(0.5, -0.5))

# maybe have to scale full stop data

# maybe scale all of the data


library(GGally)

ggpairs(features2[, 29:33],
        lower=list(continuous=wrap("points", alpha=0.25, size=0.3)))


