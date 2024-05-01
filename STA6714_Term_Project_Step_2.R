# STA6714 Term Project step 2 data analysis

# Benjamin Garcia

library(readxl)
library(ggplot2)
features2 <- read_excel("features2.xlsx")

str(features2)
round(cor(features2[,4:62]), 2)

table(features2$JJS, features2$Label)
table(features2$NNP)
table(features2$WRB)
table(features2$exclamationmarks, features2$Label)
table(features2$questionmarks)
table(features2$semicolon)
table(features2$ellipsis)
table(features2$TOs, features2$Label)

summary(features2)

# Let 1 = objective and 0 = subjective

table(features2$Label)/nrow(features2)

feat3 <- features2[,-c(1, 2, 15, 19, 31, 42, 51 )]

feat3$Label <- ifelse(feat3$Label == "objective", 1, 0)
table(feat3$Label)

round(cor(feat3), 2)

round(cor(feat3[,c(1,43:53)]), 2)

table(feat3$sentence1st)
table(feat3$sentencelast)

feat4 <- feat3[,-c(53,54)]

round(cor(feat4), 2)

pairs(feat4[,2:6])

scal_feat4 <- cbind(feat4[,1], scale(feat4[,c(2:53)]))

round(cor(scal_feat4), 2)

pairs(scal_feat4[,2:6])


library(reshape) 

cor.mat2 <- round(cor(scal_feat4[,5:19]), 2) 
melted.cor.mat2 <- melt(cor.mat2)
ggplot(melted.cor.mat2, aes(x=X1, y=X2, fill=value)) +
  geom_tile() + xlab("") + ylab("") + ggtitle("Heatmap for word and grammar predictors") +
  scale_fill_distiller(palette="RdBu", limits=c(-1, 1))

pcs <- prcomp(scal_feat4)
summary(pcs)
