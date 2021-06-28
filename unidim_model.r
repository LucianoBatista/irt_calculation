### Unidimensional model - Higth Example
### Loading necessary packages 
library(mirt)
library(tidyverse)
### Unidimensional model, Part 1 - Estimating model

### Importing data
mydata <- read_csv("example.csv") %>% 
  select(-X1)
### Inspecting data
head(mydata)

### Estimating IRT model
mymodel <- mirt(mydata, ### Data used to estimate model
                1,          ### Type of model to estimate. 1 means a unidimensional model
                itemtype = '3PL')          
### Inspecting items
for(i in 1:length(mydata)){
  ItemPlot <- itemfit(mymodel, 
                      group.bins=15,
                      empirical.plot = i,
                      empirical.CI = .95,
                      method = 'ML') 
  print(ItemPlot)
}

### Estimating latent heigth
latent_height <- as.vector(fscores(mymodel))
### Calculating sum score from survey
sum_score <- apply(mydata, 1, sum)
### Combining real height, sum score on surevey and latent height
height_measures <- data.frame(mydata[2], sum_score,
                             latent_height)
### Plotting real heigth vs sum score
plot(height_measures$TRUEHEIGHT,
     height_measures$sum_score)
### Plotting real heigth vs latent height
plot(height_measures$TRUEHEIGHT,
     height_measures$latent_height)
### Correlations between the different measures of height
cor(height_measures[1], height_measures[2])
cor(height_measures[1], height_measures[3])
cor(height_measures[2], height_measures[3])
### Extracting IRT parameters
irt_coefs <- coef(mymodel, simplify = T, IRTpars =T)
### Inspecting IRT parameters
irt_coefs
### Plotting IRT parameters 
plot(irt_coefs$items[,1],
     irt_coefs$items[,2],
     xlab = "Item discrimination",
     ylab = "Item difficulty")




### Unidimensional model, Part 2 - Wright Map
### Necessary packages
#install.packages('WrightMap')
library(WrightMap)
### Plotting wright map
wrightMap(latent_height, irt_coefs$items[,2])
### Plots, item level
### ICC for each item
plot(mymodel, type = 'trace')
### Item information, separate plots
plot(mymodel, type = 'infotrace')
### Item information, joint plot
plot(mymodel, type = 'infotrace', facet_items = F)
### Plots, test level
### Test information curve 
plot(mymodel, type = 'info')
### Test information curve with conditional standard error
plot(mymodel, type = 'infoSE')
### Conditional reliability 
#plot(mymodel, type = 'rxx')

