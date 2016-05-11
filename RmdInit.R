#
# This R script is included in R markdown to initialize the  environment with
# libraries and commonly used objects, functions.
#

library(ggplot2)
library(gridExtra)
library(knitr)
library(car)
library(knitcitations)
library(RefManageR)
library(caret)
library(data.table)
library(reshape2)
library(stargazer) ## I'm guessing you might need to install this one.

cleanbib()

cite_options(style="markdown")

myTheme <- theme(axis.ticks=element_blank(),  
                 panel.border = element_rect(color="gray", fill=NA), 
                 panel.background=element_rect(fill="#FBFBFB"), 
                 panel.grid.major.y=element_line(color="white", size=0.5), 
                 panel.grid.major.x=element_line(color="white", size=0.5),
                 plot.title=element_text(size="8"))

rotateXaxisLabels <- theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Setup for KNITR Table Captions
tn = local({
  i = 0
  function(x) {
    i <<- i + 1
    paste('\n\n:', ' ', x, sep = '')
    # The : before Table tells pandoc to wrap your caption in <caption></caption>
  }
})
knit_hooks$set(tab.cap = function(before, options, envir) {
  if(!before)
    tn(options$tab.cap)
})
default_output_hook = knit_hooks$get("output")
knit_hooks$set(output = function(x, options) {
  if (is.null(options$tab.cap) == F)  
    x
  else
    default_output_hook(x,options)
})

# Disable scientific notation
options(scipen=999)

# Function that allows for easy multi column layouts of ggplot2 plots
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## Generates a dataframe summarizing VIF for all predictors of a model
generateVifSummary <- function(m){
  vif4 <- vif(m)
  problem <- sqrt(vif(m)) > 2
  
  vifDf4 <- data.frame(cbind(vif4, problem))
  vifDf4$problem <- as.logical(vifDf4$problem)
  
  return <- vifDf4
}

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Generates a dataframe of all correlations of variables in a dataset
# ordered by magnitude of correlation
# filter the data as appropriate before calling this function
generateCorrelationDF <- function(filteredTrainData) {
  d<-na.omit(filteredTrainData)
  d_cor <- as.matrix(round(cor(d),2))
  d_cor_melt <- arrange(melt(d_cor), -abs(value))
  
  c <- ncol(d)
  r <- nrow(d_cor_melt)
  numTail <- r- c
  d_cor_melt<- tail(d_cor_melt, numTail)
  
  Nth.delete<-function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
  
  corFrame <- data.frame(d_cor_melt)
  row.names(corFrame) <- seq(nrow(corFrame))
  corFrame <- Nth.delete(corFrame, 2)
  row.names(corFrame) <- NULL
  
  return <- corFrame
}

