library(RJSONIO)
source("F:/Education/Party.r") #hjælpefunktioner (ie partyToFile)

#indlæs data
Inside <- read.delim("F:/Education/insideFinish.txt") 
#Inside = Inside[sample(1:nrow(Inside), 1000),] #anvend sample af data

#summary(Inside)
newrow = c(0,0,0,0,5,0,0,0) #ig=0, dur=0, hf=5
Inside = rbind(Inside,newrow)
newrow = c(0,0,5,0,11,0,0,0) #ig=5, hf=11
Inside = rbind(Inside,newrow)
newrow = c(0,0,1,0,0,0,0,0) #ig=1
Inside = rbind(Inside,newrow)
newrow = c(0,0,11,0,0,0,0,0) #ig=11
Inside = rbind(Inside,newrow)

#angiv variabel typer
#Inside$alder = ordered(Inside$alder)
Inside$oprindelse = as.factor(Inside$oprindelse)
Inside$koen = ordered(Inside$koen)
Inside$ig = ordered(Inside$ig)
Inside$finish = ordered(Inside$finish)
Inside$dur = ordered(Inside$dur)
Inside$hf = ordered(Inside$hf)
Inside$n = as.integer(Inside$n)

#egentlige beregninger
ctrl = party::ctree_control(minsplit=20L, minbucket = 7L) #minsplit og minbucket er ganget op med 3 fordi tallene er summen over seneste 3 år
tree = party::ctree(finish~oprindelse+koen+ig+dur+hf,data=Inside,weights=Inside$n,control=ctrl) #byg træ



pred <- predict(tree, newdata=Inside)
library(caret)
confusionMatrix(pred, Inside$finish)

library(ROCR)
roc_pred <- prediction(pred[,1], Inside$finish)
plot(performance(roc_pred, measure="tpr", x.measure="fpr"), colorize=TRUE)

model <- train(finish~oprindelse+koen+ig+dur+hf, data = Inside, weights=Inside$n, method='ctree', tuneLength=10,
               trControl=trainControl(
                 method='cv', number=10, classProbs=TRUE, summaryFunction=twoClassSummary))
model
plot(model)



get_ctree_parts <- function(x, ...)
{
  UseMethod("get_ctree_parts")
}

get_ctree_parts.partynode <- function(x, pr, ...)
{
  if (is.terminal(x))
  {
    nodeCount <<- nodeCount + 1
    
    with (x,
          list(nodeID        = id,
               weights       = pr$weight[nodeCount],
               p             = info$p.value,
               probabilities = pr$probabilities[nodeCount,])
         )
  }
  else
  {
    with(x,
         list(nodeID       = id,
              split        = split$breaks,
              index        = split$index,
              variableName = split$varid,
              children     = lapply(x$kids, get_ctree_parts, pr))
        )
  }
}


calcW = function(nid, ws) 
{
  wSum=0
  nids = ws$id #optimering, markant hurtigere end at slå op ved brug
  wights = ws$w #optimering
  
  for (i in 1:length(ws$id)) {
    if (nids[i] == nid)  {
      wSum = wSum + wights[i]
    }
  }
  return(wSum)
}


GetWeights = function(dtree)
{
  data = data_party(dtree)
  ws = data.frame(w = as.integer(data$"(weights)"), id = data$"(fitted)") 
  term_nodes = as.data.frame(nodeids(dtree, terminal = T))
  return (apply(term_nodes, 1, calcW, ws))
}


GetProbabilities = function(partyObj, includeID=FALSE)
{
  term_nodes = nodeids(partyObj, terminal = T)
  pr = as.data.frame(predict(partyObj, id = term_nodes, type = "prob", simplify = T))
  if(includeID) pr$id = term_nodes
  return(pr)
}

nodeCount <- 0

get_tree = function(t)
{
 nodeCount <- 0
 pr = data.frame(probabilities = GetProbabilities(tree), weight = GetWeights(tree))
 return (get_ctree_parts(tree$node, pr))
}

useful_bits_of_irisct <- get_tree(tree)


fileConn<-file("ctree.js")
writeLines(toJSON(useful_bits_of_irisct), fileConn)
close(fileConn)
