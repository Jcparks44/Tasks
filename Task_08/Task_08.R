setwd('~/Desktop/Evolution/Tasks/Task_08')
#Uniting data and phylogenies into one grand thing

library('phytools')
tree <- read.tree('https://jonsmitchell.com/data/anolis.tre')
plot(tree, type='fan')
tree$tip.label
#Question 1: 82 tips, branch lengths are present, producing 161.
tree$edge.length

data <- read.csv('https://jonsmitchell.com/data/svl.csv', stringsAsFactors=F, row.names=1)
data
data[,1]
#Question 2: There are 100 dimensions in data, and it is a list of each species of lizard and their snout-vent length.

svl <- setNames(data$svl, rownames(data))
svl
#now we have an evolutionary tree and the body size of living species. Now we will reconstruct the ancestral states. Let s estimate how large the ancestors were using the phylogeny and the data.

Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
Ancestors
?fastAnc
#quesion 3: estimated values are stored in a list (ace), CI95 is the confidence interval. 
#question 4: state computed for the root node of the tree during Felsenstein's contrasts algorithm is also the MLE of the root node, and it assumes the contrast state is the variance. 

par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type='fan', lwd=2, show.tip.label=F)

#add points instead of names at the tips, the size of the points will be proportional to the size of the lizard. to do this, we'll use tiplabels() and tell it to plot points instead of words and to scale teh points by the size of each lizard using the parameter cex. We'll make sure the tips match the points by reorganizing the svl vector using the tip labels of the tree using square brackets. 
tiplabels(pch=16, cex=0.25*Ancestors$ace)
#now we'll add the ancestral states to the tree using nodelabels(), as each node
obj <- contMap(tree, svl, plot=F)
plot(obj, type='fan', legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))

# now we're going to add some fossils from 'Amber Fossils demonstrate deep-time stability of Caribbean lizard communities'

fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c('Anolis_aliniger', 'Anolis_aliniger', 'Anolis_occultus', 'Anolis_ricordii', 'Anolis_cristatellus', 'Anolis_occultus' ), tip2=c('Anolis_chlorocyanus', 'Anolis_coelestinus', 'Anolis_hendersoni', 'Anolis_cybotes', 'Anolis_angusticeps', 'Anolis_angusticeps'))
fossilData

#what we'll do is: for each fossil, find what node corresponds to the MRCA, of the pair of tips in the dataframe. 

#Question 5: i've set up most of the for loop. you'll need to add the actual for() line, and close the loop in appropriate places: 
#loops are fundamental structure of repitition. for() performs the same action for each item in a list of things for(item in list of items) #####{
###	do_something(item)
### }
{	
	for(i in 1:nrow(fossilData))
	i <- 1
	if( i == 1) {
	print(Ancestors)
	 
	}
}
##?fastMRCA
fossilNodes <- c()
nodeN <- c()
{	
	for(i in 1:nrow(fossilData))
	i <- 1
	if( i == 1) {
	print(Ancestors)
	 
	}
}
Node <- fastMRCA(tree, fossilData[i, 'tip1'], fossilData[i, 'tip2'])
Node
fossilNodes[i] <- fossilData[i, 'svl']
fossilNodes[i]
#Node
nodeN[i] <- Node

names(fossilNodes) <- nodeN
Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
Ancestors_withFossils
Ancestors_withoutFossils <- fastAnc(tree, svl, CI=TRUE, var=TRUE)
Ancestors_withoutFossils
plot(Ancestors_withFossils$ace, Ancestors_withoutFossils$ace, xlab = 'with fossils' , ylab = 'without fossils')
#question 7: Data with fossils increse the estimated ancestral sizes. 

#8-10
install.packages('geiger')
library('geiger')
#?fitContinuous
fitContinuous(tree, svl, model='BM')
fitContinuous(tree, svl, model='OU')
fitContinuous(tree, svl, model='EB') ##LOWEST AIC, best fit
?fastAnc
#fastAnc uses BM to determine what it assumes, and here EB has the lowest AIC, thus best fit. 
