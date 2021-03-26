setwd('~/Desktop/Evolution/Tasks/Task_07')
#Evolutionary Trees in R
library(phytools)
library(ape)
#input a tree and plot it
text.string <-
	'(((((((cow, pig), whale),(bat,(lemur, human))), (robin, iguana)), coelacanth
	), (gold_fish, trout)), shark);'
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width=2)

#help you read the nodes
nodelabels(frame='circle', bg='white', cex=1)

#you can determine how distant two lineages are by looking at when the recent node is.

#Question 1: The shark is more closely related to the goldfish.

#The object created in memory when we simulate or estimate a phylogeny is a list of class 'phylo'
#a list is a customizable object that can combine different objects of different types. A list might have a vector of real numbers with 'numeric' as its first and then a vector of strings with 'character' as its second element. Assigning our trees with phylo is a convenient way to tell special functions in R how to treat that object.
#An object of class phylo has 3 parts, at least. Just typing phylo doesn't give you the structure because it's hidden.

vert.tree

#Question 2: No branch lengths
str(vert.tree)

#Use simple tree to dig into phylo object.
tree <- read.tree(text='(((A,B), (C,D)), E);')
plotTree(tree, offset=1)
tiplabels(frame='circle', bg='lightblue', cex=1)
nodelabels(frame='circle', bg='white', cex=1)

#we can call tip labels using:
tree$tip.label

#if we look into the phylo object's edge component, we can see the structure of the phylogeny as a matrix
tree$edge

#each line on the phylogeny is called an edge. so each row of tree$edge corresponds to one of the lines (edges) of the phylogeny. the first number is where the line starts and the second is where it ends. So the first row starts at 6 and ends with 7
#now we'll use a real phylogeny of Anolis lizards
AnolisTree <- force.ultrametric(read.tree('https://jonsmitchell.com/data/anolis.tre'))

#unlike the previous tree, this one has lengths associated with each edge, so edge lengths
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main='', xlab='edge lengths for Anolis tree', ylim=c(0,50), xlim=c(0,6))

tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
tipEdges
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]

plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
#Each edge has a length, and they're in order. So the first value of edge length is the length of the edge defined by the first row of the edge matrix. 

?plot.phylo
#Question 3:
tree <- read.tree(text='(((A,B), (C,D)), E);')
plot.phylo(tree, type='phylogram', show.tip.label=FALSE, edge.color='purple')

#Question 4:
plot.phylo(tree, type='radial')

#Question 5:
plot.phylo(tree, tip.color = 'red')
#Question6-8:
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
which(Lengths == min(Lengths))
#Anolis occultis has the shortest
names(Lengths)
AnolisTree2 <- drop.tip(AnolisTree, 'Anolis_occultus')
plot(AnolisTree2, cex=0.25)


#ltt() plot shows how fast species appeared in this phylogeny - make a lineage through time plot
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
#The line is always going up, the slope is plateuing at the end. And this tells me that the lizards are approaching maximum number. The curve is approaching what looks like an asymptote.

fit.bd(AnolisTree, rho = 0.2)

#install.packages('treebase')
library('treebase')
library('ape')

Dolphins <- search_treebase("Delphinus", by='taxon', max_trees = 10)
WillWork <- sapply(Dolphins, function(x) try(is.ultrametric(x)))
which(WillWork == TRUE)
pdf('r07-DolphinPhylo.pdf', height =5, width=5)
DolphinPlot <- plot.phylo(Dolphins[[9]], cex = 0.35)
dev.off()
is.ultrametric(Dolphins[[7]])
bddolphin <- fit.bd(Dolphins[[7]], rho = 0.2)
bddolphin
#b = 0.6704 and d= 0.5892


Tigers <- search_treebase("tigris", by='taxon', max_trees=3)
WillWork <- sapply(Tigers, function(x) try(is.ultrametric(x)))
WillWork
pdf('r07-TigerPhylo.pdf', height =5, width=5)
TigersPlot <- plot.phylo(Tigers[[2]], cex=0.1)
dev.off()
bdtigers <- fit.bd(Tigers[[2]], rho = 0.2)
bdtigers
#b = 0.668, d = 0.6154

Warblers <- search_treebase("Basileuterus", by='taxon', max_trees=20)
length(Warblers)
pdf('r07-WarblerPhylo.pdf', height =5, width=5)
WarblersPlot <- plot.phylo(Warblers[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Warblers, function(x) try(is.ultrametric(x)))
WillWork
bdwarblers <- fit.bd(Warblers[[1]], rho =0.2)
bdwarblers
# b = 1.076, d = 0.9025

Elephants <- search_treebase('elephas', by='taxon', max_trees=25)
pdf('r07-ElephantsPhylo.pdf', height =5, width=5)
ElephantsPlot <- plot.phylo(Elephants[[12]], cex=0.35)
dev.off()
WillWork <- sapply(Elephants, function(x) try(is.ultrametric(x)))
which(WillWork == TRUE)
fit.bd(Elephants[[12]], rho = 0.2)
# b = 0.2163, d = 0.1874

Capybara <- search_treebase('Hydrochoerus', by='taxon', max_trees=25)
length(Capybara)
pdf('r07-CapybaraPhylo.pdf', height =5, width=5)
CapybaraPlot <- plot.phylo(Capybara[[8]], cex=0.5)
dev.off()
WillWork <- sapply(Capybara, function(x) try(is.ultrametric(x)))
which(WillWork == TRUE)
fit.bd(Capybara[[8]], rho = 0.2)
# b = 21.7442, d = 13.8


Tegu <- search_treebase("salvator", by='taxon', max_trees=2)
WillWork <- sapply(Tegu, function(x) try(is.ultrametric(x)))
pdf('r07-TeguPhylo.pdf', height =5, width=5)
TeguPlot <- plot.phylo(Tegu[[1]], cex=0.4)
dev.off()
fit.bd(Tegu[[1]], rho = 0.2)
# b = 0.06916 d = 0.06007

armadillo <- search_treebase("dasypus", by='taxon', max_trees=15)
pdf('r07-ArmadilloPhylo.pdf', height =5, width=5)
ArmadilloPlot <- plot.phylo(armadillo[[1]], cex=0.35)
dev.off()
WillWork <- sapply(armadillo, function(x) try(is.ultrametric(x)))
fit.bd(armadillo[[1]], rho = 0.2)
# b = 0.3758, d = 53.6889

killwhale <- search_treebase("orcinus", by='taxon', max_trees=10)
pdf('r07-killwhalekPhylo.pdf', height =5, width=5)
killwhalePlot <- plot.phylo(killwhale[[1]], cex=0.25)
dev.off()
WillWork <- sapply(killwhale, function(x) try(is.ultrametric(x)))
fit.bd(killwhale[[1]], rho = 0.2)
# b = 0.668, d = 0.6154

TreeFrog <- search_treebase("hyla", by='taxon', max_trees=2)
length(TreeFrog)
pdf('r07-HylaPhylo.pdf', height =5, width=5)
TreeFrogPlot <- plot.phylo(TreeFrog[[2]], cex=0.6)
dev.off()
WillWork <- sapply(TreeFrog, function(x) try(is.ultrametric(x)))
fit.bd(TreeFrog[[2]], rho = 0.2)
species9 <- TreeFrog[[2]]$tip

Bluebird <- search_treebase("Sialia", by='taxon', max_trees=10)
pdf('r07-BluebirdPhylo.pdf', height =5, width=5)
BluebirdPlot <- plot.phylo(Bluebird[[1]], cex=0.5)
dev.off()
bdbird <- fit.bd(Bluebird[[1]], rho = 0.2)
bdbird
WillWork <- sapply(Bluebird, function(x) try(is.ultrametric(x)))
species10 <- Bluebird[[1]]$tip
species10
#install.packages('diversitree')


#I am running birth-death models on each phylogeny to determine trends in b and d in terms of species on the phylogenies I made above. B and D are the parameters and lambda and mu are the probability density of diversity. Species number is included in the bd calculations, as they are based on the phylogenies I made. The lines that surround the tops of the bars are indicative of simulation values, which typically fall closely in line with observed values based on our phylogenies.
library('diversitree')
library('TreeSim')
bdModel <- make.bd(Bluebird[[1]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
coef(fit)
pdf('r07bd_model_bluebird[[1]]', height = 5, width = 5)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)
dev.off()

bdModel <- make.bd(TreeFrog[[2]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
pdf('r07bd_model_TreeFrog[[2]]', height = 5, width = 5)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)
dev.off()

bdModel <- make.bd(killwhale[[1]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
pdf('r07bd_model_killwhale[[1]]', height = 5, width = 5)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)
dev.off()


bdModel <- make.bd(armadillo[[1]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
pdf('r07bd_model_armadillo[[1]]', height = 5, width = 5)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)
dev.off()


#I kept having to ForceQuit R because it won't load the remainder of my functions. I tried reinstalling it. Restarted the computer twice. I will write the code to develop the PDFs for the rest of the phylogenies, but I will not have the PDFs uploaded to the folder. 

bdModel <- make.bd(Tegu[[1]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
pdf('r07bd_model_Tegu[[1]]', height = 5, width = 5)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)
dev.off()

bdModel <- make.bd(Capybara[[8]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
pdf('r07bd_model_Capybara[[8]]', height = 5, width = 5)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)
dev.off()


bdModel <- make.bd(Elephants[[12]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
pdf('r07bd_model_Elephants[[12]]', height = 5, width = 5)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)
dev.off()

bdModel <- make.bd(Warblers[[1]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
pdf('r07bd_model_Warblers[[1]]', height = 5, width = 5)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)
dev.off()


bdModel <- make.bd(Tigers[[2]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
pdf('r07bd_model_Tigers[[2]]', height = 5, width = 5)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)
dev.off()

bdModel <- make.bd(Dolphins[[7]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
pdf('r07bd_model_Dolphins[[7]]', height = 5, width = 5)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)
dev.off()
