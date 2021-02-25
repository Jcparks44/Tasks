setwd('~/Desktop/Evolution/Tasks/Task_05')
library(learnPopGen)
coalescent.plot()
?coalescent.plot
#Each simulation begins with 10 alleles which can be modfiied using the n= part of the coalescent.plot() fxn. 
pdf('r05-Question1.pdf', height=5, width=5)
coalescent.plot(n=20, ngen=20, colors=NULL)
dev.off()


#To determine how long it takes alleles to go extinct, I ran several coalescent.plot fxns, changing the number of generations. Then I ran it over and over without changing the number of generations. It appears to be highly variable as to when an allele goes to fixation, but the average is approximately 10 generations. 
pdf('r05-Question2.pdf', height=5, width=5)
coalescent.plot(n=10, ngen=15, colors=NULL)
dev.off()

pdf('r05-Question3.pdf', height=5, width=5)
coalescent.plot(n=10, ngen=5, colors=NULL)
dev.off()
#The lines represent parent-offspring relationships as stated by the help page, so I ran a simulation of one generation. The average was pretty consistently 2 from what I saw. 
#Fitness is very clearly demonstrated here, in that the alleles can only be widely distributed if the indvidual carrying these alleles is able to mate a lot. 
#The common ancestor of gen 0 typically would be alive, as a coalescence graph is just a simulation of a sample from a population to determine the patterns of recombination.
install.packages('coala') 
library('coala')
install.packages('phytools')
library('phytools')
#coala uses odd syntax and you can add functions using ('feat_') to build a model and tell it what kind of data you want to have output. i.e. (summary statistics, 'sumstat_') Then you use the simulate function on that built model to get your output.
?coala
?phytools

#Setting up a model
#Sample 5 individuals from 1 population
#Each person has 10 loci
#Each locus will have 500 bp
#Two copies per individual(diploid)
#We will feature mutation and recombination in fixed rates and then finally summarize the ouput by showing pedigrees for each locus, and the overall diversity
model <- coal_model(sample_size=5, loci_number=10, loci_length=500, ploidy=2) +
	feat_mutation(10) +
	feat_recombination(10) +
	sumstat_trees() +
	sumstat_nucleotide_div()


#Now we run the simulation: We can run more than one for the same parameters by changing nsim. 
stats <- simulate(model, nsim=1)


#Each locus has a measure of genetic diveristy called 'pi' which is a standard measure. It's the average number of differences at a locus between any two individuals
Diversity <- stats$pi

#The Diversity object has all different numbers and this is because in our model function we featured mutation and recombination. Therefore, in the simulation, the number of differences at each locus will differ by locus, and by individual.

#Each SNP has its own ancestry tree. We'll take a look.
Nloci <- length(stats$trees)
Nloci

#Look at the first SNP for the first locus
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
t1
#There are 10 tips, and we had 5 individuals, two alleles per indvidual 

Age1 <- max(nodeHeights(t1))
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
#The most recent common ancestor for 2nd SNP is much further back than is the 1st SNP

par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
#They do not match, not even close. 

#We can compare the patterns of descent and timing differences between these two SNPS at different loci. 
compare.chronograms(t1, t2) 

t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[2]][1])
compare.chronograms(t1_1, t1_2)
t1_3 <- read.tree(text=stats$trees[[3]][1])
compare.chronograms(t1_1, t1_3)

#Use R to compare all of the SNPs from all of the loci all at one time
#This is called FOR loop. FOR numbers 1, 2, 3, 4...Nloci, locus changes value. So the first time through locus= 1 and the second for locus= 2. This continues until locus= Nloci
#Loops are a way to tell the computer the same thing over and over again. And we will run a simulation several times and save each tree. It's the same thing as what we did above but instead of us changing it and doing it over and over, we can manipulate r to do it for us. 
for(locus in 1:Nloci)	{
	ntrees <- length(stats$trees[[locus]])
	for (n in 1:ntrees)	{
		if (locus == 1 && n == 1)	{
			outPhy <- read.tree(text=stats$trees[[locus]][n])
		}
		else {
			outPhy <- ape ::: c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
		}
	}
}
par(mfrow=c(1,1))
densityTree(outPhy)

#Changing recombination frequency
model <- coal_model(sample_size=5, loci_number=10, loci_length=500, ploidy=2) +
	feat_mutation(10) +
	feat_recombination(30) +
	sumstat_trees() +
	sumstat_nucleotide_div()
#After changing recombination freqeuncy, I expected to see more distribution among alleles, which I did. 

#Specifying more complicated models: Here, mutation rate varies in each of the 40 simulations.
model3 <- coal_model(10, 50) +
	feat_mutation(par_prior('theta', sample.int(100, 1))) +
	sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)

mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[['theta']])

pdf('r05-Model3Graph.pdf', height=5, width=5)
plot(mean_pi)
plot(theta)
plot(mean_pi, theta, xlab='Diversity', ylab='Mutation Rate', pch=16, cex=1.3, col='black', main='Diversity As a Result of Varied Mutation Rate')
abline(lm(mean_pi ~ theta), col='red')
dev.off()

###Extra Credit###
help(feat_selection)
install.packages('phyclust')
library('phyclust')
activate_ms(priority = 100)
activate_msms(priority = 500, download = TRUE)

model4 <- coal_model(c(13, 18), loci_number = 10, loci_length = 1000, ploidy = 2) +
	feat_selection(10, time = 1, population = 1) +
	feat_selection(25, time = 1, population = 2) +
	feat_size_change(0.5,
		population = 1, 
		time = '1', 
		locus_group = 'all') +
	feat_size_change(0.1, 
		population = 2, 
		time = '2', 
		locus_group = 'all') +
	feat_migration(1.2, 
		pop_to = 1, 
		pop_from = 2, 
		symmetric = FALSE, time = '3', locus_group = 'all') +
	feat_mutation(10) +
	sumstat_trees() +
	sumstat_nucleotide_div()
check_model(model4)
list_simulators()
stat2 <- simulate(model4, nsim = 40)
Diversity2 <- stat2$pi
Diversity2