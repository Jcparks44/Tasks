setwd('~/Desktop/Evolution/Tasks/Task_03')
#A sample is a subset of a population that you take measurements/data from to represent the whole population. 

#Make our population
trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
population1

trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
population2

#Take a sample of each population
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
Sample1
Sample2

#The samples are different because the population object was defined differently. Population one was a simulated random variate distribution across a population of size 1e^6 with a mean of 5 and standard deviation of 5. The second was the same size population but with varied by a mean of 4 and a standard deviation of 5. 


boxplot(Sample1, Sample2)
#The samples are different means and standard deviations which is shown using the boxplot. the lines across the box indicate the mean and the dashed lines are the standard deviation. The two boxes are in different positions according the data we typed in. 

#We're going to track the genes shared between four grandparents and two parents, Alan and Brenda. We can manipulate the number of loci within the len argument using the makeFounder function. We will model 10,000 loci from each grandparent and then follow those loci into Alan and Brenda, then to their kids. 

#Reading the needed functions
source('http://jonsmitchell.com/code/simFxn04.R')#### Fxn for multi-locus IBD tracker
makeFounder <- function(Name, len = 1e1, uni = T)	{
	z <- sapply(1:len, function(x) paste(Name, x, sep="-"))
	return(cbind(z,z))
}
makeGamete <- function( individual, rate = 1.6 )	{
	Ncuts <- rgeom(1, prob = 1 / rate) + 1
	Stop <- nrow(individual) - 1
	Spots <- sort(sample(1:Stop, Ncuts))

	for ( i in Spots )	{
		individual <- rbind(individual[1 : i, c(1,2)], individual[(i + 1) : nrow(individual), c(2,1)])
	}

	Gam <- individual[,sample(c(1,2),1)]
	return(Gam)
}
makeBaby <- function(mother, father)	{
	egg <- makeGamete( mother )
	sperm <- makeGamete( father )
	zygote <- cbind(egg, sperm)
	return(zygote)
}


#Make the Grandparents, Don't forget to use head() and nrow() to examine the objects.
MatGrandma <- makeFounder('grandma_mom')
head(MatGrandma)
nrow(MatGrandma)

MatGrandpa <- makeFounder('grandpa_mom')
head(MatGrandpa)
nrow(MatGrandpa)

PatGrandma <- makeFounder('grandma_da')
head(PatGrandma)
nrow(PatGrandma)

PatGrandpa <- makeFounder('grandpa_da')
head(PatGrandpa)
nrow(PatGrandpa)

Alan <- makeBaby(PatGrandma, PatGrandpa)
Alan
#Alan's genome

Brenda <- makeBaby(MatGrandma, MatGrandpa)
Brenda
#Brenda's genome is the name number as nrow for each parent. 

#Focus is Alan and Brenda's child
Focus <- makeBaby(Brenda, Alan)
Focus

#Each locus is tagged with which grandparent and parent it came from, and we can search to see how many genes came from any one ancestor of Focus. For this we use grep() which is basically like ctrlF function. It will find any matches. So grandpa_mom and grandma_mom both include 'mom' and will both match to the line below. We will find how many loci Focus inherited from Brenda, and then divide that by the number of loci present. That gives us the percentage of genes shared between Brenda and Focus. I think the number will be 10,000/2. 

ToMom <- length(grep('mom', Focus)) / length (Focus)
ToMom
#Okay so I wasn't dead on, but my fraction was right in a sense I guess.

#Now we'll look at how many genes Focus shares with each of his maternal grandparents. What should these numbers be? My guess is 0.125
ToMomMom <- length(grep('grandma_mom', Focus)) / length(Focus)
ToMomDad <- length(grep('grandpa_mom', Focus)) / length(Focus)
ToDadDad <- length(grep('grandpa_da', Focus)) / length(Focus)
ToDadMom <- length(grep('grandma_da', Focus)) / length(Focus)
ToMomMom
ToMomDad
ToDadMom 
ToDadDad
#Focus is not equally related to the four grandparents like I expected. 
AvgRelate <- mean(0.33765, 0.16235, 0.29125, 0.20875)
AvgRelate
#Focus' average relatedness to the four grandparents is 0.33765. This means that the majority of Focus' DNA in terms of grandparents is from the maternal grandmother

#Focus gets sibling
Sibling_01 <- makeBaby(Brenda, Alan)
Sibling_01

#I would expect sibling 1 to share approximately 50% of its DNA with Focus, but after seeing the relatedness of Focus to the grandparents, I have my doubts. 
ToSib <- length(intersect(Focus, Sibling_01)) / length(Focus)
ToSib
#0.4301, so not too far from 50. 

#Brenda and Alan have 1,000 more children lol
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/ length(Focus))
ManySiblings
#This object shows how much DNA each sibling has in common with Focus

quantile(ManySiblings)
mean(ManySiblings)

plot(density(ManySiblings), main='', xlab='proportion shared genes')
#There's a range of values for how many genes are shared between Focus and his many siblings becuase it is completely random. They share around 50% because there is such a large population of siblings, bringing it closer to the expected values. The less siblings he has the lower the distribution of traits, thus the lower they are in relatedness. 

# Given allele frequency 'p', we can calculate the expected genotype frequencies
HWE <- function(p)	{
	aa <- p^2
	ab <- 2*p*(1-p)
	bb <- (1-p)^2
	return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
#this function defines P as HWE and the equation aa, ab, bb are all parts to add up to get p. When P is 0.5, our values of aa, ab, and bb are 0.25, 0.50, and 0.25 respectively.

#Make a Blank Plot
plot(1, 1, type='n', xlim=c(0,1), ylim=c(0,1), xlab='freq. allele a', ylab='geno. freq')

#Calculate genotype frequencies for a bunch of allele frequencies.
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))

#Plot known allele frequency (p) against our expected genotype frequencies(GenoFreq)
lines(p, GenoFreq[,'aa'], lwd=2, col='red')

#GenoFreq represents our expected genotype frequencies so the y axis is what we expect to occur, and as the frequency of allele a increases, the likelihood of what we expect increases also. This graph is showing how the more allele a is present, the more likely ot is expressed at a rate that we would expect it to. The units on each axis represent a percentage. i.e. 0.8 is 80%

#Add other genotypes
lines(p, GenoFreq[,'ab'], lwd=2, col='purple')
lines(p, GenoFreq[,'bb'], lwd=2, col='blue')
legend('top', legend=c('aa', 'ab', 'bb'), col=c('red', 'purple', 'blue'), lty=1, lwd=2, bty='n')
#The plot is the defintion of HWE

Pop <- simPop(500)
#Add these simulation pop points to the HWE plot I just made
points(Pop[,'freqa'], Pop[,'Genotypes.aa']/500, pch=21, bg='red')

#The frequency of the aa genotype almost matches the expecation from HWE but it isn't exact. It's also clustered in one area, so it follows the trend but doesn't distribute throughout the whole trendline. I'm thinking if we had a larger population, it would though.

Pop<- simPop(50)
points(Pop[,'freqa'], Pop[,'Genotypes.aa']/50, pch=22, bg='red')

#The frequency of a has increased because our population is smaller. It is more frequently occurring because the likelihood of b is smaller in a smaller population size. 

install.packages('learnPopGen')
library('learnPopGen')
#Ne is how many individuals are in each population
#nrep is how many populations are simulating at once
#pause is how fast the lines grow 
x <- genetic.drift(Ne=5, nrep=5, pause=0.001)
#It's the same graph we saw in lecture today, the lines eventually separate out to 0 and 1 until the allele wipes out. Smaller population sizes make this happen faster.

#Let's look at how population sizes effect time to extinction for an allele. 
PopSizes <- 5:50

#There are 5 populations within each given size
Samples <- rep(PopSizes, 5)
Samples
#We will simulate all 230 of those populations and get the time one of the two alleles went extinct
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
tExt

#Linear model
Line <- lm(tExt ~Samples)
Line
summary(Line)

#to extract coefficients, use $coef
Line$coef
#To add it to the plot we use abline()
plot(Samples, tExt)
abline(Line)
Line2 <- lm( tExt~Samples +0 )
#Samples+0 gets rid of the intercept leaving the only coefficient the samples. The sample number is also smaller now. 

#As the population size increases, the points move away from the line. To me, this means that they are less likely to go extinct as population size increases. This is because the line is a function of the extinction of the allele in our samples. So as sample size increases, it is not as likely that our allele will go extinct. 

#Heteroskedasticity basically means what I previously said. Essentially, the variance of residuals is unequal over a range of values. This is exactly what our last graph and its residuals showed. Our summary() function defined our residuals in terms of numerical values and the graph provided a visual representation. Robust standard errors occur under populations so large that the errors are appropriated even under homoskedasticity. 
#It is obvious that the data we're given exhibits heteroskedasticity, so to find the robust standard error and corresponding t values we can use coeftest() and vcovHC() from the lmtest and sandwich packages, respectively. Robust simply means exclusion of outliers.

install.packages('lmtest') 
install.packages('sandwich')
#Run our linear regression again
summary(Line)
#Our standard error is 61.03 on 228 df.
bptest(Line)
#This test shows we can reject the null and that heteroskedacity is present. 
bptest(Line2)
#Line2 won't work because as I said, we got rid of the intercept. We need the intercept.
#To get the correct standard errors from Line we can use the vcovHC() from {sandwich} but I couldn't figure that out so I did the single step which was coeftest() with vcovHC()
coeftest(Line, vcov = vcovHC(Line))
#Because the source says our standard error should go up with this and mine went down, I will try their alternative method using lmrob() from the {robustbase} package.
install.packages('robustbase')
Linerob <- lmrob(tExt~Samples)
summary(Linerob)
#Okay, my standard error decreased yet again. I would think it should decrease though because aren't we ridding the data of outliers? 
#Let's see if I can add this to a plot..
plot(Samples, tExt)
abline(Line)
abline(Linerob)
#I kept the old line for comparison of the slope. Now I'm going to add color to the lines for clarification. The slope which is a rate, is lower with our robust line. (Blue)
pdf('r03ec-RobustHeteroskedasticity.pdf', height = 5, width = 5)
plot(Samples, tExt)
abline(Line, col='red')
abline(Linerob, col='blue')
legend(x=20,y=400, legend=c('Robust', 'Linear'), col=c('blue', 'red'), lwd=1.2, cex=0.8)
dev.off()


