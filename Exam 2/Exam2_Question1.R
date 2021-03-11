setwd('~/Desktop/Evolution/Tasks/Exam 2')
#Question 1a
#The frequency of an allele is defined as the number of times an allele shows up in a population relative to the number of alleles at the same locus. Our sample size was 177. 127 were aa, 43 were ab, and 7 were bb. 
n <- 177 #sample size
aa <- 127/n
aa # frequency of aa
ab <- 43/n 
ab # frequency of ab
bb <- 7/n
bb # frequency of bb
# aa have no b alleles, ab have 1 b allele, and bb have 2 b alleles. Therefore, the allele frequency for b is the frequency of bb plus half of the frequency of ab.
b <- (bb + (0.5*ab))
b # frequency of allele b 
#To double check myself, I am calculating the frequency of allele a and adding it to b to make sure I get 1. 
a <- (aa + (0.5*ab))
a
a + b


#Question 1b
# Relative fitness of a genotype is defined as the survival or reproduction rate of a genotype in relation to the maximum survival of the other genotypes in a population. The maximum reproductive rate given was 7.7. Reproductive rate doesn't change between genotypes, just survival probability. The maximum survival rates are the same up to age 40. So before the age of 40, the fitness of all genotypes will be correlated only with the number of genotypes present in the population.
before40aa <- 0.995 * 127
before40aa # fitness of aa before the age of 40
before40ab <- 0.995 * 43
before40ab #fitness of ab before the age of 40
rbefore40ab <- before40ab / before40aa
rbefore40ab
# After 40, the fitnesses will change, as survival rate changes. Reproductive rate only changes with age, not genotype, because the age at reproduction is an average of 30.5 with a standard deviation of 6.7. 

age <- 40:55
age 
dnorm(age, mean = 30.5, sd = 6.7)
#This gives us the percentage of babies born to a mother from age 40-55. The percentage of babies born multiplied by the average number of viable offspring (7.7) should give us the reproductive rate of each genotype by age. 
percent_birth <- dnorm(age, mean = 30.5, sd = 6.7)
percent_birth * 7.7
#As far as calculating fitness goes, though, this wouldn't vary between genotype. Reproductive rate is consistent between each genotype. So looking at the probability of survival would be the key in determining fitness. Our sample size is 177. We have 127 aa individuals. At approximately age 45 on the graph, the survival probability diverges between genotypes. aa drops to 98% and we have to account for the fact that 2% of babies die. Therefore after age of 40, 96% of 127 is the number of surviving viable offspring for aa. We have 43 ab individuals. For ab, we're still at 100% survivability with 2% death of babies to give us 98% of 43. 
fit_aa <- 0.96 * 127
fit_aa
fit_ab <- 0.98 * 43
fit_ab
# In terms of relative fitness, we divide the fitness of our genotypes by the fitness of aa.
rfit_aa <- fit_aa / fit_aa
rfit_aa
rfit_ab <- fit_ab / fit_aa
rfit_ab
#The above calculates the fitness of ab relative to aa after the lines diverge. We need to look at ages 40-55 to determine overall relative fitness of ab to aa. To do this, I will repeat the process above for each age, then average those values to get a relative fitness for ab. Ages 40-45 are equal. 

fit_aa40 <- 0.98 * 127
fit_aa40
fit_ab40 <- 0.98 *43
fit_ab40
rfit_ab40 <- fit_ab40 / fit_aa40
rfit_ab40 
#Ages 45-50, the line for aa drops and plateaus at what looks like 98%
fit_aa45 <- 0.96 * 127 
fit_aa45
fit_ab45 <- 0.98 * 43
fit_ab45
rfit_ab45 <- fit_ab45 / fit_aa45
rfit_ab45
#Ages 50-55, the line for aa drops and plateaus at what looks like 95%
fit_aa50 <- 0.93 * 127 
fit_aa50
fit_ab50 <- 0.98 * 43
fit_ab50
rfit_ab50 <- fit_ab50 / fit_aa50
rfit_ab50
mean(c(0.3385827, 0.3456365, 0.356786))
#The average relative fitness of ab relative to aa is 0.35, and as age increases, a gradual increase is shown of the relative fitness value of ab. The main thing I learned from this question is that even though ab is a better genotype, with a higher probability of survival, it will take a long time for this genotype to be the most fit. Fitness is strictly number of offspring. This SERPINE1 allele doesn't confer reproductive ability, just survivability. It is only gradually increasing fitness in a small window: between ages 40-55 in this population. In this sample, aa still has the dominant frequency, thus more offspring. So even though ab promotes survival, it can never be the most fit in this given example. Because bb has no viable offspring and are all below the age of the 40, the fitness of bb relative to aa in this example is zero, which also effects its ability for a high fitness. 

#Question 1c
#The selection coefficient is s in the following equation: Relative Fitness = 1 - s. I determined the relative fitness to be 0.35 for genotype ab, therefore the selection coefficient is 0.65. Ab individuals have 65% fewer offspring than the aa individuals. The fitness of bb is 0, therefore bb is 100% selected against. 
#Selection happens when a variation increases reproduction. Evolution by selection is a result of a heritable variation that meets the previous criteria. Heritability is just the variation in a trait due strictly to genetics. The trait expressed and inherited here is survivability. Longevity is not at all due to environmental effects. Phenotype here is a direct expression of genetic variation. Heritability has two components: genetic variation divided by phenotypic variation. Relative allele frequency of b is the percentage of copies of b in this population. This is a measurement of genetic variation. We found the allele frequency b to be 0.16. 
b <- (bb + (0.5*ab))
b # frequency of allele b 
#Those that have the heterozygous genotype express the phenotype of longevity. We know this because if it were due to environmental factors, we would see that all Amish people have a longer lifespan, not just the ones with a genetic mutation. The example in class was number of eyes, all humans have two eyes, so there is zero heritability, it is environmentally driven. This example is full heritability. So h = 1. 

#Question 1d
#The change in F per generation is defined as 1/2Ne, so I started by finding the change in F by subtracting 0.035 from 0.017. The change in F is 0.018. To isolate Ne, I multiplied both sides by 2Ne. This gave me 1 = 0.036Ne. I divided 1 by 0.036 to get 27.8. 
delta_F <- abs(0.035-0.017)
delta_F
x <- delta_F * 2
Ne <- 1/x 
Ne
#Question 1e
#A new mutation spends an average of 4Ne generations as a polymorphism. Using the Ne calculated from above, 4 * 27.8 is 111.2. If a generation is 21 years:
Generations <- 4 * Ne
Years <- Generations * 21
Years #2,333 years that this locus will be polymorphic.
#The most likely allele to go to fixation is going to be allele b because in the notes within the powerpoint, the chance of fixation is 2s(Ne/N). The selection coefficient is relative fitness = 1 - s. If the fitness of aa is always 1, then that makes s = 0. We found ab to have a selection coefficient of 0.65. Allele b will be painfully slow, just because it only increases survivability of a mother aged 40-55, and the likelihood of reproducing at these ages is super small, as shown by dnorm(). 
