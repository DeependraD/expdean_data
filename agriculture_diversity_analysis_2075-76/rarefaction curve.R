# load libraries 
require(tidyverse)
require(vegan)

## simple curve generation with BCI data (included in vegan)
# load dataset
data(BCI)

# number of taxa observed in a sample (single row) 
S <- specnumber(BCI)

# minimum number of individuals that were observed in a sample summed acrossed all taxa
raremax <- min(rowSums(BCI))

# gives the expected species richness in random subsamples of size sample from the community.
# ideally the size of sample is smaller than total community size
Srare <- rarefy(BCI, raremax)

# plot rarefaction results
# rarecurve may be drawn from any subset of data, using custom samples (rows)
par(mfrow = c(1,2))
plot(S, Srare, xlab = "Observed No. of Species", 
     ylab = "Rarefied No. of Species",
     main = " plot(rarefy(BCI, raremax))")
abline(0, 1)
rarecurve(BCI, step = 20, # for subset of samples use: BCI[num_of_samples, ]
          sample = raremax, 
          col = "blue", 
          cex = 0.6,
          main = "rarecurve()")

## try out with a dataset from phyloseq package (available from Bioconductor)
# source('http://bioconductor.org/biocLite.R')
# biocLite('phyloseq')
library(phyloseq)

# import dataset
mothlist <- system.file("extdata", "esophagus.fn.list.gz", package = "phyloseq")
mothgroup <- system.file("extdata", "esophagus.good.groups.gz", package = "phyloseq")
mothtree <- system.file("extdata", "esophagus.tree.gz", package = "phyloseq")
cutoff <- "0.10"
esophman <- import_mothur(mothlist, mothgroup, mothtree, cutoff)

# extract original taxonomic unit (OTU), and convert it to a dataframe after transposing
# otu is initially taxonomically indexed by row.  
otu <- otu_table(esophman)
otu <- as.data.frame(t(otu))
sample_names <- rownames(otu)

# contains fit values for a rarecurve and curve itself
out <- rarecurve(otu, step = 5, sample = 6000, label = T) #low step size because of low abundance

# clean the fit values to get a list of dataframes
rare <- lapply(out, function(x){
  b <- as.data.frame(x)
  b <- data.frame(OTU = b[,1], raw.read = rownames(b))
  b$raw.read <- as.numeric(gsub("N", "",  b$raw.read))
  return(b)
})

# rename the list
names(rare) <- sample_names

# convert the list of dataframes to a singe dataframe by binding together
rare <- map_dfr(rare, function(x){
  z <- data.frame(x)
  return(z)
}, .id = "sample")

# plotting simple
ggplot(data = rare)+
  geom_line(aes(x = raw.read, y = OTU, color = sample))+
  scale_x_continuous(labels =  scales::scientific_format())

# suppose more grouping variables were available
groupings <- data.frame(sample = c("B", "C", "D"),
                        location = c("one", "one", "two"), stringsAsFactors = F)

# join the grouping variable with the dataframe containing fit values
rare <- map_dfr(groupings$sample, function(x){ #loop over samples
  z <- rare[rare$sample == x,] #subset rare according to sample 
  loc <- groupings$location[groupings$sample == x] #subset groupings according to sample, if more than one grouping repeat for all
  z <- data.frame(z, loc) #make a new data frame with the subsets
  return(z)
})

# plotting with groups
ggplot(data = rare)+
  geom_line(aes(x = raw.read, y = OTU, group = sample, color = loc))+
  geom_text(data = rare %>% #here we need coordinates of the labels
              group_by(sample) %>% #first group by samples
              summarise(max_OTU = max(OTU), #find max OTU
                        max_raw = max(raw.read)), #find max raw read
            aes(x = max_raw, y = max_OTU, label = sample), check_overlap = T, hjust = 0)+
  scale_x_continuous(labels =  scales::scientific_format())+
  theme_bw()

## try out with some other data
# data available in sheets
# important note: here location name/sheets_name variable is equivalent to sampling units
sheets_name <- c("pragatinagar1",
                 "geetanagar",
                 "pragatinagar2",
                 "bach")
# assemble a composite dataframe
insect_study <- sheets_name %>% 
  map_dfr(~readxl::read_xlsx("./rarefaction curve1.xlsx", .x, skip = 1), .id = "location")

# recode factors to that were inaccurately named during mapping
insect_study <- insect_study %>% 
  mutate(location = fct_recode(location, "pragatinagar1" = "1", "geetanagar" = "2", 
                               "pragatinagar2" = "3", "bach" = "4"))

# # insect taxa naming standardization
# insect_study$Insect_taxa %>% 
#   unique()

# remove if any taxa is unnamed (is NA)
insect_study <- na.omit(insect_study)

# now spread the data
insect_study <- insect_study %>% 
  spread(key = Insect_taxa, value = Insect_count, fill = 0) %>% 
  column_to_rownames(var = "location")

# fit a rarefaction curve to data
out <- rarecurve(insect_study, step = 5, sample = 6000, label = T) #low step size because of low abundance

# clean the fit values to get a list of dataframes
rare <- lapply(out, function(x){
  b <- as.data.frame(x)
  b <- data.frame(OTU = b[,1], step_raw = rownames(b))
  b$step_raw <- as.numeric(gsub("N", "",  b$step_raw))
  return(b)
})

# rename the list
names(rare) <- sheets_name

# convert the list of dataframes to a singe dataframe by binding together
rare <- map_dfr(rare, function(x){
  z <- data.frame(x)
  return(z)
}, .id = "location")

### Diversity indices
## Shannon or Shannon-Weaver (or Shannon-Wiener) index
# H = -sum(p_i)log(b)p_i; p_i is the proportional abundance of species i 
# and b is the base of logarithm. It is most popular to use natural log, but 
# no real difference is due to b = 2
H <- diversity(insect_study)

## Simpson's index
# simp = 1 - sum(p_i)^2; where last term is also known as "D"
simp <- diversity(insect_study, "simpson")

## Inverse simpson's index
# invsimp = 1/sum(p_i)^2; where denominator term is also known as "D"
invsimp <- diversity(insect_study, "inv")

## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy
unbias.simp <- rarefy(insect_study, 2) - 1

## Fisher alpha
alpha <- fisher.alpha(insect_study)

## Plot all
pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")

## Species richness (S) and Pielou's evenness (J)
S <- specnumber(insect_study) ## rowSums(insect_study > 0) does the same
J <- H/log(S)

## Beta diversity
# defined as gamma/alpha - 1
# is useful when additional grouping factors are available
# for example let us create an arbitrary temperature related variable
# and integrate it with the given dataset
groupings <- data.frame(location = sheets_name,
                        temperature = c("Hot", "Hot", "Hot", "Cool"), 
                        stringsAsFactors = F)

# join the grouping variable with the dataframe containing fit values
rare <- map_dfr(groupings$location, function(x){ #loop over locations
  z <- rare[rare$location == x,] #subset rare according to location 
  temperature <- groupings$temperature[groupings$location == x] #subset groupings according to location, if more than one grouping repeat for all
  z <- data.frame(z, temperature) #make a new data frame with the subsets
  return(z)
})

alpha <- with(groupings, tapply(specnumber(insect_study), temperature, mean))
gamma <- with(groupings, specnumber(insect_study, temperature))
gamma/alpha - 1

# pretty plotting with/without grouping variables
insect_rarefactiongg <- ggplot(data = rare)+
  geom_line(aes(x = step_raw, y = OTU, group = location
                # , color = temperature
                ))+
  geom_text(data = rare %>% #here we need coordinates of the labels
              group_by(location) %>% #first group by samples
              summarise(max_OTU = max(OTU), #find max OTU
                        max_raw = max(step_raw)), #find max raw read
            aes(x = max_raw, y = max_OTU, label = location), check_overlap = T, hjust = 0)+
  scale_x_continuous(labels =  scales::comma_format(), limits = c(NA, 600))+
  xlab("Number of samples") +
  # ylab("Number of species") +
  ylab("Species richness") +
  theme_bw()
insect_rarefactiongg

ggsave("./rarefaction curve.png", insect_rarefactiongg, 
       width = 8, height = 6, units = "in")


# species accumulation curve
species_counts <- matrix(c(0,8,9,7,0,0,0,8,0,7,8,0,
                           5,0,9,0,5,0,0,0,0,0,0,0, 5,0,9,0,0,0,0,0,0,6,0,0,
                           5,0,9,0,0,0,0,0,0,0,0,0, 5,0,9,0,0,6,6,0,0,0,0,0,
                           5,0,9,0,0,0,6,6,0,0,0,0, 5,0,9,0,0,0,0,0,7,0,0,3,
                           5,0,9,0,0,0,0,0,0,0,1,0, 5,0,9,0,0,0,0,0,0,0,1,0,
                           5,0,9,0,0,0,0,0,0,0,1,6, 5,0,9,0,0,0,5,0,0,0,0,0,
                           5,0,9,0,0,0,0,0,0,0,0,0, 5,1,9,0,0,0,0,0,0,0,0,0),
                         byrow=TRUE,nrow=13)

species_counts %>% head()
species_counts %>% ncol()
species_counts %>% nrow()

set.seed(101)
library(vegan)

# species accumulation
# specaccum indeed accumulates sites -- and there is not much to accumulate in one site.
# so trying specaccum, we need to have multiple site measurements

accum <- specaccum(species_counts, method="random", permutations=100)

# treats row to record sites
# treats column to record to species 
accum$sites
accum$richness # for each site

# Extract the richness and sites components from within the returned 
# object and compute d(richness)/d(sites) 
# (note that the slope vector is one element shorter 
# than the origin site/richness vectors: be careful if you're trying 
# to match up slopes with particular numbers of sites)
                                                                                                            
                                                                                                            