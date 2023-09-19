#+ message=F
library(tidyverse)
library(readxl)
library(countrycode)
library(archive)
library(ggmosaic)

# Lynn-Becker National IQ data V1.3.4, July 2023
url <- "https://viewoniq.org/wp-content/uploads/2023/07/NIQ-DATASET-V1.3.4.zip"
directory <- "NIQ-DATASET-V1.3.4"

# Uncomment this line to download data
# archive_extract(url, directory)

fn <- file.path(directory, "NIQ-DATA (V1.3.4).xlsx")

REC <- read_excel(fn, sheet = 4, skip = 1, n_max = 683) # Source data
SEL <- read_excel(fn, sheet = 5, skip = 1, n_max = 683) # Combined with other indicators
suppressMessages(
  NAT <- read_excel(fn, sheet = 7, skip = 1, n_max = 203) # Nation-level IQ and other vars
)

# Assign World Bank regions
REC$region <- countrycode(REC$`ISO 3166-1 ALPHA-3`, origin = 'iso3c', destination = 'region')

# ANT, "Netherlands Antilles", not matched by countrycode function
# Assign to Latin America & Caribbean?
REC$region[REC$`ISO 3166-1 ALPHA-3` == 'ANT'] <- 'Latin America & Caribbean'

# Typo in country code
NAT$`ISO 3166-1 ALPHA-3`[NAT$`ISO 3166-1 ALPHA-3` == 'KNA.'] <- 'KNA'

# Assign World Bank regions
NAT$region <- countrycode(NAT$`ISO 3166-1 ALPHA-3`, origin = 'iso3c', destination = 'region')

# ANT, "Netherlands Antilles", not matched by countrycode function
# Assign to Latin America & Caribbean?
NAT$region[NAT$`ISO 3166-1 ALPHA-3` == 'ANT'] <- 'Latin America & Caribbean'

# Saint Helena, Ascension, and Tristan da Cunha: 
# British territory in the South Atlantic
# Tiny population (~5600)
# https://en.wikipedia.org/wiki/Saint_Helena,_Ascension_and_Tristan_da_Cunha
# Not assigning to a region for now.

# The number of records per region
table(REC$region)

# Types of cognitive tests in each study
#+ fig.height=8
dotchart(sort(c(table(REC$`Test (meas.)`))))

# Study years
hist(REC$`Year (meas.)`, main = 'Histogram of study years', xlab = 'Study year')

# Plot Age means and ranges for each data source
#+ warning=F, fig.width=10, fig.height=10
REC %>% 
  group_by(region) %>% 
  arrange(`Mean age`, .by_group = T) %>% 
  mutate(row = 1:n()) %>% 
  ungroup() %>% 
  ggplot(aes(xmin = `Lowest age`, x = `Mean age`, xmax = `Highest age`, y = row)) +
  geom_linerange() + 
  geom_point(colour = 'red') +
  scale_x_continuous(breaks = seq(0, 90, 20)) +
  labs(title = 'Age range and mean age of each study', y = '', x = '\nAge (years)') +
  theme_minimal(15) + facet_wrap(~region)

# Sample sizes of each study
summary(REC$`N (ind.)`)

# Origin coding from included Manual
# - Var: [Origin (type)]
# - Type of the geographic origin of individuals of a sample
# - '1'	= national	= from all over the country
# - '2'	= regional	= from one or a small number of regions
# - '3'	= urban		= from one or more urban areas, specific name in parentheses if available
# - '4'	= rural		= from one or more rural areas, specific name in parentheses if available
# - '5'	= foreign	= from a national group within a foreign country

# Recode `Origin (type)` per above:

origin_dict <- c(
  '1' = 'national',
  '2' = 'regional',
  '3' = 'urban',
  '4' = 'rural',
  '5' = 'foreign',
  '6' = '?'
)

REC$origin <- origin_dict[REC$`Origin (type)`]

ggplot(REC) + 
  geom_mosaic(aes(x=product(origin, region), fill = origin)) +
  labs(x = '', y = '') +
  theme_minimal(15) +
  guides(fill = 'none') +
  ggtitle('Data sources by region') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# From included Manual
# - Var: [Sample char.]
# - Type of data collection (refers to the information in both previous columns)
# - '1'	= represent	= representative for the population the sample describes
# - '2'	= normative	= sample for IQ-test standardization
# - '3'	= random	= base value; no factors of selection stated
# - '4'	= control	= a sample, representative for a non-treated sub-population
# - '5'	= selective	= selection influenced by further factors not given in table

# Recoding dict from above
sample_dict <- c(
  '1' = 'representative',
  '2' = 'normative',
  '3' = 'random',
  '4' = 'control',
  '5' = 'selective'
)

REC$sample_type <- sample_dict[REC$`Sample char.`]

ggplot(REC) + 
  geom_mosaic(aes(x=product(sample_type, region), fill = sample_type)) +
  labs(x = '', y = '') +
  theme_minimal(15) +
  guides(fill = 'none') +
  ggtitle('Data sources by type of sample') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

subsaharan <- REC$region == 'Sub-Saharan Africa'
table(REC$`Country name`[subsaharan], REC$sample_type[subsaharan] == 'representative')

# Coding error: British sample GBR1967 is labeled as South Africa
REC$Full[REC$`Country name` == 'South Africa' & REC$sample_type == 'representative']
