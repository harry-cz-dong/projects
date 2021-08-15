# Setup
library('tidyverse')

# Import and rename columns
df <- read.csv('movies.csv')
names(df)[names(df)=='Revenue..Millions.'] <- 'Revenue'
names(df)[names(df)=='Runtime..Minutes.'] <- 'Runtime'

# Remove NAs
df %>% 
  filter(!is.na(Rating) & !is.na(Metascore)) -> df

# Add new feature: rating gap
df %>% 
  mutate(Gap = Rating - Metascore / 10) -> df

# Extract top and bottom
df %>% 
  arrange(desc(Gap)) %>% 
  slice_head(n=10) %>% 
  mutate(Type='top') -> top_gap

df %>% 
  arrange(Gap) %>% 
  slice_head(n=10) %>% 
  mutate(Type='bottom') -> bottom_gap

# Merge and export
result <- union(top_gap, bottom_gap)
result <- select(result, !Rank)
write.csv(result, 'movies_final.csv')
  