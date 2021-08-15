# Setup
library('tidyverse')
library('ggalluvial')
library('Cairo')

# Import
gold <- read.csv('gold.csv', header=F)
silver <- read.csv('silver.csv', header=F)
bronze <- read.csv('bronze.csv', header=F)

# Add medal type column
prep <- function(old_df) {
  name <- deparse(substitute(old_df))
  new_df <- mutate(old_df, medal=name)
  return(new_df)
}
gold <- prep(gold)
silver <- prep(silver)
bronze <- prep(bronze)

# Merge
gold %>% 
  union(silver) %>% 
  union(bronze) -> medals
names(medals) = c('name', 'sport', 'event', 'medals')

# Clean up errors
medals %>% 
  mutate(sport=str_replace(sport, ' ', '')) %>% 
  mutate(event=str_replace(event, ' ', '')) -> medals
medals$event[medals$sport == 'men\'s 4x200m freestyle relay'] = 
  'men\'s 4x200m freestyle relay'
medals$sport[medals$sport == 'men\'s 4x200m freestyle relay'] = 'swimming'

# Add feature: sport type
get_type <- function(sport) {
  sport_pattern = '(swimming|rowing|canoe|sailing|surfing|diving)'
  if (str_detect(sport, sport_pattern)) {
    type = 'water'
  } else {
    type = 'land'
  }
  return(type)
}
get_type <- Vectorize(get_type)
medals %>% 
  mutate(type=get_type(sport)) -> medals

# Aggregate by {sport, type, medal} and find count
medals %>% 
  group_by(sport, type, medals) %>% 
  summarise(count=n()) -> tally

# Reorder factor levels for plot
tally$type = factor(tally$type, levels=c('water', 'land'))
tally$medals = factor(tally$medals, levels=c('gold', 'silver', 'bronze'))
tally %>% 
  ungroup() %>% 
  arrange(type, medals, desc(count)) %>% 
  select(sport) %>% 
  unique() -> sport_names
tally$sport = factor(tally$sport, levels=pull(sport_names))

# Build Sankey chart
plot <- ggplot(tally, aes(y=count,
           axis1=sport, axis2=type, axis3=medals)) +
          geom_alluvium(aes(fill=medals), width=1/12) +
          geom_stratum(width=1/8, fill='grey', color='white') +
          geom_text(stat='stratum', aes(label=after_stat(stratum)), size=1.5) +
          scale_x_continuous(breaks=1:3, labels=c("Sport", "Type", "Medal")) +
          scale_fill_manual(values=c('#FFD700', '#C0C0C0', '#CD7f32')) +
          theme(text=element_text(size=10), legend.position="none")  
ggsave('alluvial2.png', height=4, width=6, type='cairo', dpi=400, limitsize=FALSE)
print(plot)
dev.off()



