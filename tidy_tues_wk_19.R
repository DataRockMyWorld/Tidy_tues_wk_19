
## Setting Up the Environment

library(tidyverse)
library(skimr)
library(tidytuesdayR)
library(lubridate)
library(showtext)

font_add_google("El Messiri","El Messiri")
font_add_google("Marck Script","Marck Script")
font_add_google("Merienda One","Merienda One")
font_add_google("Playfair Display","Playfair Display")
font_add_google("Bangers","Bangers")

showtext_auto()

## Importing Data----

#titles
nyt_titles <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')

# dataframe titles
nyt_tt <- nyt_titles %>% separate( "id\ttitle\tauthor\tyear\ttotal_weeks\tfirst_week\tdebut_rank\tbest_rank", 
                         into = c("id","title","author","year","total_weeks","first_week","debut_rank","best_rank"), sep ="\t")

# Full
nyt_full <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

skim(nyt_full)

#dataframe nyt_full
nyt_ful <- nyt_full %>% separate("year\tweek\trank\ttitle_id\ttitle\tauthor", 
                                   into = c("year","week","rank","title_id","title","author"), sep ="\t")

#Exploration

nyt_ful %>% distinct(title_id) #unique books published

skim(nyt_tt)

nyt_tt %>% mdy("first_week")

#Converting variables to numeric
nyt_tt$first_week <- ymd(nyt_tt$first_week)
nyt_tt$debut_rank <- as.numeric(nyt_tt$debut_rank)
nyt_tt$total_weeks <- as.numeric(nyt_tt$total_weeks)


#Introduce a month column
nyt_tt <- nyt_tt %>% mutate(month = month(first_week, label = TRUE)) %>% 
  relocate(month, .before = debut_rank)

#Books that debuted at No.1
nyt_rnk1 <- nyt_tt %>% filter(debut_rank == 1 & year >= 1990) %>% group_by(month) %>% 
  tally(debut_rank)

#Plot of books that debuted at No.1
nyt_books <- nyt_rnk1 %>% 
  ggplot(aes(month,n)) +
  geom_col(aes(fill = month),show.legend = FALSE) +
  coord_polar() +
  labs(title = "October & November Have The Most Books Debuting at No.1",
       subtitle = "Number of Books debuting at No.1(1990 - 2020)",
       caption = "Tidy_tuesday | Plot by balke analytics") +
  scale_fill_brewer(type = "qual",palette = "Paired")+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold",family = "Bangers", color = "black"),
    plot.subtitle = element_text(size = 17, face = "italic", colour = "black", family = "El Messiri",lineheight = .5),
    plot.title.position = "plot",
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(face = "bold", family = "Playfair Display"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold", family = "El Messiri", size = 10),
    panel.grid.minor =element_blank()
  )


ggsave("nyt_books.png")
  
