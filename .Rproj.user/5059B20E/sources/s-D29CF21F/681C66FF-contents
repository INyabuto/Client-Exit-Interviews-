#'
#'
#'Data Visualization
#'
#'Using bbs_style() and ggoplot 2
#'
#'

pacman::p_load('dplyr','tidyr','gapminder','ggplot2','ggalt','forcats','R.utils','png','ggpubr','scales')

# install bbplot 
devtools::install_github("bbc/bbplot")

line_df <- gapminder %>% filter(country == "Malawi")

# make a plot
#' a line graph of life expectancy in Malawi 
line <- ggplot(line_df,aes(x=year,y=lifeExp)) + 
  geom_line(colour = "#1380A1", size = 1) + 
  geom_hline(yintercept = 0, size = 1, colour = "#333333") + 
  bbc_style() + 
  labs(title = "Living Longer",
       subtitle = "Life expectancy in Malawi 1952-2007")
line

finalise_plot(line,source_name = "Source: Gapminder", save_filepath = paste0(getwd(),"/Malawi lifeExp.png"))

# LifeExp for china
line_df <- gapminder %>% filter(country == "China")
line <- ggplot(line_df, aes(x=year, y = lifeExp)) + 
  geom_line(colour = "#1380A1", size = 1) + 
  geom_hline(yintercept = 0, size = 1, colour = "#333333") + 
  bbc_style() +
  labs(title = "Living longer",
       subtitle = "Life expectancy in Chana 1952 - 2007")

line
#save
finalise_plot(line,source_name = "Source: Gapminder", save_filepath = paste0(getwd(),"/China lifeExp.png"))
#' 
#' multiple line plot 

multiple_line_df <- gapminder %>% filter(country == "China" | country == "United States")

multiple_line <- ggplot(multiple_line_df, aes(x=year,y=lifeExp,colour = country)) + 
  geom_line(size=1) + 
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_color_manual(values = c("#FAAB18","#1380A1")) + 
  bbc_style() + 
  labs(title = "Living longer",
       subtitle = "Life expectancy in China and United States 1952 - 2007")

multiple_line
#save
finalise_plot(multiple_line,source_name = "Source: Gapminder", save_filepath = paste0(getwd(),"/China and US lifeExp.png"))

#'
#'Make a barchart
#'

bar_df <- gapminder %>% filter(year == 2007 & continent == "Africa") %>%
  arrange(desc(lifeExp)) %>%
  head(5)

bars <- ggplot(bar_df, aes(x=country,y=lifeExp)) + 
  geom_bar(stat = "identity",
           position = "identity",
           fill = "#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  bbc_style() + 
  labs(title = "Reunion is highest",
       subtitle = "Highest African life expectancy, 2007")

bars
finalise_plot(bars,source_name = "Source: Gapminder", save_filepath = paste0(getwd(),"/Highest African lifeExp.png"))

#'
#'Make a stacked barchart
#'
# prepare data
stacked_df <- gapminder %>% 
  filter(year == 2007) %>%
  mutate(lifeExpGrouped = cut(lifeExp, 
                              breaks = c(0,50,65,80,90),
                              labels = c("under 50", "50 - 65", "65 - 80", "80+"))) %>%
  group_by(continent,lifeExpGrouped) %>%
  summarise(continentPop = sum(as.numeric(pop)))

# set order of stacks by changing the facrtor levels
stacked_df$lifeExpGrouped <- factor(stacked_df$lifeExpGrouped, levels = rev(levels(stacked_df$lifeExpGrouped)))

# create the plot
stacked_bars <- ggplot(stacked_df,aes(x = continent, y = continentPop, fill = lifeExpGrouped)) + 
  geom_bar(stat = "identity",
           position = "fill") + 
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  bbc_style() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_viridis_d(direction = -1) + 
  labs(title = "How life expectancy varies",
       subtitle = "% of population by life expectancy band, 2007") +
  theme(legend.position = "top",
        legend.justification = "left") + 
  guides(fill = guide_legend(reverse = T))

stacked_bars
#save
finalise_plot(stacked_bars,source_name = "Source: Gapminder", save_filepath = paste0(getwd(),"/stacked continent lifeExp.png"))


#' 
#' Mae a grouped barchart
#' you just need to change position to dodge
#' and set fill aesthetically 

grouped_bar_df <- gapminder %>%
  filter(year == 1967 | year == 2007) %>%
  select(country, year, lifeExp) %>%
  spread(year,lifeExp) %>%
  mutate(gap = `2007` - `1967`) %>%
  arrange(desc(gap)) %>%
  head(5) %>%
  gather(key = year,
         value = lifeExp, 
         -country, 
         -gap)

  
#' make the plot
grouped_bars <- ggplot(grouped_bar_df, 
                       aes(x=country,
                           y=lifeExp,
                           fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  bbc_style() +
  scale_fill_manual(values = c("#1380A1", "#FAAB18")) +
  labs(title = "We're living longer",
       subtitle = "Biggest life expectancy rise, 1967 - 2007") +
  theme(legend.position = "top",
        legend.justification = "left")
grouped_bars
# save
finalise_plot(grouped_bars,source_name = "Source: Gapminder", save_filepath = paste0(getwd(),"/grouped bars countries lifeExp.png"))


#'
#'Make a dumbbell chart
#'
#'prepare data
dumbbel_df <- gapminder %>%
  filter(year == 1967 | year == 2007) %>%
  select(country, year, lifeExp) %>%
  spread(year,lifeExp) %>%
  mutate(gap = `2007` - `1967`) %>%
  arrange(desc(gap)) %>%
  head(10)

#plot
dumbbel <- ggplot(dumbbel_df, aes(x = `1967`, xend = `2007`, y = reorder(country,gap), group = country)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") + 
  bbc_style() +
  labs(title = "We are living longer",
       subtitle = "Biggest life expectancy rise, 1967 - 2007") 

dumbbel
finalise_plot(dumbbel,source_name = "Source: Gapminder", save_filepath = paste0(getwd(),"/dumbel countries lifeExp.png"))




