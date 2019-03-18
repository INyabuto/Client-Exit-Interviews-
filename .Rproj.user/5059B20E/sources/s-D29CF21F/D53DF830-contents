#'
#' 
#' 

pacman::p_load('dplyr','tidyr','gapminder','ggplot2','ggalt','forcats','R.utils','png','ggpubr','scales','bbplot')

cei_avg <- cei_events %>% 
  group_by(country) %>%
  summarise(avg_cost = mean(as.numeric(cost_of_service), na.rm = T))


#' multiple bar graph


avg_cos_bars <- ggplot(cei_avg, aes(x = country, y = avg_cost)) +
  geom_bar(stat = "identity",
           position = "identity",
           fill = "#1380A1") + 
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  bbc_style() + 
  labs(title = "The average cost of family planning among the clients interviewed in 2017 was $10.62.
Clients in El Salvador paid USD 19 more than clients in Nepal. ",
       subtitle = "Average cost of family planning service by country") +
  geom_hline(yintercept = mean(cei_avg$avg_cost))
avg_cos_bars

finalise_plot(avg_cos_bars,source_name = "Source: WHP Client Exit Interviews 2017", save_filepath = paste0(getwd(),"/Highest AVG COS by country.png"))


#' Cost of service per method 
#' 
avg_cost_per_method <- cei_events %>%
  gather(method,value,c(46:54,56:57), -country,-cost_of_service) %>%
  filter(value == "true")

methods_map <- data.frame(name = names(cei_events[c(46:54,56:57)]),
                          code = c("Counseling only","Emergency contraception","Female condom","Implant insertion",
                                   "Implant removal only","Injectable","IUD insertion","IUD removal only","Male condom","Other","Pill"),
                          stringsAsFactors = F)
# remap the methods
avg_cost_per_method$method <- plyr::mapvalues(avg_cost_per_method$method,from = methods_map$name, to = methods_map$code, warn_missing = F)
avg_cost <- avg_cost_per_method %>%
  group_by(country,method) %>%
  summarise(avg_cost=mean(as.numeric(cost_of_service), na.rm = T))

avg_method_bars <- ggplot(avg_cost, aes(x=method,y=avg_cost, fill = method)) + 
  geom_bar(stat = "identity",
           position = "identity") +
  facet_wrap(~country, scales = "free") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") + 
  bbc_style() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(direction = -1)


#'feeling about cost of service 
cei_events$feeling_about_cost <- factor(cei_events$CEI.SRH.FP.TRK...How.did.you.feel.about.the.cost.of.your.services.,levels = unique(cei_events$CEI.SRH.FP.TRK...How.did.you.feel.about.the.cost.of.your.services.))
roi_country <- cei_events %>%
  group_by(country, feeling_about_cost) %>%
  summarise(total_cost = sum(as.numeric(cost_of_service), na.rm = T))

roi_bar <- ggplot(roi_country, aes(x=country, y= total_cost, fill = feeling_about_cost)) +
  geom_bar(stat = "identity",
           position = "fill") + 
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  bbc_style() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(direction = -1) +
  labs(title = "How feeling about cost of service varies",
       subtitle = "% of cost of service by feeling about the cost of service, 2017") +
  theme(legend.position = "top",
        legend.justification  = "left") 

finalise_plot(roi_bar,source_name = "Source: WHP Client Exit Interviews 2017", save_filepath = paste0(getwd(),"/feeling by cost of service.png"))



#'
#'Feeling / percepttion 
#'dumbel
#'pleased/neutral/displeased

#prepare data
cei_feelings <- cei_events %>%
  gather(Feeling,value, c(96:107,204)) 

rank_map <- data.frame(name = c("5 - Very Pleased", "3 - Neutral","4 - Pleased","1 - Very Displeased","2 - Displeased"),
                       code = c(5,3,4,1,2))
# remap 
cei_feelings$rank <- plyr::mapvalues(cei_feelings$value, from = rank_map$name, to = rank_map$code, warn_missing = F)
cei_feelings <- cei_feelings %>%
  select(country,Feeling,value,rank)

feeling_map <- data.frame(name = unique(cei_feelings$Feeling),
                          code = c("Caring and Concern of the provider",
                                   "Caring and concern of staff other than the provider",
                                   "Comfort of the waiting area",
                                   "Cost of service",
                                   "Friendliness of the provider",
                                   "Friendliness of the staff other than the provider",
                                   "Length of time provided to ask questions or clarify doubts",
                                   "Privacy of body during service",
                                   "Privacy of conversation during service",
                                   "Privacy of records were given",
                                   "Respect provided by the staff other than tthe provider",
                                   "Length of time spent in the facility",
                                   "Treatment as unmarried person"), 
                          stringsAsFactors = F)

cei_feelings$Feeling <- plyr::mapvalues(cei_feelings$Feeling, from = feeling_map$name, to = feeling_map$code, warn_missing = F)
  
cei_feelings$Feeling <- as.factor(cei_feelings$Feeling)
cei_feelings$value <- as.factor(cei_feelings$value)
cei_feelings$rank <- as.numeric(cei_feelings$rank)
cei_feelings <- cei_feelings[order(cei_feelings$rank),]
cei_feelings$Feeling <- factor(cei_feelings$Feeling, levels = levels(cei_feelings$Feeling))
cei_feelings$value <- factor(cei_feelings$value, levels = levels(cei_feelings$value))

cei_ordered_bars <- ggplot(cei_feelings, aes(x = Feeling, y = rank, label = rank)) +
  geom_bar(stat = "identity", position = "fill",
           aes(fill = value), width = .5) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") + 
  scale_y_continuous(labels = scales::percent) + 
  bbc_style() +
  scale_fill_viridis_d(direction = 1) +
  labs(title = "Clients are pleased with services provided, all countries",
       subtitle = "Feeling about the services provided")+
  theme(legend.position = "top",
        legend.justification = "left")+
  guides(fill = guide_legend(reverse = F)) + 
  coord_flip() 

finalise_plot(cei_ordered_bars, source_name = "Source: WHP Client Exit Interviews 2017", save_filepath = paste0(getwd(),"/clients_perception.png"))



  

