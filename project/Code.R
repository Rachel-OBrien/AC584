install.packages("tidyverse")
library(tidyverse)

#Data
unicef_indicator_1_1_ <- read_csv("unicef_indicator_1 (1).csv")
unicef_metadata_1_ <- read_csv("unicef_metadata (1).csv")

#Data Join
data_join <- full_join(unicef_indicator_1_1_, unicef_metadata_1_)

map_world <- map_data("world")

#Map 1
map_data_join <- full_join(unicef_indicator_1_1_, map_world, by = c("country" = "region"))

ggplot(map_data_join)+
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon()

#Scatter Plot 1
ggplot(unicef_metadata_1_)+
  aes(x = 'GDP per capita', y = 'Life expectancy', color = country)+
  geom_point()

#Time Series 1
ggplot(unicef_metadata_1_)+
  aes(x = year, y = Life expectancy, color = country)+
  geom_line()

unicef_metadata_1_ <- read_csv("unicef_metadata (1).csv")

#Scatter Plot 2
ggplot(unicef_metadata_1_)+
  aes(x = GDPpercapita, y = LifeExp, color = country)+
  geom_point()

data_join_1 <- full_join(unicef_indicator_1_1_,unicef_metadata_1_)

#Scatter Plot 3
ggplot(data_join_1)+
  aes(x = GDPpercapita, y = obs_value, color = country)+
  geom_point()

unicef_metadata_2_ <- read_csv("unicef_metadata (2).csv")

ggplot(unicef_metadata_2_)+
  aes(x = GDPpercapita, y = LifeExp, color = country)+
  geom_point()+
  filter(year = 2015)
data_join_2015 <- unicef_metadata_1_ %>%
  filter(year == 2015)

ggplot(data_join_2015)+
  aes(x = GDPpercapita, y = LifeExp, color = country)+
  geom_point()

#Time Series 1
time_series_plot_1 <- ggplot(unicef_metadata_1_)+
  aes(x = year, y = LifeExp, color = country)+
  geom_line()+
  theme_bw()+
  labs(title = "Life Expectancy by country over time")
time_series_plot_1 + theme(legend.position = "none")

#Scatter Plot 1
scatter_plot_1 <- ggplot(data_join_2015)+
  aes(x = GDPpercapita, y = LifeExp, color = country)+
  geom_point()+
  geom_smooth(method = "lm", color = 'blue')+
  theme_bw()+
  labs(title = "Relationship between Life Expectancy and GDP per Capita in 2015")
scatter_plot_1 + theme(legend.position = "none")

#Scatter Plot 2
scatter_plot_2 <- ggplot(data_join_2015)+
  aes(x = GDPpercapita, y = LifeExp, color = country)+
  geom_point()+
  geom_smooth(method = "loess", color = 'blue')+
  theme_bw()+
  labs(title = "Relationship between Life Expectancy and GDP per Capita 2")
scatter_plot_2 + theme(legend.position = "none")

#Bar Chart
obs_value_total <- unicef_indicator_1_1_ %>%
  filter(sex == "Total")

bar_chart_1 <- ggplot(obs_value_total)+
  aes(x = country, y = obs_value, fill = country)+
  geom_col()
bar_chart_1 + theme(legend.position = "none")


data_right_3_1_ <- read_csv("data_right_3 (1).csv")

data_join_2 <- full_join(unicef_indicator_1_1_, data_right_3_1_)

#Bar Chart 2
obs_value_total_2 <- data_join_2 %>%
  filter(sex == "Total") %>%
  group_by(continent) %>%
  summarise(m_obs_value = mean(obs_value))

bar_chart_2 <- ggplot(obs_value_total_2)+
  aes(x = continent, y = m_obs_value, fill = continent)+
  geom_col()
bar_chart_2 + theme(legend.position = "none")

#Bar chart 3
obs_value_filtered <- obs_value_total_2 %>%
  filter(!is.na(continent))

bar_chart_3 <- ggplot(obs_value_filtered)+
  aes(x = continent, y = m_obs_value, fill = continent)+
  geom_col()+
  labs(title = "Average Percentage of Students Bullied by Continent")
bar_chart_3 + theme(legend.position = "none")

print(bar_chart_3)

#Map 1
map_data_join <- full_join(unicef_indicator_1_1_, map_world, by = c("country" = "region"))

ggplot(map_data_join)+
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon()+
  labs(title = "Percentage of Children Bullied Globally")+
  theme_bw()

#Time Series 1
time_series_plot_1 <- ggplot(unicef_metadata_1_)+
  aes(x = year, y = LifeExp, color = country)+
  geom_line()+
  theme_bw()+
  labs(title = "Life Expectancy by country over time")
time_series_plot_1 + theme(legend.position = "none")


data_join_3 <- full_join(unicef_metadata_1_, data_right_3_1_)

#Time Series 2
continent_filtered <- data_join_3 %>%
  filter(!is.na(continent))

time_series_plot_1 <- ggplot(continent_filtered)+
  aes(x = year, y = LifeExp, group = country, color = continent)+
  geom_line()+
  theme_bw()+
  labs(title = "Life Expectancy by continent over time")
time_series_plot_1 + theme(legend.position = "none")
print(time_series_plot_1)


