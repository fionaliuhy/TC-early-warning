#######added analysis during revision round 1

###############residual diagnostics
load("/Volumes/LaCie/02 extreme weather&policy/13 final/23/3/intra/model_2.8.RData")
data$fit <- model$summary.fitted.values$`0.5quant`
##calculate residues
data$residuals <- data$intrac - data$fit

p_rvf <- ggplot(data, aes(x = fit, y = residuals)) +
  geom_point(alpha = 0.3, shape = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "a: Residuals vs. Predicted Values",
    x = "Predicted Values",
    y = "Residuals"
  ) +
  theme_bw(base_family = "Arial") +
  theme(
    text = element_text(family = "Arial", size = 7),
    plot.title = element_text(family = "Arial", size = 7, face = "bold"),
    axis.title = element_text(family = "Arial", size = 7),
    axis.text = element_text(family = "Arial", size = 7),
    legend.text = element_text(family = "Arial", size = 7),
    legend.title = element_text(family = "Arial", size = 7)
  )

city_residuals <- data %>%
  group_by(citycode) %>%
  summarize(mean_residual = mean(residuals, na.rm = TRUE))

# 将残差数据合并到您的地图 shapefile 'map'
# (您的 'map' 已经加载，并且有 'city_code' 列)
map_with_residuals <- merge(map, city_residuals, by.x = "city_code", by.y = "citycode")

# 绘制地图
p_spatial_resid <- ggplot(map_with_residuals) +
  geom_sf(aes(fill = mean_residual), color = "grey", lwd = 0.1) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    name = "Mean Residual"
  ) +
  labs(title = "b: Spatial Distribution of Mean Residuals") +
  theme_void(base_family = "Arial") +
  theme(
    text = element_text(family = "Arial", size = 7),
    plot.title = element_text(family = "Arial", size = 7, face = "bold"),
    legend.text = element_text(family = "Arial", size = 7),
    legend.title = element_text(family = "Arial", size = 7)
  )

###############data comparsion
data0<-read_xlsx("/Volumes/LaCie/02 extreme weather&policy/04 mobility/mobility scale.xlsx",sheet='intra-city mobility')
data_long <- data0 %>%
  pivot_longer(
    cols = -c(city_code, city),     
    names_to = "date",              
    values_to = "intracity"         
  )

data_long <- data_long %>%
  mutate(date = as.Date(date, format = "%Y%m%d"))

data_2023 <- data_long %>%
  filter(format(date, "%Y") == "2023")%>%
  filter(city_code!=81000&city_code!=82000)
summary(data_2023$date)
summary(data_2023$intracity)
sd(data_2023$intracity)

meta<-read.csv("/Volumes/LaCie/meta mobility/2 preprocessed data/distance0_22_23.csv")

meta<-meta%>%
  filter(year(ds)=="2023")
meta<-meta%>%
  filter(year(ds)=="2023")
meta <- meta %>%
  filter(ds < as.Date("2023-11-10"))
summary(meta$distance_category_ping_fraction)
meta$distance_category_ping_fraction<-1-meta$distance_category_ping_fraction

summary(meta$distance_category_ping_fraction[meta$country=='JPN'])
sd(meta$distance_category_ping_fraction[meta$country=='JPN'])
summary(meta$distance_category_ping_fraction[meta$country=='KOR'])
sd(meta$distance_category_ping_fraction[meta$country=='KOR'])
summary(meta$distance_category_ping_fraction[meta$country=='SGP'])
sd(meta$distance_category_ping_fraction[meta$country=='SGP'])
summary(meta$distance_category_ping_fraction[meta$country=='HKG'])
sd(meta$distance_category_ping_fraction[meta$country=='HKG'])
