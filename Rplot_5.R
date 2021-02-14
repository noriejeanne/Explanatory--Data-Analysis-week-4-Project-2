#5: How have emissions from motor vehicle sources changed from 1999 to 2008 in Baltimore City?
SCC_Vehicles <- SCC %>%
  filter(grepl('[Vv]ehicle', SCC.Level.Two)) %>%
  select(SCC, SCC.Level.Two)

Tot_Emi_24510_V <- NEI %>%
  filter(fips == "24510") %>%
  select(SCC, fips, Emissions, year) %>%
  inner_join(SCC_Vehicles, by = "SCC") %>%
  group_by(year) %>%
  summarise(Total_Emissions = sum(Emissions, na.rm = TRUE)) %>%
  select(Total_Emissions, year)

Baltimore_Vehicles_Plot <- ggplot(Tot_Emi_24510_V, aes(factor(year), Total_Emissions)) +
  geom_bar(stat = "identity", fill = "sienna3", width = 0.5) +
  labs(x = "Year", y = "Emissions (Tons)", title = "Total Motor Vehicle Related Emissions In Baltimore City From 1999 - 2008") +
  theme(plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  ggsave("plot5.png", width = 30, height = 30, units = "cm")

print(Baltimore_Vehicles_Plot)