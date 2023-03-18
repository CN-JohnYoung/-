library(ggplot2)
rawdata = read.table(file = "Data20_6.CSV", 
                     header = T, 
                     stringsAsFactors = F, 
                     sep = ",")

Df = rawdata[, c(1,2,3,4)]
Df$Table = as.factor(Df$Table)

for(t in Df$Table) {
  Temp = Df[Df$Table == t, ]
  Temp$X = Temp$X - Temp$X[1]
  Temp$Y = Temp$Y - Temp$Y[1]
  Df[Df$Table == t, ] = Temp
}

limits = ceiling(max(abs(c(max(Df$X), max(Df$Y), min(Df$X), min(Df$Y))))/50)*50

ggplot(Df, aes(x = X, y = Y, group = Table, color = Table)) + 
  geom_path(linewidth = .9) +
  coord_fixed() +
  theme(legend.position = "none") +
  labs(x = "Distance from origin (μm)", 
       y = "Distance from origin (μm)", 
       title = "Representative trajectories",
       subtitle = "0.5-6") +
  xlim(c(-limits, limits)) + ylim(c(-limits, limits)) +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#000000"),
        panel.grid = element_line(color = "#808080", linetype = "dashed"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14))
  
  
