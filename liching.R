# load libraries
library(ggplot2)
library(ggpmisc)
library(ggthemes)
library(readxl)
library(scales)

# load data 
lichen_data <- read_excel("data/coenogonium.xlsx")
light_data <- read_excel("data/luxdens.xlsx")

# LUX x DENS ------------------------------------------------------------------
luxdens.reg <- ggplot(light_data, aes(x = open, y = lux)) +
  # add points
  geom_point(size = 1, shape = 1, color = "black") +
  # add regression
  geom_smooth(method="lm", se=FALSE, color="black")+ 
  # add theme
  theme_tufte(base_size=20) + 
  # axis management
  geom_rangeframe() +
  # limit axes
  scale_x_continuous(limits = c(0, 60)) + 
  scale_y_continuous(limits = c(0, 17000), labels = comma) + 
  # add axes lines
  theme(axis.line.x = element_line(color = "black", size = 0.2), 
        axis.line.y = element_line(color = "black", size = 0.2)) +
  # labels
  labs(x = "Percent Canopy Open", y = "Lux") 
ggsave("luxdens_reg_plot.png", luxdens.reg, width = 8, height = 6, dpi = 300)
# AREA x DIAMETER -------------------------------------------------------------
lichen.reg <- ggplot(lichen_data, aes(x = Diameter, y = Area)) +
  # add points
  geom_point(size = 1.5, shape = 1, color = "black") +
  # add regression
  geom_smooth(method="lm", se=FALSE, color="black")+ 
  # add theme
  theme_tufte(base_size=20) + 
  # axis management
  geom_rangeframe() +
  # limit axes
  scale_x_continuous(limits = c(0, 30)) + 
  scale_y_continuous(limits = c(0, 8)) + 
  # add axes lines
  theme(axis.line.x = element_line(color = "black", size = 0.2), 
        axis.line.y = element_line(color = "black", size = 0.2)) +
  # axis labels
  labs(x = "Substrate Diameter", y = "Thallus Surface Area") 

# AREA x DENS -----------------------------------------------------------------
dens.reg <- ggplot(lichen_data, aes(x = open, y = Area)) + 
  # add points
  geom_point(size = 1.5, shape = 1, color = "black") +
  # add regression
  geom_smooth(method="lm", se=FALSE, color="black")+ 
  # add theme
  theme_tufte(base_size=20) + 
  # axis management
  geom_rangeframe() +
  # limit axes
  scale_x_continuous(limits = c(0, 13)) + 
  scale_y_continuous(limits = c(0, 8)) + 
  # add axes lines
  theme(axis.line.x = element_line(color = "black", size = 0.2), 
        axis.line.y = element_line(color = "black", size = 0.2)) +
  labs(x = "Percent Canopy Open", y = "Thallus Surface Area") 

# AREA x LUX ------------------------------------------------------------------
lux.reg <- ggplot(lichen_data, aes(x = lux, y = Area)) +
  # add points
  geom_point(size = 1.5, shape = 1, color = "black") +
  # add regression
  geom_smooth(method="lm", se=FALSE, color="black")+ 
  # add theme
  theme_tufte(base_size=20) + 
  # axis management
  geom_rangeframe() +
  # limit axes
  scale_x_continuous(limits = c(0, 4100)) + 
  scale_y_continuous(limits = c(0, 8)) + 
  # add axes lines
  theme(axis.line.x = element_line(color = "black", size = 0.2), 
        axis.line.y = element_line(color = "black", size = 0.2)) +
  # axis labels
  labs(x = "Lux", y = "Thallus Surface Area") 




# visualize
print(luxdens.reg)
# visualize
print(lichen.reg)
# visualize
print(dens.reg)
# visualize
print(lux.reg)
```

