# Set a new variable indicating hybrid zone or outside
alldat.orn$loc <- ifelse(alldat.orn$cat == "Bull_ref", "Outside", "Hybrid zone") 

# Separate Bullock's
bullocks <- alldat.orn[which(alldat.orn$sp == "Bull"),]

# extract values post-1950
bullocks2 <- bullocks[which(bullocks$date > "1950-01-01"),] # 80 -> 65 obs


#Linear models

summary(lm(B1 ~ date*loc, data=bullocks2))

summary(lm(S9 ~ date*loc, data=bullocks2))

summary(lm(H3 ~ date*loc, data=bullocks2))


#### Plots

#1. Brightness
# Regression over time 
h3a <- ggplot(bullocks, aes(x = date, y = B1, shape = loc, color = loc)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, aes(fill = loc), se = T) +
  theme_classic() +
  mytheme +
  #  stat_fit_glance(method = "lm", 
  #                        method.args = list(formula = y ~ x), size = 5, 
  #                        aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.3g',
  #                                            stat(..r.squared..), stat(..p.value..))), 
  #  parse = TRUE) + 
  labs(y = "Total brightness (B1)", x= "Collection date", color = "Location", 
       fill = "Location", shape = "Location") +
  scale_color_manual(values = pal3) +
  scale_fill_manual(values = pal3)



#2. Chroma

h3b <- ggplot(bullocks, aes(x = date, y = S9, shape = loc, color = loc)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, aes(fill = loc), se = T) +
  theme_classic() +
  mytheme +
  #  stat_fit_glance(method = "lm",
  #                        method.args = list(formula = y ~ x), size = 5, 
  #                        aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.3g',
  #                                            stat(..r.squared..), stat(..p.value..))), 
  #                  parse = TRUE) + 
  labs(y = "Carotenoid chroma (S9)", x = "Collection date", color = "Location", 
       fill = "Location", shape = "Location") +
  scale_color_manual(values = pal3) +
  scale_fill_manual(values = pal3)



#3. Hue

h3c <- ggplot(bullocks, aes(x = date, y = H3, shape = loc, color = loc)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, aes(fill = loc), se = T) +
  theme_classic() +
  mytheme +
  #  stat_fit_glance(method = "lm", 
  #                        method.args = list(formula = y ~ x), size = 5, 
  #                        aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.3g',
  #                                            stat(..r.squared..), stat(..p.value..))), 
  #                        parse = TRUE) + 
  labs(y = "Hue (H3)", x = "Collection date", color = "Location", 
       fill = "Location", shape = "Location") +
  scale_color_manual(values = pal3) +
  scale_fill_manual(values = pal3)



#Merge 

pg <- plot_grid(
  h3a + theme(legend.position="none"),
  h3b + theme(legend.position="none"),
  h3c + theme(legend.position="none"),
  align = 'vh',
  labels = c("A", "B", "C"),
  hjust = -1,
  nrow = 1, label_size = 16
)

leg <- get_legend(h3a + theme(legend.box.margin = margin(0,0,0,12)))

plot_grid(pg, leg, rel_widths = c(6, 0.8))

ggsave("Figure5.png", plot=last_plot(), width=600, height=225, units="mm", dpi=600, scale=T)
