# Packages
library(rio)
library(ggplot2)  # ggplot() etc.
library(tidyr)  # gather()
library(grid) # final graph
library(ggpubr) # ggarrange

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) ## sets the source file location
# Importing the data ----
dat <- import("data.xlsx") %>%
  gather("var", "value", -depth)
dat2 <- dat[dat$var == "cond", ]
dat1 <- dat[dat$var == "pH" | dat$var == "temp" | dat$var == "ox", ]

## the legend text
expr <- c("ox" = expression(O[2]~(mg~L^-1)), 
          "pH" = expression(pH),
          "temp" = expression(Temp~(degree*C)),
          "cond" = expression(~mu~S~cm^-1),
          "ORP" = expression(ORP),
          "TDS" = expression(TDS))

## we choose colors
colors <- c("ox" = "cyan",
             "pH" = "yellow",
            "temp" = "red",
            "cond" = "green",
            "ORP" = "blueviolet",
            "TDS" = "darkgoldenrod")

## First graph ---
xaxismax <- max(dat$depth, na.rm = TRUE)+0.5
yaxismax <- max(dat$value[dat$var == "ox" | dat$var == "temp"], na.rm = TRUE)+5
if (yaxismax > 13){
  secchix <- 12
} else {
  secchix <- yaxismax/5
}
secchidepth <- 2 ## enter the secchi disk depth
g1 <- ggplot() + 
  geom_line(data = na.omit(dat1), aes(x = depth, y = value, group = var)) +  # lines
  geom_point(data = na.omit(dat1), aes(x = depth, y = value, fill = var), 
             shape = 21, colour = "black") +  # points
  geom_line(aes(x = c(0,secchidepth), y = secchix)) +  # secchi line
  geom_point(aes(x = secchidepth, y = secchix), shape = 10, size = 6) +  # secchi disk
  coord_flip() +  # Reverse x and y axes
  scale_fill_manual(name = "", values = colors[names(colors) %in% dat1$var],
                    labels = expr[names(expr) %in% dat1$var]) +
  scale_x_reverse(name = "Depth (m)", expand = c(0,0), breaks = 0:xaxismax, limits = c(xaxismax,0)) +  
  scale_y_continuous(name = "", position = "right", limits = c(0,yaxismax), expand = c(0,0),
                     breaks = seq(0,yaxismax,2)) +  #  Put y axis on top
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        legend.text.align = 0) ## delete ticks and justify legend's text

if ("cond" %in% dat$var){
## Second graph ---
  yaxismax <- max(dat2$value)*1.1
  g2 <- ggplot() + 
    geom_line(data = na.omit(dat2), aes(x = depth, y = value, group = var), na.rm = TRUE) +  # lines
    geom_point(data = na.omit(dat2), aes(x = depth, y = value, fill = var), 
               shape = 21, colour = "black", na.rm = TRUE) +  # points
    coord_flip() +  # Reverse x and y axes
    scale_fill_manual(name = "", values = colors[names(colors) %in% dat2$var],
                      labels = expr[names(expr) %in% dat2$var]) +
    scale_x_reverse(name = "Depth (m)", expand = c(0,0), breaks = 0:xaxismax, limits = c(xaxismax,0)) +  
    scale_y_continuous(name = "", position = "right", limits = c(min(dat2$value)*0.75,yaxismax), expand = c(0,0)) +  #  Put y axis on top
    theme_bw() +
    theme(axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
          legend.text.align = 0,
          axis.text.y = element_blank(), axis.title.y = element_blank()) 
  
  ## Plot the two graphs
} 
if ("TDS" %in% dat$var & "ORP" %in% dat$var){
  dat3 <- dat[dat$var == "TDS" | dat$var == "ORP", ]
  g3 <- ggplot() + 
    geom_line(data = na.omit(dat3), aes(x = depth, y = value, group = var)) +  # lines
    geom_point(data = na.omit(dat3), aes(x = depth, y = value, fill = var), 
               shape = 21, colour = "black") +  # points
    coord_flip() +  # Reverse x and y axes
    scale_fill_manual(name = "", values = colors[names(colors) %in% dat3$var],
                      labels = expr[names(expr) %in% dat3$var]) +
    scale_x_reverse(name = "Depth (m)", expand = c(0,0), limits = c(xaxismax, 0)) +  
    scale_y_continuous(name = "", position = "right") +  #  Put y axis on top
    theme_bw() +
    theme(axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
          legend.text.align = 0,
          axis.text.y = element_blank(), axis.title.y = element_blank())  ## delete ticks and justify legend's text
} 

## graph!
if(!"cond" %in% dat$var & "TDS" %in% dat$var & "ORP" %in% dat$var){
  gf <- ggarrange(g1, g3, labels = c("A", "B"), widths = c(2,1), legend = "bottom")
  print(gf)
  ggsave("Perfil limnológico.png", gf)
} else if ("cond" %in% dat$var & !"TDS" %in% dat$var & !"ORP" %in% dat$var){
  gf <- ggarrange(g1, g2, labels = c("A", "B"), widths = c(2,1), legend = "bottom")
  print(gf)
  ggsave("Perfil limnológico.png", gf)
} else if (!"cond" %in% dat$var & !"TDS" %in% dat$var & !"ORP" %in% dat$var) {
  ## Plot only one
  ggsave("Perfil limnológico.png", g1)
  print(g1)
} else if ("cond" %in% dat$var & "TDS" %in% dat$var & "ORP" %in% dat$var){
  gf <- ggarrange(g1, g2, g3, labels = c("A", "B", "C"), widths = c(2,1,1), legend = "bottom", ncol = 3)
  print(gf)
  ggsave("Perfil limnológico.png", gf)
}
