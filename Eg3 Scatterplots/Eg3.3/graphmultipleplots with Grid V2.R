library(ggplot2) # ggplot
library(rio) # import
# library(ggpubr) # ggarrange

## not used in this case, but can be useful to give prespective
getfirstplot <- function(data,var){
  if (var == "Clorofila"){
    ylabel <- expression(paste("Clorofila", ~mu ~mol ~m^-2))
  } else {
    ylabel <- paste(var,"(mm)")
  }
  data$Cultivar <- factor(data$Cultivar, levels=c("Merlot","Chardonnay","Cabernet sauvignon"))
  
  a <- ggplot(data) + aes(x = Cultivar, y = 
                            if (var == "Clorofila"){
                              VALORES
                            } else if ( var == "Diametro de Tallo"){
                              `tallo (mm)`
                            } else if ( var == "Diametro de Entrenudo"){
                              `entrenudo (mm)`
                            }) +
    geom_boxplot() +
    theme(axis.text.x = element_text(color = "grey20", size = 9)) +
    stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
    scale_x_discrete(guide = guide_axis(angle = 35)) +
    theme_light() +
    xlab("") +
    ylab(ylabel)
  a
}

toplot <- function(data,var){
  
  ## define ylab() from the ggplot
  if (var == "Clorofila"){
    ylabel <- expression(paste("Clorofila", ~mu ~mol ~m^-2))
  } else {
    ylabel <- paste(var,"(mm)")
  }
  
    df1 <- data
    ## multiple the df by 3 
    df1a <- df1
    df1b <- df1
    df1c <- df1
    df1d <- df1
    ## this will be x axis
    df1a$Tratamiento <- df1$Patogeno
    df1b$Tratamiento <- df1$Micorriza
    df1c$Tratamiento <- df1$Trichoderma
    df1d$Tratamiento <- df1$Cultivar
    ## this will be our facet
    df1a$type <- "Patógeno"
    df1b$type <- "Micorriza"
    df1c$type <- "Trichoderma"
    ## this is weird to incorporate, but it was asked by the client
    df1d$type <- "Variedad"
    
    ## desired order for Trichoderma facet
    df1a$Tratamiento <- factor(df1a$Tratamiento, levels=unique(df1a$Tratamiento))
    df1b$Tratamiento <- factor(df1b$Tratamiento, levels=unique(df1b$Tratamiento))
    df1c$Tratamiento <- factor(df1c$Tratamiento, levels=unique(df1c$Tratamiento))
    df1d$Tratamiento <- factor(df1d$Tratamiento, levels=c("Merlot","Chardonnay","Cabernet sauvignon"))
    
    df1f <- rbind(df1d,df1a,df1b,df1c)
    
    ## order the facet
    df1f$type<- factor(df1f$type, levels=c("Variedad", "Micorriza", "Patógeno","Trichoderma"))
    
    a <- ggplot(df1f) + aes(x = Tratamiento, 
                            y =  
                              if (var == "Clorofila"){
                                VALORES
                              } else if ( var == "Diametro de Tallo"){
                                `tallo (mm)`
                              } else if ( var == "Entrenudo"){
                                `entrenudo (mm)`
                              },
                            color = type) +
      geom_boxplot() +
      scale_x_discrete(guide = guide_axis(angle = 35)) +
      facet_grid(cols = vars(type), scales = "free_x") +
      theme(axis.text.x = element_text(color = "grey20", size = 9)) +
      guides(color = "none") +
      theme_light() +
      ylab(ylabel) +
      stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
      xlab("") +
      ggtitle(paste("Gráfica de efectos principales para",var))
    

    ggsave(paste(var,".png"), a, width = 8, height = 6)
}

## data was invented, original data is private, yeah I know.
df <- import("data.xlsx", sheet = 1)

toplot(df, var = "Clorofila")

df <- import("data1.xlsx", sheet = 1)

toplot(df, var = "Diametro de Tallo")
toplot(df, var = "Entrenudo")

## the data was made up based on this:
# df$VALORES <- rnorm(2685, mean = 100, sd = 30)
# df$`tallo (mm)` <- rnorm(537, mean = 10, sd = 1.5)
# df$`entrenudo (mm)` <- rnorm(537, mean = 60, sd = 10)
