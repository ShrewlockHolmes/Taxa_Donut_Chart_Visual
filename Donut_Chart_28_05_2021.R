library(ggplot2)
library(tidyverse)
library(knitr)
library(RColorBrewer)

df <- read.table("Zeale_Taxa.csv", sep = ',', header = TRUE)

tab_path <- ("./Zeale_Taxa.csv")

df <- read.table(tab_path, sep = ',', header = TRUE)

dfb <- df %>% # the %>% pipes commands through R
  mutate(value = 1) %>%
  ###########################################################
################ !!!! ATTENTION !!!! ######################
###########################################################
group_by(lev.1 = class, # Fill in which taxonomic levels you want
         lev.2 = order, 
         lev.3 = family) %>% # level 3 is the outter layer 
  summarise(otus = sum(value)) %>%
  mutate(value = 1)   # mutate() adds new variables

lvl0 <- tibble(lev.1 = "Diet", value = 0, level = 0, fill = NA)

lvl1 <- dfb %>% # the %>% pipes commands through R
  group_by(lev.1) %>% # i feel this is self explanitory
  summarise(value = sum(value)) %>% # adds values within each group, then saves only these two columns
  #ungroup() #%>% # ungroups again - this is not required here as groups have gone using summarise()
  mutate(level = 1) %>%  # mutate() adds new variables
  mutate(fill = lev.1) #%>%

lvl2 <- dfb %>% # df at order level
  group_by(lev.1, lev.2) %>% # i feel this is self explanitory
  summarise(value = sum(value)) %>% # adds values
  ungroup() %>% # ungroups again
  mutate(level = 2) %>%  # mutate() adds new variables
  mutate(fill = lev.2) # %>% # remove this if I don't want the ame in the graph 
#mutate(name = order)

lvl3 <- dfb %>%
  select(lev.1 = lev.1, lev.3 = lev.3, value, fill = lev.2, otus) %>%
  #mutate(level = 3) %>%
  # mutate(lv2lab = family) %>% ######
arrange(lev.1, fill, lev.3) %>%
  mutate(level = 3) #%>%


lvl3$num <- nrow(lvl3):1
lvl3$ang <- NA
lvl3$label <- NA

angcoef <- 360/nrow(lvl3)

for (i in 1:nrow(lvl3)){
  lvl3$ang[i] <- lvl3$num[i]*angcoef
  lvl3$label[i] <- paste(lvl3$lev.3[i], " ", lvl3$otus[i])    # Use this line if NOT using species
  #lvl3$label[i] <- paste(lvl3$lev.3[i])                             # Use this line if using species
}

mid <- round(nrow(lvl3)/2) + 1

for (i in mid:nrow(lvl3)){
  lvl3$ang[i] <- lvl3$num[i]*angcoef + 180
  lvl3$label[i] <- paste(lvl3$otus[i], " ", lvl3$lev.3[i])    # Use this line if NOT using species
  #lvl3$label[i] <- paste(lvl3$lev.3[i])                             # Use this line if using species
}

#########

angcoef <- 360/nrow(lvl3)

for (i in 1:nrow(lvl3)){
  lvl3$ang[i] <- lvl3$num[i]*angcoef
  lvl3$label[i] <- paste(lvl3$lev.3[i], " ", lvl3$otus[i])    # Use this line if NOT using species
  #lvl3$label[i] <- paste(lvl3$lev.3[i])                             # Use this line if using species
}

mid <- round(nrow(lvl3)/2) + 1

for (i in mid:nrow(lvl3)){
  lvl3$ang[i] <- lvl3$num[i]*angcoef + 180
  lvl3$label[i] <- paste(lvl3$otus[i], " ", lvl3$lev.3[i])    # Use this line if NOT using species
  #lvl3$label[i] <- paste(lvl3$lev.3[i])                             # Use this line if using species
}


#########


comb_d <- bind_rows(lvl0, lvl1, lvl2, lvl3)  %>%
  #fct_reorder2(class, level) # %>% # fct_reorder2() reorders the factors
  arrange(lev.1, level, num) %>%
  mutate(level = as.factor(level)) %>%
  mutate(fill = as.factor(fill))

ord <- unique(comb_d$fill)             # these lines are to get everything in order as appears
comb_d$fill <- factor(comb_d$fill,      # in the data frame. otherwise the order automatically
                      levels = ord)         # reverts to alphabetical


n <- lvl2$value
names(n) <- lvl2$fill

for (i in 1:length(n)){
  o <- which(lvl3[,"fill"] == names(n[i]))
  lvl3$ang[o]
  mean(lvl3$ang[o])
  comb_d[which(comb_d[,"lev.2"] == names(n[i])),"ang"] <- mean(lvl3$ang[o])
}

p <- as.character(lvl1$lev.1)

for (i in 1:length(p)){
  o <- which(lvl3[,"lev.1"] == p[i])
  lvl3$ang[o]
  mean(lvl3$ang[o])
  comb_d[which(comb_d[,"fill"] == p[i]),"ang"] <- mean(lvl3$ang[o])
}

for (i in 1:length(p)){
  comb_d$label[which(comb_d[,"fill"] == p[i])] <- p[i]
}

p <- ggplot(comb_d, aes(x = level, y = value, fill = fill, alpha = level)) # group = ?

p +  geom_col(width = 1, color = "gray40", size = 0.25, position = position_stack()) +
  #geom_text(aes(label = lev.2), alpha = 1, position = position_stack(vjust = 0.5), 
  geom_text(aes(label = lev.2, angle = (ang+86)), alpha = 1, position = position_stack(vjust = 0.5), 
            ################ THOMAS!!!!! You can change the letter size here!!!
            size = 2.5) + # This is the size of level 2 label letters
  geom_text(aes(label = label, angle = (ang+86)), alpha = 1, position = position_stack(vjust = 0.5),
            ############### THOMAS!!!! More leter size changing!!!!!
            size = 2.5) + # This is the size of the level 3 label letters
  coord_polar(theta = "y", direction = -1) +
  scale_alpha_manual(values = c("0" = 0, "1" = 1, "2" = 0.7, "3" = 0.5), guide = F) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  #scale_fill_brewer(palette = "Dark2", na.translate = F) +
  #scale_fill_manual(values = mycolors) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(legend.position = "none")


