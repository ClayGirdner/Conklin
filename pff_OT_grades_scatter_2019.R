library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(ggalt)
library(readxl)
library(ggimage)
library(nflscrapR)

# pff theme for plot
source("theme_pff.R")

# load in data
pff_OT_both <- read_xlsx("pff_OT_grades.xlsx", sheet = "combined")

# map pff team names to nflsrapR and join color codes/logo urls
mapping <- read_xlsx("pff_nflscrapR_mapping.xlsx")
nfl_logos <- read_csv("nfl_team_logos.csv")
pff_OT_both <- left_join(pff_OT_both, mapping, by = c("team_name" = "PFF"))
pff_OT_both <- left_join(pff_OT_both, nflteams, by = c("nflscrapR" = "abbr"))
pff_OT_both <- left_join(pff_OT_both, nfl_logos, by = c("nflscrapR" = "team_code"))

# filter out guys with fewer than 1000 snaps
pff_OT_both <- filter(pff_OT_both, snap_counts_offense >= 1000)

# Caclulate medians
median_pass_grade <- median(pff_OT_both$grades_pass_block)
median_run_grade <- median(pff_OT_both$grades_run_block)


# Plot scatter
pff_OT_both %>%
    ggplot(aes(x = grades_pass_block, y = grades_run_block)) +
    geom_point(data = filter(pff_OT_both, player != "Greg Robinson", player != "Chris Hubbard", player != "Jack Conklin"),
               aes(color = primary),
               size = 2) +
    # add labels
    geom_text(data = subset(pff_OT_both, player == "Greg Robinson" | player == "Chris Hubbard" | player == "Jack Conklin"),
              aes(color = primary, label = player),
              nudge_y = 2.5,
              fontface = "bold",
              size = 5) +
    # we need this to assign colors
    scale_color_identity() +
    # add logos
    geom_image(data = subset(pff_OT_both, player == "Greg Robinson" | player == "Chris Hubbard" | player == "Jack Conklin"),
               aes(image = url)) +
    # reference lines with median for each variable
    geom_hline(aes(yintercept = median_run_grade), color = "black", linetype = "dashed", alpha=0.4) +
    geom_vline(aes(xintercept =  median_pass_grade), color = "black", linetype = "dashed", alpha=0.4) +
    # plot labels
    labs(x = "Pass Block Grade",
         y = "Run Block Grade",
         caption = "1,000+ snaps",
         title = "Instant Improvement in the Run Game",
         subtitle = "Pro Football Focus (PFF) offensive tackle grades, 2018-2019") +
    # Pff theme
    theme_pff +
    # cosmetic adjustments
    theme(panel.grid.major = element_blank())

ggsave("pics/pff_OT_grades_scatter_both.png", height=7, width=8)