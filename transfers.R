#### Transfers and Fees ####
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/wiki/List_of_English_football_transfers_summer_2024"
page <- read_html(url)
summer_transfers <- page %>% html_nodes("table") %>% .[[2]] %>% html_table(fill = T)

url <- "https://en.wikipedia.org/wiki/List_of_English_football_transfers_winter_2024%E2%80%9325"
page <- read_html(url)
winter_transfers <- page %>% html_nodes("table") %>% .[[1]] %>% html_table(fill = T)
colnames(winter_transfers)[2] <- "Player"

transfers <- rbind(summer_transfers, winter_transfers)
colnames(transfers) <- c("Date", "Player", "Moving_From", "Moving_To", "Fee")
transfers <- transfers %>% filter(Moving_To %in% c("Liverpool", "Arsenal", "Nottingham Forest",
                                                        "Chelsea", "Manchester City", "Newcastle United",
                                                        "Brighton & Hove Albion", "Fulham", "Aston Villa",
                                                        "Bournemouth", "Brentford", "Crystal Palace", 
                                                        "Manchester United", "Tottenham Hotspur", 
                                                        "Everton", "West Ham United",
                                                        "Wolverhampton Wanderers", "Ipswich Town",
                                                        "Leicester City", "Southampton")) %>% 
  select(-Date) %>% arrange(Moving_To)
transfers[5, 4] <- as.character(14.0)
transfers[7, 4] <- as.character(10.0)
transfers[9, 4] <- as.character(5.0)
transfers[20, 4] <- as.character(1.9)
transfers[23, 4] <- as.character(8.4)
transfers[25, 4] <- as.character(1.3)
transfers[27, 4] <- as.character(6.7)
transfers[44, 4] <- as.character(11.0)
transfers[70, 4] <- as.character(5.7)
transfers[75, 4] <- as.character(18.1)
transfers[79, 4] <- as.character(8.9)
transfers[81, 4] <- as.character(12.0)
transfers[88, 4] <- as.character(11.8)
transfers[89, 4] <- as.character(13.7)
transfers[106, 4] <- as.character(1.5)
transfers[111, 4] <- as.character(19.9)
transfers[113, 4] <- as.character(9.8)
transfers[114, 4] <- as.character(5.6)
transfers[115, 4] <- as.character(1.3)
transfers[116, 4] <- as.character(34.7)
transfers[119, 4] <- as.character(5.9)
transfers[121, 4] <- as.character(9.3)
transfers[130, 4] <- as.character(15.0)
transfers[137, 4] <- as.character(4.2)
transfers[143, 4] <- as.character(3.4)
transfers[149, 4] <- as.character(24.7)
transfers[153, 4] <- as.character(12.6)
transfers[155, 4] <- as.character(8.4)
transfers[156, 4] <- as.character(4.2)
transfers[163, 4] <- as.character(10.1)
transfers[164, 4] <- as.character(15.2)
transfers <- transfers[!grepl("Undisclosed", transfers$Fee), ]
transfers$Fee <- gsub("£", "", transfers$Fee)
transfers$Fee <- trimws(gsub("\\[.*?\\]", "", transfers$Fee))
transfers$Fee <- gsub("m-plus", "", transfers$Fee)
transfers$Fee <- gsub("m", "", transfers$Fee)
transfers$Fee <- gsub("Free", "0", transfers$Fee)
transfers$Fee <- as.numeric(transfers$Fee)
transfers_total <- transfers
transfers$Fee[is.na(transfers$Fee)] <- 0
transfers_total$Fee[is.na(transfers_total$Fee)] <- 0

#### Stats and Age ####
urls <- c("https://fbref.com/en/squads/18bb7c10/Arsenal-Stats",
          "https://fbref.com/en/squads/822bd0ba/Liverpool-Stats",
          "https://fbref.com/en/squads/b2b47a98/Newcastle-United-Stats",
          "https://fbref.com/en/squads/cff3d9bb/Chelsea-Stats",
          "https://fbref.com/en/squads/8602292d/Aston-Villa-Stats",
          "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats",
          "https://fbref.com/en/squads/e4a775cb/Nottingham-Forest-Stats",
          "https://fbref.com/en/squads/d07537b9/Brighton-and-Hove-Albion-Stats",
          "https://fbref.com/en/squads/cd051869/Brentford-Stats",
          "https://fbref.com/en/squads/fd962109/Fulham-Stats",
          "https://fbref.com/en/squads/4ba7cbea/Bournemouth-Stats",
          "https://fbref.com/en/squads/47c64c55/Crystal-Palace-Stats",
          "https://fbref.com/en/squads/d3fd31cc/Everton-Stats",
          "https://fbref.com/en/squads/8cec06e1/Wolverhampton-Wanderers-Stats",
          "https://fbref.com/en/squads/7c21e445/West-Ham-United-Stats",
          "https://fbref.com/en/squads/19538871/Manchester-United-Stats",
          "https://fbref.com/en/squads/361ca564/Tottenham-Hotspur-Stats",
          "https://fbref.com/en/squads/a2d435b3/Leicester-City-Stats",
          "https://fbref.com/en/squads/b74092de/Ipswich-Town-Stats",
          "https://fbref.com/en/squads/33c895d4/Southampton-Stats")

scrape_and_label <- function(url) {
  Sys.sleep(10)
  page <- read_html(url)
  
  table <- page %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table(fill = TRUE)
  
  colnames(table) <- table[1, ]
  table <- table[-1, ]
  
  return(table)
}

combined_table <- urls %>%
  lapply(scrape_and_label) %>%
  bind_rows()

combined_table <- combined_table %>% select(Player, Pos, Age, MP, Starts, Min)
combined_table$Age <- sapply(strsplit(combined_table$Age, "-"), function(x) {
  years <- as.numeric(x[1])
  days  <- as.numeric(x[2])
  years + days / 365
})
combined_table$Age <- round(combined_table$Age, digits = 2)

transfers <- left_join(transfers, combined_table)
transfers$Min <- gsub(",", "", transfers$Min)
transfers$Min <- as.numeric(transfers$Min)
transfers <- filter(transfers, Min >= 360)
transfers <- arrange(transfers, Player)
transfers <- transfers[-15, ]


#### Player Ratings ####
ratings <- read.csv("Player_Ratings.csv", header = T)
transfers <- left_join(transfers, ratings)
rm(combined_table, page, ratings, summer_transfers, winter_transfers, url, urls, scrape_and_label)
transfers$avg <- round(transfers$avg, digits = 2)

#### Data Viz ####
library(gt)
library(scales)
library(formattable)
library(gtExtras)
Overview <- read.csv("Prem_Spending.csv", header = T)
Overview[, 2:4] <- round((Overview[, 2:4]* 0.84), digits = 2)
Overview$badge <- paste0("/Users/robg/Documents/R_Projects/transfer_value/badges/",
                          Overview$Club, ".png")
Overview <- arrange(Overview, Net)
Overview$Outgoing <- paste0("£", Overview$Outgoing)
Overview$Outgoing <- paste0(Overview$Outgoing, "m")
Overview$Incoing <- paste0("£", Overview$Incoing)
Overview$Incoing <- paste0(Overview$Incoing, "m")
Overview$Net <- paste0(Overview$Net, "m")
Overview$Net <- paste0("£", Overview$Net)
Overview_Top_10 <- Overview %>% slice(1:10)
Overview_Bottom_10 <- Overview %>% slice(11:20)

Overview_Top_10 %>% select(badge, Club, Outgoing, Incoing, Net) %>% 
  gt() %>% gt_img_rows(badge, img_source = "local") %>% 
  cols_label(badge = "", Club = "Club", Outgoing = "Spent", Incoing = "Received", Net = "Net") %>% 
  cols_align(align = "center",columns = everything()) %>% 
  tab_options(heading.title.font.size = 32, source_notes.font.size = 12,
              column_labels.font.weight = "bold", column_labels.font.size = 18,
              heading.title.font.weight = "bold", heading.subtitle.font.size = 15)  %>% 
  tab_header(title = "EPL Transfer Spend - 24/25") %>% 
  tab_source_note("Created by Rob Gilligan | @RobG195") %>% 
  tab_style(style = cell_fill(color = "grey92"),
            locations = cells_body(rows = seq(2, 10, 2))) %>% 
  gt_highlight_cols(Net, fill = "gold", alpha = 0.5)

Overview_Bottom_10 %>% select(badge, Club, Outgoing, Incoing, Net) %>% 
  gt() %>% gt_img_rows(badge, img_source = "local") %>% 
  cols_label(badge = "", Club = "Club", Outgoing = "Spent", Incoing = "Received", Net = "Net") %>% 
  cols_align(align = "center",columns = everything()) %>% 
  tab_options(heading.title.font.size = 32, source_notes.font.size = 12,
              column_labels.font.weight = "bold", column_labels.font.size = 18,
              heading.title.font.weight = "bold", heading.subtitle.font.size = 15)  %>% 
  tab_header(title = "EPL Transfer Spend - 24/25") %>% 
  tab_source_note("Created by Rob Gilligan | @RobG195") %>% 
  tab_style(style = cell_fill(color = "grey92"),
            locations = cells_body(rows = seq(2, 10, 2))) %>% 
  gt_highlight_cols(Net, fill = "gold", alpha = 0.5)

top5 <- transfers %>% arrange(-avg) %>% slice(1:5)
top5$badge <- paste0("/Users/robg/Documents/R_Projects/transfer_value/badges/",
                     top5$Moving_To, ".png")
top5$player_pic <- paste0("/Users/robg/Documents/R_Projects/transfer_value/player_pic/",
                          top5$Player, ".png")
top5$rank <- 1:5
top5$Fee <- paste0("£", top5$Fee)
top5$Fee <- paste0(top5$Fee, "m")
top5 %>% select(rank, player_pic, Player, badge, Moving_To, Fee, MP, avg) %>% 
  gt() %>% gt_img_rows(badge, img_source = "local") %>% gt_img_rows(player_pic, img_source = "local") %>% 
  cols_label(rank = "", player_pic = "", Player = "Player", badge = "", Moving_To = "Club",
             Fee = "Fee", MP = "Matches", avg = html("Rating<sup>*</sup>")) %>%
  cols_align(align = "center",columns = everything()) %>% 
  tab_options(heading.title.font.size = 32, source_notes.font.size = 12,
              column_labels.font.weight = "bold", column_labels.font.size = 18,
              heading.title.font.weight = "bold", heading.subtitle.font.size = 15,
              footnotes.font.size = 12)  %>% 
  tab_header(title = "Top 5 Best Transfers 24/25",
             subtitle = "(minnimum 360 mins played)") %>% 
  tab_source_note("Created by Rob Gilligan | @RobG195") %>% 
  tab_style(style = cell_fill(color = "grey92"),
            locations = cells_body(rows = seq(2, 5, 2))) %>% 
  gt_highlight_cols(avg, fill = "gold", alpha = 0.5) %>% 
  tab_footnote(
    footnote = html("<sup>*</sup>Average of player's fotmob and sofascore rating"))

bot5 <- transfers %>% arrange(avg) %>% slice(1:5)
bot5$badge <- paste0("/Users/robg/Documents/R_Projects/transfer_value/badges/",
                     bot5$Moving_To, ".png")
bot5$player_pic <- paste0("/Users/robg/Documents/R_Projects/transfer_value/player_pic/",
                          bot5$Player, ".png")
bot5$rank <- 1:5
bot5$Fee <- paste0("£", bot5$Fee)
bot5$Fee <- paste0(bot5$Fee, "m")
bot5 %>% select(rank, player_pic, Player, badge, Moving_To, Fee, MP, avg) %>% 
  gt() %>% gt_img_rows(badge, img_source = "local") %>% gt_img_rows(player_pic, img_source = "local") %>% 
  cols_label(rank = "", player_pic = "", Player = "Player", badge = "", Moving_To = "Club",
             Fee = "Fee", MP = "Matches", avg = html("Rating<sup>*</sup>")) %>%
  cols_align(align = "center",columns = everything()) %>% 
  tab_options(heading.title.font.size = 32, source_notes.font.size = 12,
              column_labels.font.weight = "bold", column_labels.font.size = 18,
              heading.title.font.weight = "bold", heading.subtitle.font.size = 15,
              footnotes.font.size = 12)  %>% 
  tab_header(title = "Top 5 Worst Transfers 24/25",
             subtitle = "(minnimum 360 mins played)") %>% 
  tab_source_note("Created by Rob Gilligan | @RobG195") %>% 
  tab_style(style = cell_fill(color = "grey92"),
            locations = cells_body(rows = seq(2, 5, 2))) %>% 
  gt_highlight_cols(avg, fill = "gold", alpha = 0.5) %>% 
  tab_footnote(
    footnote = html("<sup>*</sup>Average of player's fotmob and sofascore rating"))

Num_and_rating <- transfers %>% group_by(Moving_To) %>% summarise(transfers = n(),
                                                                  rating = round(mean(avg), digits = 2))
Num_and_rating[5, 1] <- as.character("Brighton")
Num_and_rating[19, 1] <- as.character("Wolves")
Num_and_rating$badge <- paste0("/Users/robg/Documents/R_Projects/transfer_value/badges/",
                                             Num_and_rating$Moving_To, ".png")
library(ggimage)
library(showtext)
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()
Num_and_rating %>% ggplot(aes(x = transfers, y = rating)) +
  geom_point() +
  geom_image(aes(image = badge))+
  theme(
    plot.background = element_rect(fill = "antiquewhite1", color = "antiquewhite1"),
    panel.background = element_rect(fill = "antiquewhite1", color = "antiquewhite1"),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 15, family = "Roboto", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "Roboto"),
    axis.title = element_text(family = 'Roboto', face = "bold", size = 12),
    axis.text = element_text(face = "bold"),
    plot.caption = element_text(size = 9, hjust = 1, family = "Roboto", 
                                face = "bold"),
    panel.grid = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 0),
    legend.text = element_text(family = "Roboto"),
    legend.background = element_rect(fill = "antiquewhite1", color = "antiquewhite1")
  )+
  labs(title = "Transfer Rating EPL Clubs in 24/25 Season",
       subtitle = "(only non-loan transfers that played 360+ mins)", caption = "Created by Rob Gilligan | @RobG195") +
  labs(x = "Transfers", y = "Average Transfer Rating") + 
  scale_x_continuous(breaks = c(1:9)) +
  scale_y_continuous(breaks = c(6.6, 6.7, 6.8, 6.9, 7.0, 7.1, 7.2))

p <- transfers %>% ggplot(aes(x = Age, y = Fee)) +
  geom_point(size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "darkorchid4", linetype = "dashed") +
  theme(
    plot.background = element_rect(fill = "antiquewhite1", color = "antiquewhite1"),
    panel.background = element_rect(fill = "antiquewhite1", color = "antiquewhite1"),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 15, family = "Roboto", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "Roboto"),
    axis.title = element_text(family = 'Roboto', face = "bold", size = 12),
    axis.text = element_text(face = "bold"),
    plot.caption = element_text(size = 9, hjust = 1, family = "Roboto", 
                                face = "bold"),
    panel.grid = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 0),
    legend.text = element_text(family = "Roboto"),
    legend.background = element_rect(fill = "antiquewhite1", color = "antiquewhite1")
  )+
  labs(title = "Transfer Age vs Fee for EPL 24/25 Teams",
       subtitle = "(only non-loan transfers that played 360+ mins)", caption = "Created by Rob Gilligan | @RobG195") +
  labs(x = "Age", y = "Trnasfer Fee (million £)")

library(cowplot)
logo <- paste0("/Users/robg/Documents/R_Projects/transfer_value/EPL.png")
ggdraw() + 
  draw_plot(p) +
  draw_image(
    logo, x = 0.98, y = 1, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.1
  )

