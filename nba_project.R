library(tidyverse)
library(RSelenium)
library(rvest)
library(lubridate)
library(mailR)
library(patchwork)

# Get status NBA function ----

get_stats_nba <- function(link){

read_html(link) %>% 
    html_nodes("table") %>% 
    html_table(fill = TRUE) %>%
    tibble() %>% 
    slice(13:13) %>% unnest()
    
}

read_html("https://basketball.realgm.com/nba/stats/2020/Averages/Qualified/points/All/desc/1/Regular_Season") %>% 
    html_nodes("table") %>% 
    html_table(fill = TRUE)

#read_html("https://basketball.realgm.com/nba/stats/2020/Averages/Qualified/points/All/desc/1/Regular_Season") %>% html_nodes("table") %>% html_table(fill = TRUE)
# Player Stats

nba_player_status_pages <- c("https://basketball.realgm.com/nba/stats/2020/Averages/Qualified/points/All/desc/1/Regular_Season", 
                             "https://basketball.realgm.com/nba/stats/2020/Averages/Qualified/points/All/desc/2/Regular_Season",
                             "https://basketball.realgm.com/nba/stats/2020/Averages/Qualified/points/All/desc/3/Regular_Season",
                             "https://basketball.realgm.com/nba/stats/2020/Averages/Qualified/points/All/desc/4/Regular_Season",
                             "https://basketball.realgm.com/nba/stats/2020/Averages/Qualified/points/All/desc/5/Regular_Season"
)

player_status_tbl <- nba_player_status_pages %>% map(get_stats_nba) %>% tibble() %>% unnest() %>% select(-.)

# Player Stats plot

player_stats_ggplot <- player_status_tbl %>%
    select(Player, PPG) %>% 
    top_n(20) %>%
    mutate(Player = Player %>% as_factor() %>% fct_reorder(PPG)) %>% 
    arrange(desc(PPG)) %>% 
    
    ggplot(aes(Player, PPG, fill = PPG)) +
    
    geom_col() +
    geom_text(aes(label = PPG), position = position_dodge(0.5), hjust = -0.3) +
    coord_flip() +
    theme_minimal() +
    scale_fill_viridis_c(option = "D") +
    
    labs(
        title = "Jogadores Líderes de Estatística em Pontos por Jogo - NBA",
        subtitle = "Top 20 jogadores em PPG da temporada regular da NBA de 2019-2020",
        x = "Jogador",
        y = "Pontos por Jogo (PPG)",
        caption = str_glue("\n Gráfico: @jonatas.souza | Dados: basketball.realgm.com
                           \n Referência: {now()}")
    ) +
    theme(axis.title.y=element_blank())

player_stats_ggplot


# Team Stats

team_stats_tbl <- get_stats_nba("https://basketball.realgm.com/nba/team-stats")


# Team Stats plot

team_stats_ggplot <- team_stats_tbl %>%
    select(Team, PPG) %>% 
    top_n(20) %>%
    mutate(Team = Team %>% as_factor() %>% fct_reorder(PPG)) %>% 
    arrange(desc(PPG)) %>% 
    
    ggplot(aes(Team, PPG, fill = PPG)) +
    
    geom_col() +
    geom_text(aes(label = PPG), position = position_dodge(0.5), hjust = -0.3) + 
    coord_flip() +
    theme_minimal() +
    scale_fill_viridis_c(option = "D") +
    
    labs(
        title = "Times Líderes de Estatística em Pontos por Jogo - NBA",
        subtitle = "Top 20 Times em PPG da temporada regular da NBA de 2019-2020",
        x = "Time",
        y = "Pontos por Jogo (PPG)",
        caption = str_glue("\n Gráfico: @jonatas.souza | Dados: basketball.realgm.com
        \n Referência: {now()}")
    ) +
    theme(axis.title.y=element_blank()) +
    scale_y_continuous(limits = c(0,130))

team_stats_ggplot


double_plot_ggplot <- team_stats_ggplot + player_stats_ggplot


player_stats_png <- ggsave(plot = player_stats_ggplot, filename = "player_stats.png", dpi=500)

team_stats_png <- ggsave(plot = team_stats_ggplot, filename = "team_stats.png", dpi=500)

double_plot_png <- ggsave(plot = double_plot_ggplot, filename = "double_plot.png", dpi=500)

send.mail(from = "jntscardoso@gmail.com", to = "jntscardoso@gmail.com",
          subject = ref,
          body = str_glue("Status da liga em - {today()}") %>% as.character(),
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "jntscardoso@gmail.com",            
                      passwd = "itamar2530", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE,
          attach.files = c("player_stats.png", "team_stats.png", "double_plot.png"))




