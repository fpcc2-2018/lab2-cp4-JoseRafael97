library(tidyverse)
library(lubridate)
library(here)

message("Lendo dados brutos de eventos")

events = read_csv("https://github.com/wikimedia-research/Discovery-Hiring-Analyst-2016/raw/master/events_log.csv.gz")

# events = events %>% slice(1:5e4) # Útil para testar código em dados pequenos. Comente na hora de processá-los para valer.

message("Transformando em dados por busca")


searches = events %>% 
    group_by(session_id) %>% 
    arrange(timestamp) %>% 
    summarise(
        session_start_timestamp = first(timestamp),
        session_end_timestamp = ymd_hms(last(timestamp)),
        session_start_date = ymd_hms(first(timestamp)),
        session_durantion_sec = difftime( ymd_hms(last(na.omit(timestamp))), ymd_hms(first(na.omit(timestamp))), units="secs"),
        session_durantion_min = difftime( ymd_hms(last(na.omit(timestamp))), ymd_hms(first(na.omit(timestamp))), units="mins"),
        group = first(group), # eventos de uma mesma sessão são de um mesmo grupo
        results = max(n_results, na.rm = 0), # se não houver busca, retorna -Inf
        position_clicked = median(result_position, na.rm = TRUE), # se não houver busca, retorna -Inf
        num_clicks = sum(action == "visitPage"), 
        search_index = sum(action == "searchResultPage"),
        first_click = ifelse(num_clicks == 0, 
                             NA_integer_, 
                             first(na.omit(result_position))
        )
    )  

out_file = here("data/search_data.csv")

message("Salvando em ", out_file)

searches %>% 
    write_csv(out_file)