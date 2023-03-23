date_change <- function(date){
  shell(cmd = paste0("date ", date))
}

date_change("2022-3-3")
