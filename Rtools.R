sapply(c("installr","stringr"),require,character.on = T)
install.Rtools(choose_version = TRUE, check = FALSE, GUI = TRUE,
page_with_download_url = "https://cran.r-project.org/bin/windows/Rtools/")
