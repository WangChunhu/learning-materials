extract2plot_colours <- function(path.pictures, num_col = 15){
  #' @describeIn 提取本地路径下所有图片颜色(PNG, JPG, JPEG, TIFF)
  #' @param path.pictures 图片存放路径
  #' @param num_col 每张图片提取颜色个数
  
  # # 颜色自由
  # BiocManager::install('EBImage')
  # require(EBImage)
  # devtools::install_github("ramnathv/rblocks")
  # devtools::install_github("woobe/rPlotter")# 颜色自由
  # BiocManager::install('EBImage')
  # require(EBImage)
  # devtools::install_github("ramnathv/rblocks")
  # devtools::install_github("woobe/rPlotter")
  # p <- rPlotter::extract_colours("D:/娱乐/壁纸/人民币.jpeg", num_col = 15)
  # barplot(1:15, col = p)
  
  
  # 图片存放路径(PNG, JPG, JPEG, TIFF)
  path.pictures <- file.path(path.pictures)
  # 生成图片完整路径
  path <- paste0(path.pictures, "/", dir(path.pictures))
  # 根据num_col提取图片颜色
  colors <- lapply(path, rPlotter::extract_colours, num_col = num_col) %>% unlist()
  # 颜色行数
  rep <- length(colors) / num_col
  data <- data.frame(x = sort(rep(1:num_col, rep)), y = rep(1:rep, num_col), z = sort(colors)); data
  
  ggplot2::ggplot(data, aes(x = x, y = y)) +
    geom_tile(mapping = aes(fill = z), show.legend = F)+
    geom_text(mapping = aes(x = x, y = y, label = z))+
    scale_fill_manual(values = data$z)+
    labs(title = paste0("共", num_col, " × ", rep, "个颜色供您精心挑选"))+
    theme_void()+
    theme(plot.title = element_text(hjust = 0.5, size = 20))
  
}

# # 参数
# ## 图片存放路径(PNG, JPG, JPEG, TIFF)[网页或本地]
# path.pictures <- "D:/娱乐/壁纸/"
# ## 每张图片提取颜色个数
# num_col <- 5
# 
# extract2plot_colours(path.pictures = path.pictures, num_col = num_col)