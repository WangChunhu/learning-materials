# 地图数据
boder <- sf::read_sf("D:/学习/地理数据/中国省级地图/chinaMap/china.shp")
nine <- sf::read_sf("D:/学习/地理数据/中国省级地图/九段线/SouthSea/九段线.shp") 
island <- sf::read_sf("D:/学习/地理数据/中国省级地图/九段线/SouthSea/bou2_4l.shp")

# 绘制中国地图
fig = ggplot() +
  geom_sf(data = boder, fill = 'NA',  color = 'black') + # 中国地图轮廓
  geom_sf(data = nine, color = 'black')+
  geom_sf(data = island, color = 'black')+
  coord_sf(ylim = c(18, 53)) + #截取中国地图，不在大图上显示九段线区域
  # 添加比例尺
  annotation_scale(location='bl') +
  # 添加指北针
  annotation_north_arrow(location='tr', which_north='false',
                         style=north_arrow_nautical()) +
  # 设置大图边框
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.title = element_blank(), axis.text = element_text(size = 20))
fig

# 绘制九段线小图
nine_p = ggplot() +
  geom_sf(data = boder, fill = 'NA',  color = 'black') + # 中国地图轮廓
  geom_sf(data = nine, color = 'black')+
  geom_sf(data = island, color = 'black')+
  coord_sf(xlim = c(105, 125), ylim = c(0, 22))+
  theme(aspect.ratio = 1.25, #调节长宽比
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "grey10", linetype = 1, size = 0.5),
        plot.margin=unit(c(0, 0, 0, 0), "mm"))
nine_p

all = ggdraw() +
  draw_plot(fig) +
  draw_plot(nine_p, x = 0.8, y = 0.1, width = 0.13, height = 0.39)
all

ggsave(filename = "D:/学习/地理数据/中国省级地图/china.map.article.pdf", width = 8, height = 8)