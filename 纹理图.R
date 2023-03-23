#require(devtools)
# devtools::install_github("coolbutuseless/lofi")      # Colour encoding
# devtools::install_github("coolbutuseless/minisvg",force = T)   # SVG creation
# devtools::install_github("coolbutuseless/devout",force = T)    # Device interface
# devtools::install_github("coolbutuseless/devoutsvg") # This package
#devtools::install_github("coolbutuseless/poissoned")
#devtools::install_github("coolbutuseless/svgpatternsimple") 
#devtools::install_github("coolbutuseless/svgpatternusgs") 
#devtools::install_github("ImageMagick/ImageMagick") 
require(lofi)
require(minisvg)
require(devout)
require(devoutsvg)
require(svgpatternsimple)


x <- rep(1:5, each = 3)
y <- rep(c('A','B','C'),times = 5)
set.seed(1234)
z <- round(runif(min = 10, max = 20, n = 15))
df <- data.frame(x= x, y = y, z = z)

gear4 <- encode_pattern_params_as_hex_colour( pattern_name = 'null', colour = '#123456')
gear6 <- encode_pattern_params_as_hex_colour( pattern_name = 'stipple', colour = '#ff4455', spacing = 10)
gear8 <- encode_pattern_params_as_hex_colour(pattern_name  = 'hex', angle = 0,spacing = 20, fill_fraction = 0.1,colour  = '#125634')
yanse <- c(gear4,gear6,gear8)   #scale_fill_manual(values=yanse)

require(ggplot2)
svgout(pattern_pkg = "svgpatternsimple",filename = "C:/Users/³Â½¨Õä/Desktop/1.svg")
ggplot(data = df,aes(x,z,fill=y))+
  geom_bar(stat = "identity",position=position_dodge(.9),color="black")+
  scale_fill_pattern_simple(
    pattern_name  = c("null",'stripe', 'stipple'),
    fill_fraction = c(0,0.1,0.1), 
    angle         = c(90), 
    spacing       = c(10,10,10)) +
  theme_bw()
invisible(dev.off())








