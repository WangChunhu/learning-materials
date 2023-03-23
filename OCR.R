if(!require(tesseract)) install.packages("tesseract")

require(tesseract)

# 查看包含信息，和可以识别文字的包有哪些。
tesseract_info()

# 语言包下载地址
# https://tesseract-ocr.github.io/tessdoc/Data-Files

# 图片-------------------------------------
# 图片支持JPG PNG。
img.file <- "E:/娱乐/壁纸/1.png"
language <- "chi_sim"

text <- ocr(image = img.file,
            engine = tesseract(language), 
            HOCR = F)
cat(text)

# Simple example
img.file <- "https://jeroen.github.io/images/testocr.png"
language <- "eng"

text <- ocr(image = img.file,
            engine = tesseract(language))
cat(text)


# pdf----------------------------------------
## 下载pdf
curl::curl_download(url = "https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf", 
                    destfile = "R-intro.pdf") # 要保存的文件名
## 读入pdf
orig <- pdftools::pdf_text("R-intro.pdf")[1]
cat(orig)

# Render pdf to png image
img_file <- pdftools::pdf_convert("R-intro.pdf", format = 'png', pages = 1, dpi = 400) # format = 'tiff'
img_file


# 支持pdf图文识别,Read from PDF files
pngfile <- pdftools::pdf_text("D:/学习/课题/4.青年科学基金-正文_刘科.pdf")[2]
cat(pngfile)

## 1 转换为png
img_file <- pdftools::pdf_convert("D:/学习/课题/4.青年科学基金-正文_刘科.pdf", format = 'tiff', pages = 1, dpi = 400)
img_file

## 2 OCR文字识别
text <- tesseract::ocr(img_file, 
                       engine = tesseract("chi_sim"))
cat(text)


pngfile <- pdftools::pdf_convert("D:/学习/课题/2016-2020年长江中下游地区县级统计年鉴/湖北省/恩施13/2016.pdf", format = "tiff", pages = 287, dpi = 400)
pngfile

text <- tesseract::ocr(pngfile, 
                       engine = tesseract("chi_sim"))
cat(text)

text <- pdftools::pdf_ocr_text(pdf = "D:/学习/课题/2016-2020年长江中下游地区县级统计年鉴/湖北省/恩施13/2016.pdf", pages = 287, language = "chi_sim", dpi = 400)
cat(text)



