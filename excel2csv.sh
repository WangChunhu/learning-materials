#!/bin/bash
echo "本程序共两个参数;功能是将excel转换为csv"
echo "参数一:input"
echo "参数二:output"
function excel2csv.linux(){
	R -q --save -e "excel2csv <- function(input,output){require(readxl);data <- read_excel(input,col_names = T);write.csv(data,output,row.names = F)}"
	R -q -e "excel2csv(\"$1\",\"$2\")"
	return
}
excel2csv.linux $1 $2
