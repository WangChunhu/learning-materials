require(data.table)
# require(BiocManager)
# install("EnhancedVolcano")
# install("airway")
# install("apeglm")
require(EnhancedVolcano)
data <- openxlsx::read.xlsx("I:/许福春/702/MWXS-20-712/2.Basic_analysis/Difference_analysis/WT_vs_MU/WT_vs_MU_info.xlsx")
require(magrittr)
#数据处理用edgeR
#画图
pdf("I:/棉花/许福春/702/WT-MU_vol_self.pdf",width = 6.7,height = 5)
EnhancedVolcano(data,
                #selectLab = c("ENSG00000152583","ENSG00000148175"),
                lab = NA,
                x = 'Log2FC',
                y = 'p_value',
                xlim = c(-21, 2),
                ylim = c(0,9),
                pCutoff = 0.05,
                FCcutoff = 1,
                pointSize = 3,
                #labSize = 3.0,
                colAlpha = 1,
                legendPosition = 'right',
                legendLabSize = 10,
                legendIconSize = 2.0,
                drawConnectors = T,
                widthConnectors = 0.2,
                colConnectors = 'black',
                col = c("grey30", "forestgreen", "royalblue", "red2",colAlpha = .2),
                boxedLabels = T
                ) -> p
p +
  theme_classic()
dev.off()
