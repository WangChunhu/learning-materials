#!/bin/bash
# 生成插值任务列表

workdir="/public/home/zzuaga06/yanhl/prj/huhu/huhu.met/future_2036-2065_插值"

for i in `seq 1 189`
do echo '#!/bin/bash' > $workdir/idw.$i.sh ; echo -e ". ~/miniconda3/bin/activate\nRscript /public/home/zzuaga06/yanhl/prj/huhu/huhu.met/idw.服务器执行.R -t $i" >> $workdir/idw.$i.sh
done 

ls idw.*.sh > run.list
