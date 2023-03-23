#!/bin/bash
#SBATCH -N 54
#SBATCH --ntasks=54
#SBATCH --job-name=idw
#SBATCH --cpus-per-task=3
#SBATCH -p fat
#SBATCH -t 30-1

# 提交sbatch任务
# sbatch /public/home/zzuaga06/yanhl/prj/huhu/huhu.met/future_2036-2065_插值/sbatch.future.idw.sh /public/home/zzuaga06/yanhl/prj/huhu/huhu.met/future_2036-2065_插值/run.list

# 参数1 run.list
runList=$1

if [ ! -n $runlist ] ; then
    echo "No runlist file!"
    exit 1
fi

if [ -n $maxnode ] ; then
    maxnode=$SLURM_JOB_NUM_NODES
fi

# 创建管道文件，绑定管道号1000到文件，删除文件（防止写入读出只能同步进行）
mkfifo testfifo
exec 1000<>testfifo
rm -fr testfifo

# 通过向管道符1000写入空行来设定后台最大进程数
for ((n=1;n<=$((SLURM_JOB_NUM_NODES + 1));n++))
do
    echo >&1000
done

# 记录开始时间
dt1=`date +%Y-%m-%d'_'%H:%M`
echo $dt1
start=`date "+%s"`

# 提交任务
cat $runList | while read list
do 
    read -u1000
    {
        echo `date` "Now running with $list" >> /public/home/zzuaga06/yanhl/prj/huhu/huhu.met/future_2036-2065_插值/run.list.log
        srun -N1 -n1 -c3 -o ${list}.out -e ${list}.error sh $list
        echo `date` "Now job ${list} is done" >> /public/home/zzuaga06/yanhl/prj/huhu/huhu.met/future_2036-2065_插值/run.list.log
        echo >&1000
    }&
done


sleep 24h
# 等待所有任务完成后向下进行
wait

# 记录结束时间
dt2=`date +%Y-%m-%d'_'%H:%M`
echo $dt2
end=`date "+%s"`

echo "Time range: `expr $end - $start `"

# 删除管道符1000
exec 1000>&-
exec 1000<&-