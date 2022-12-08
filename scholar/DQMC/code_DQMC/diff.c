{
declare -i n
declare -i nt
read nt
let n=1
while [ $n -le  $nt   ];
  do
    read C
    echo $C
    let n=n+1
    echo $C
      diff $C ../Prog_tau/$C
    done
} < List
