#!/bin/bash

function regex { 
	gawk 'match($0,'$1', ary) {print ary['${2:-'1'}']}'; 
}

path=../testcases

printf "#file \t states \t time\n"
for tcase in $path/*.in
do
	file=$(basename $tcase)
	res=`(./Main < ${tcase})`
	states=`echo $res | regex '/nodes\ssearched:\s([0-9]+)/'`
	tim=`echo $res | regex '/([0-9].[0-9]+)s/'`
	printf "%s\t%d\t%.6f\n" "$file" "$states" "$tim"
done
