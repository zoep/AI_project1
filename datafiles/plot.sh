#!/bin/bash

gnuplot << EOF
	set terminal png size 1024, 1024
	set output "graph.png"

	set xlabel "States"
	set ylabel "Time (sec)"
	set grid lt 0 lw 1
	set tmargin 4

	set key tmargin right box	
	
	plot "manhattan.out" u 2:3 title "Admissible heuristic (manhattan distance)"  w lp lw 2, \
	     "euclidean.out" u 2:3 title "Non-Admissible heuristic (euclidean distance)"  w lp lw 2
EOF
