#!/bin/bash
if [ -n "$*" ]; then
	echo 'bc output on left, java on right'
	paste <(cat $* | bc -l) <(java Program $*)
else
	exec java Program
fi
