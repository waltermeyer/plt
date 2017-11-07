#!/bin/bash  

for file in ./test*.gty; do
 	[ -e "$file" ] || continue
	touch ${file%.*}.out
	sed -n -e '/####TEST####/,$p' $file | tail -n +2 > ${file%.*}.out
done

for file in ./fail*.gty; do
 	[ -e "$file" ] || continue
	touch ${file%.*}.err
	grep -v '^\s*$' "$file"|sed -r 's/^.{10}//' | tail -n1 > ${file%.*}.err
done
