#!/bin/bash  

for file in ./test*.gty; do
	if [[ `grep "****TEST****" $file` ]]; then
	  sed -n -e '/\*\*\*\*TEST\*\*\*\*/,$p' $file | tail -n +2 | head -n -1 > $file.out
	  echo "Generated $(echo $file | cut -c 3-).out"
	fi
done

for file in ./fail*.gty; do
	if [[ `grep "****TEST****" $file` ]]; then
	  sed -n -e '/\*\*\*\*TEST\*\*\*\*/,$p' $file | tail -n +2 | head -n -1 > $file.err
	  echo "Generated $(echo $file | cut -c 3-).err"
	fi
done
