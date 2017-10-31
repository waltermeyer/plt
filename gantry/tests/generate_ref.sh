for file in ./test*.gty; do
 	[ -e "$file" ] || continue
	touch ${file%.*}.out
	grep -v '^\s*$' "$file"|sed -r 's/^.{10}//' | tail -n1 > ${file%.*}.out
done

for file in ./fail*.gty; do
 	[ -e "$file" ] || continue
	touch ${file%.*}.err
	grep -v '^\s*$' "$file"|sed -r 's/^.{10}//' | tail -n1 > ${file%.*}.err
done
