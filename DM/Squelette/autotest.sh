# !/bin/bash
make -k
for f in $(ls test/)
do
	echo "$f : "
	./VM test/$f
	echo ""
done
