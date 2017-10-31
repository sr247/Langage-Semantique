# !/bin/bash
make -k;
dir="test/"
for f in $(ls $dir)
do
	if [ $f=^*.cml$ ]; then
		echo -e "\e[38;5;82m\e[48;5;239m$f\e[0m"
		./VM $dir$f
		echo ""
	fi
done
