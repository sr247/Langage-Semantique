#  autotest.sh
#  Credits 2017 Martin Thibault & al
#!/bin/bash

make -k;

#~ dir="test/"
verbose=0
R1=/dev/null
R2=/dev/null
file_color="\e[38;5;82m\e[48;5;239m"

while getopts ":v:" opt; do
  case $opt in
    v)
	  if [ $OPTARG != 1 ] && [ $OPTARG != 2 ]
	  then
	  	printf "Verbose Usage -v 1 or -v 2\n" >&2
		exit 1
	  fi
	  verbose=$OPTARG
      ;;
    \?)
      printf "Invalid option:\n" >&2
      exit 1
      ;;
    :)
	  if [ $OPTARG = v ]
	  then
		printf "Verbose Usage -v 1 or -v 2\n" >&2
	  fi
      exit 1
      ;;
  esac
done

if [ $verbose -gt 0 ]
then
	R1=/dev/tty
fi
if [ $verbose -gt 1 ]
then
	R2=/dev/tty
fi


for fullpath in test/*.cml
do
    touch test/temp
	filename="${fullpath##*/}"
	base="${filename%.[^.]*}"
	printf "\x1B[38;5;82m\x1B[48;5;239m%s :\x1B[0m\n" ${base}.cml
	./VM test/${base}.cml 1> test/temp
	if [ $? != '0' ]
	then
		printf "Exec : \x1B[38;5;196mFailed\x1B[0m\n"
		cat test/temp
        printf "\n"
		continue
	else printf "Exec : \x1B[38;5;46mPass\x1B[0m\n"
	fi
    
    diff test/temp test/valide/${base}.txt 1> /dev/null
	if [ $? != '0' ] 
	then
		printf "Resultat : \x1B[38;5;196mIncorrecte\x1B[0m\n"
		cat test/temp 1> $R1
		printf "\x1B[38;5;196mInstead of :\x1B[0m\n" 1> $R1
		cat test/valide/${base}.txt 1> $R1
	else printf "Resultat : \x1B[38;5;46mCorrecte\x1B[0m\n"
    cat test/temp 1> $R2
	fi
	printf "\n"
done

rm test/temp

#~ Version 0
#~ make -k;
#~ dir="test/"
#~ for f in $(ls $dir)
#~ do
	#~ if [ $f=^*.cml$ ]; then
		#~ echo -e "\e[38;5;82m\e[48;5;239m$f\e[0m"
		#~ ./VM $dir$f
		#~ echo ""
	#~ fi
#~ done
