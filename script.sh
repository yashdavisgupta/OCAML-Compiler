ocamlc slangc.ml -o slangc
pwd
make
./slangc foo bar baz > test
./slangc -length foo bar baz >> test
./slangc -help foo bar baz >> test
diff test test.control > /dev/null 2>&1
error=$?
if [ $error -eq 0 ]
then
   exit 0
elif [ $error -eq 1 ]
then
   exit -1
else
   exit -1
fi
