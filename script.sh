ocamlc slangc.ml -o slangc
pwd
make
./slangc ~/csc312/OCAML-Compiler/testprograms/testprogram1.code  > test
./slangc ~/csc312/OCAML-Compiler/testprograms/testprogram2.code >> test
./slangc ~/csc312/OCAML-Compiler/testprograms/testprogram3.code >> test
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
