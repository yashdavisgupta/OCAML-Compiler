pwd
make all
./slangc.native ~/csc312/OCAML-Compiler/testprograms/testprogram1.code  > test
./slangc.native ~/csc312/OCAML-Compiler/testprograms/testprogram2.code >> test
./slangc.native ~/csc312/OCAML-Compiler/testprograms/testprogram3.code >> test
diff test ./testprograms/test.control > /dev/null 2>&1
error=$?
./slangc.native -lex ~/csc312/OCAML-Compiler/testprograms/testprogram1.code > test
diff test ./testprograms/testprogram1.lex.out > /dev/null 2>&1
error1=$?
./slangc.native -lex ~/csc312/OCAML-Compiler/testprograms/testprogram2.code > test
diff test ./testprograms/testprogram2.lex.out > /dev/null 2>&1
error2=$?
./slangc.native -lex ~/csc312/OCAML-Compiler/testprograms/testprogram3.code > test
diff test ./testprograms/testprogram3.lex.out > /dev/null 2>&1
error3=$?
./slangc.native -parse ~/csc312/OCAML-Compiler/testprograms/testprogram1.code > test
diff test ./testprograms/testprogram1.parse.out > /dev/null 2>&1
error4=$?
./slangc.native -parse ~/csc312/OCAML-Compiler/testprograms/testprogram2.code > test
diff test ./testprograms/testprogram2.parse.out > /dev/null 2>&1
error5=$?
./slangc.native -parse ~/csc312/OCAML-Compiler/testprograms/testprogram3.code > test
diff test ./testprograms/testprogram3.parse.out > /dev/null 2>&1
error6=$?
rm test
if [ $error -eq 0 ] && [ $error1 -eq 0 ] && [ $error2 -eq 0 ] && [ $error3 -eq 0 ] && [ $error4 -eq 0 ] && [ $error5 -eq 0 ] && [ $error6 -eq 0 ]
then
   exit 0
else
   exit -1
fi
