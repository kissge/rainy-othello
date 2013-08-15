RESULT =reversi
SOURCES=color.ml command.ml commandParser.mly commandLexer.mll theory.ml play.ml main.ml 
LIBS=unix 
all: native-code
#all: byte-code 

-include OCamlMakefile 
