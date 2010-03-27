RESULT = agenda
SOURCES = AnsiLib.ml \
	  Agenda.ml
LIBS = unix
INCDIRS =
OCAMLMAKEFILE = ./OCamlMakefile

include $(OCAMLMAKEFILE)

run : byte-code
	./$(RESULT) $(shell cat args)
