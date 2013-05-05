NAME = step1

ML  =   labyrinthe.ml \
	main.ml \

MLI =   labyrinthe.mli \

OCAMLFLAGS = -w A

################################################################################

CMX = $(ML:.ml=.cmx)
CMO = $(ML:.ml=.cmo)
CMI = $(ML:.ml=.cmi)

.PHONY: byte clean fclean re
.SUFFIXES: .ml .mli .cmo .cmi .cmx

all: $(NAME)
byte: $(NAME).byte

$(NAME): $(CMX)
	ocamlopt -o $(NAME) $(CMX)

$(NAME).byte: $(CMO)
	ocamlc  -o $(NAME) $+

.ml.cmo:
	ocamlc -c $< $(OCAMLFLAGS)

.mli.cmi:
	ocamlc -c $< $(OCAMLFLAGS)

.ml.cmx:
	ocamlopt -c $< $(OCAMLFLAGS)

clean:
	rm -f $(CMX) $(CMO) $(CMI) $(CMX:.cmx=.o) $(RML) .depend

fclean: clean
	rm -f $(NAME)

re: fclean all

.depend: $(MLI) $(ML)
	ocamldep $(ML) $(MLI) > .depend

-include .depend
