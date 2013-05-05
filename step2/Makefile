NAME = step2

ML  =   labyrinthe.ml \
	init.ml \
	main.ml \

MLI =   labyrinthe.mli \

OCAMLFLAGS = -I +sdl

OCAMLSDLCMO = -I +sdl bigarray.cma sdl.cma sdlloader.cma sdlmixer.cma

OCAMLSDLCMX = -I +sdl bigarray.cmxa sdl.cmxa sdlloader.cmxa sdlmixer.cmxa

################################################################################

CMX = $(ML:.ml=.cmx)
CMO = $(ML:.ml=.cmo)
CMI = $(ML:.ml=.cmi)

.PHONY: byte clean fclean re
.SUFFIXES: .ml .mli .cmo .cmi .cmx

all: $(NAME)
byte: $(NAME).byte

$(NAME): $(CMX)
	ocamlopt $(OCAMLSDLCMX) -o $(NAME) $(CMX)

$(NAME).byte: $(CMO)
	ocamlc $(OCAMLSDLCMO) -o $(NAME) $+

.ml.cmo:
	ocamlc -c $(OCAMLFLAGS) $<

.mli.cmi:
	ocamlc -c $(OCAMLFLAGS) $<

.ml.cmx:
	ocamlopt -c $(OCAMLFLAGS) $<

clean:
	rm -f $(CMX) $(CMO) $(CMI) $(CMX:.cmx=.o) $(RML) .depend

fclean: clean
	rm -f $(NAME)

re: fclean all

.depend: $(MLI) $(ML)
	ocamldep $(ML) $(MLI) > .depend

-include .depend
