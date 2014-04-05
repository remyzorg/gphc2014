all:
	ocamlopt -o hashcode str.cmxa hash_parse.ml main.ml

clean:
	rm *.cmi *.cmo *.o hashcode
