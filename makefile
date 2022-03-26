attabu: parser.cmx tokenizer.cmx
	mkdir build
	ocamlopt -o build/ataabu tokenizer.cmx parser.cmx
parser.cmx: tokenizer.cmx parser.ml
	ocamlopt -c parser.ml
tokenizer.cmx: tokenizer.ml
	ocamlopt -c tokenizer.ml
clean:
	rm -rf *.cmi *.cmx *.o build/
