ataabu: parser.cmx tokenizer.cmx codegen.cmx
	mkdir build
	ocamlopt -o build/ataabu tokenizer.cmx parser.cmx codegen.cmx
codegen.cmx: tokenizer.cmx parser.cmx codegen.ml
	ocamlopt -c codegen.ml
parser.cmx: tokenizer.cmx parser.ml
	ocamlopt -c parser.ml
tokenizer.cmx: tokenizer.ml
	ocamlopt -c tokenizer.ml
clean:
	rm -rf *.cmi *.cmx *.o build/
