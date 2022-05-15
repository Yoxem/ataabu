ataabu: parser.cmx tokenizer.cmx type_inf.cmx
	mkdir build
	ocamlopt -o build/ataabu tokenizer.cmx parser.cmx type_inf.cmx
#codegen.cmx: tokenizer.cmx parser.cmx codegen.ml
#	ocamlopt -c codegen.ml
type_inf.cmx: tokenizer.cmx parser.cmx type_inf.ml
	ocamlopt -c type_inf.ml
parser.cmx: tokenizer.cmx parser.ml
	ocamlopt -c parser.ml
tokenizer.cmx: tokenizer.ml
	ocamlopt -c tokenizer.ml
clean:
	rm -rf *.cmi *.cmx *.cmo *.o build/
