ataabu: parser.cmx tokenizer.cmx type_inf.cmx closure_conv.cmx codegen.cmx
	mkdir build
	ocamlopt -o build/ataabu tokenizer.cmx parser.cmx type_inf.cmx closure_conv.cmx codegen.cmx
codegen.cmx: tokenizer.cmx parser.cmx type_inf.cmx closure_conv.cmx codegen.ml
	ocamlopt -c codegen.ml
type_inf.cmx: tokenizer.cmx parser.cmx type_inf.ml
	ocamlopt -c type_inf.ml
parser.cmx: tokenizer.cmx parser.ml
	ocamlopt -c parser.ml
tokenizer.cmx: tokenizer.ml
	ocamlopt -c tokenizer.ml
closure_conv.cmx: closure_conv.ml
	ocamlopt -c closure_conv.ml
clean:
	rm -rf *.cmi *.cmx *.cmo *.o build/
