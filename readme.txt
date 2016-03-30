You first need to have at least installed :
-OCAML 4.0
-ocamlbuild
-the ocamlgraph lib

then run ./make

it will build main.native and main.byte

you can either run : 
	./make clean to clean the _build
	./make native to build only the native code
	./make byte to build the bytecode
	./make all same as ./make

you can add the -opt option in order to build with ocamlopt 
