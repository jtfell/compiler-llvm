LLVM_DIR=/usr/local/Cellar/llvm-5.0/5.0.0/bin/

default:
	stack build
	### First we generate LLVM IR representation of the program and runtime lib
	# Use my custom compiler for the actual program
	stack exec main > main.ll
	# Use clang for the runtime lib
	clang -emit-llvm -S lib/gc/lib.c  -o lib/gc/lib.ll
	clang -emit-llvm -S lib/gc/pool.c -o lib/gc/pool.ll

	# Use LLVM assembler for converting LLVM IR to bitcodes
	$(LLVM_DIR)/llvm-as-5.0 lib/gc/lib.ll -o lib/gc/lib.bc
	$(LLVM_DIR)/llvm-as-5.0 lib/gc/pool.ll -o lib/gc/pool.bc
	$(LLVM_DIR)/llvm-as-5.0 main.ll -o main.bc

	# Use LLVM link to generate a single bitcode file
	$(LLVM_DIR)/llvm-link-5.0 main.bc lib/gc/lib.bc lib/gc/pool.bc -o output.bc

	# Execute the bitcode using the LLVM interpreter
	$(LLVM_DIR)/lli-5.0 output.bc
