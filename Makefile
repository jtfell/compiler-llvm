
default:
	clang -fPIC -shared lib/rts.c -o lib/rts.so
	stack build && stack exec main
