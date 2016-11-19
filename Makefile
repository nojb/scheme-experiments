go:
	$(MAKE) -C byterun ocamlrund
	scheme --script c.ss
	./byterun/ocamlrund -t a.out

.PHONY: go
