all: run

compile:
	ghc Main.hs -O2 -o compiler.out
	./compiler.out $(file).lua

run: compile
	$(file).out

clean:
	touch temp.ll
	ls *.ll | grep -xv "template.ll" | xargs rm
	rm -f *.o
	rm -f *.out
	rm -f *.hi
	rm -f *.s
	rm -f ../tests/*.ll
	rm -f ../tests/*.out