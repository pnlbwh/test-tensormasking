.PHONY: all
all:
	stack build 

.PHONY: run
run:
	stack exec ppl

bsub%:
	bsub -J tensormasking -o "masking-%J.out" -e "masking-%J.err" -q "big-multi" -n $* stack exec ppl

.PHONY: clean
clean:
	rm -f *.out *.err core.*
