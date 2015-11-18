#!/bin/bash


#username = $(USER)


heapImplementations = LazyPairingHeap \
LeftistHeap \
SkewHeap \
SplayHeap


main:
	for heapImplementation in $(heapImplementations) ; \
		do \
		echo -e "\n\nProcessing "$$heapImplementation"...\n\n" ; \
		date ; \
		cabal configure ; \
		cabal build compileForAllHeapImplementations ; \
		cp heapImplementations/$$heapImplementation.txt ./heapImplementations.txt ; \
		mkdir -p tmp && mkdir -p tmp/executables ; \
		./dist/build/compileForAllHeapImplementations/compileForAllHeapImplementations ; \
		cabal configure ; \
		cabal build runForAllBenchmarkTypes ; \
		modprobe msr ; \
		./dist/build/runForAllBenchmarkTypes/runForAllBenchmarkTypes ; \
		chown $(USER):users time-Time.csv energy-Energy.csv ; \
		mkdir -p csvFileSets ; \
		mv time-Time.csv csvFileSets/$$heapImplementation.time.csv ; \
		mv energy-Energy.csv csvFileSets/$$heapImplementation.energy.csv ; \
		date ; \
		rm -fr dist/ ; \
	done


clean :
	cabal clean
	rm -f tmp/executables/*


