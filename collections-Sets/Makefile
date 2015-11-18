#!/bin/bash


#username = $(USER)


setImplementations = StandardSet \
UnbalancedSet


main:
	for setImplementation in $(setImplementations) ; \
		do \
		echo -e "\n\nProcessing "$$setImplementation"...\n\n" ; \
		date ; \
		cabal configure ; \
		cabal build compileForAllSetImplementations ; \
		cp setImplementations/$$setImplementation.txt ./setImplementations.txt ; \
		mkdir -p tmp && mkdir -p tmp/executables ; \
		./dist/build/compileForAllSetImplementations/compileForAllSetImplementations ; \
		cabal configure ; \
		cabal build runForAllBenchmarkTypes ; \
		modprobe msr ; \
		./dist/build/runForAllBenchmarkTypes/runForAllBenchmarkTypes ; \
		chown $(USER):users time-Time.csv energy-Energy.csv ; \
		mkdir -p csvFileSets ; \
		mv time-Time.csv csvFileSets/$$setImplementation.time.csv ; \
		mv energy-Energy.csv csvFileSets/$$setImplementation.energy.csv ; \
		date ; \
		rm -fr dist/ ; \
	done


clean :
	cabal clean
	rm -f tmp/executables/*


