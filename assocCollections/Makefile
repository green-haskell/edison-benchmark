#!/bin/bash


#username = $(USER)


assocCollectionImplementations = AssocList \
StandardMap


main:
	for assocCollectionImplementation in $(assocCollectionImplementations) ; \
		do \
		echo -e "\n\nProcessing "$$assocCollectionImplementation"...\n\n" ; \
		date ; \
		cabal configure ; \
		cabal build compileForAllAssocCollectionImplementations ; \
		cp assocCollectionImplementations/$$assocCollectionImplementation.txt ./assocCollectionImplementations.txt ; \
		mkdir -p tmp && mkdir -p tmp/executables ; \
		./dist/build/compileForAllAssocCollectionImplementations/compileForAllAssocCollectionImplementations ; \
		cabal configure ; \
		cabal build runForAllBenchmarkTypes ; \
		modprobe msr ; \
		./dist/build/runForAllBenchmarkTypes/runForAllBenchmarkTypes ; \
		chown $(USER):users time-Time.csv energy-Energy.csv ; \
		mkdir -p csvFileSets ; \
		mv time-Time.csv csvFileSets/$$assocCollectionImplementation.time.csv ; \
		mv energy-Energy.csv csvFileSets/$$assocCollectionImplementation.energy.csv ; \
		date ; \
		rm -fr dist/ ; \
	done


clean :
	cabal clean
	rm -f tmp/executables/*


