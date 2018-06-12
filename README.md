
git clone https://github.com/green-haskell/criterion.git

git clone https://github.com/gilbertomelfe/edison.git

git clone https://github.com/green-haskell/edison-benchmark.git

cd edison-benchmark

cabal sandbox init

cabal sandbox add-source ../criterion/

cabal sandbox add-source ../edison/edison-api/

cabal sandbox add-source ../edison/edison-core/


cd <assocCollections|collections-Heaps|collections-Sets|sequences>

cabal sandbox init --sandbox=../.cabal-sandbox/

cabal install --dependencies-only


As root:

make 2>&1 | tee fullOutput.txt
