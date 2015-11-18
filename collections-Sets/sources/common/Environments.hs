module Environments where


--import qualified Data.Edison.Coll.UnbalancedSet as S

--import qualified Data.Edison.Coll.StandardSet as S

import qualified Data.Edison.Coll.UnbalancedSet as S


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

import Defs

import Ops


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

-- Benchmarks environment setup functions

defaultEnv :: IO ( S.Set Int )
defaultEnv = return ( addNDistinctFrom S.empty baseNElems 0 )


addEnvSetup :: IO ( S.Set Int )
addEnvSetup = do
    defaultEnv


addAllEnvSetup :: IO ( S.Set Int , S.Set Int )
addAllEnvSetup = do
    let
        t = addNDistinctFrom S.empty addAllFromNElems 0
    s <- defaultEnv
    return ( s , t )


-- clear
clearEnvSetup :: IO ( S.Set Int )
clearEnvSetup = do
    return ( addNDistinctFrom S.empty clearNElems 0 )


containsEnvSetup :: IO ( S.Set Int )
containsEnvSetup = do
    defaultEnv


containsAllEnvSetup :: IO ( S.Set Int , S.Set Int )
containsAllEnvSetup = do
    let
        s = addNDistinctFrom S.empty containsAllSearchInNElems 0
        t = addNDistinctFrom S.empty containsAllSearchForNElems 0
    return ( s , t )


-- iterator
iteratorEnvSetup :: IO ( S.Set Int )
iteratorEnvSetup = defaultEnv


removeEnvSetup :: IO ( S.Set Int )
removeEnvSetup = return ( addNDistinctFrom S.empty removeFromNElems 0 )


removeAllEnvSetup :: IO ( S.Set Int , S.Set Int )
removeAllEnvSetup = do
    let
        s = addNDistinctFrom S.empty removeAllFromNElems 0
        t = addNDistinctFrom S.empty removeAllNElems 0
    return ( s , t )


retainAllEnvSetup :: IO ( S.Set Int , S.Set Int )
retainAllEnvSetup = do
    let
        s = addNDistinctFrom S.empty retainAllFromNElems 0
        t = addNDistinctFrom S.empty retainAllToNElems 0
    return ( s , t )


toListEnvSetup :: IO ( S.Set Int )
toListEnvSetup = return ( addNDistinctFrom S.empty toListFromNElems 0 )


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------


