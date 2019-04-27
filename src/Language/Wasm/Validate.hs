{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Wasm.Validate (
    ValidationError(..),
    ValidationResult(..),
    validate,
    isValid,
    ValidModule,
    getModule,
) where

import Language.Wasm.Structure
import qualified Data.Set as Set
import Data.List (foldl')
import qualified Data.Text.Lazy as TL
import Data.Maybe (fromMaybe, maybeToList, catMaybes)
import Data.Monoid ((<>))
import Numeric.Natural (Natural)

import Control.Monad (foldM)
import Control.Monad.Reader (ReaderT, runReaderT, withReaderT, ask)
import Control.Monad.Except (Except, runExcept, throwError)

import Debug.Trace as Debug

data ValidationError =
    DuplicatedExportNames [String]
    | InvalidTableType
    | MinMoreThanMaxInMemoryLimit
    | MemoryLimitExceeded
    | AlignmentOverflow
    | MoreThanOneMemory
    | MoreThanOneTable
    | FunctionIndexOutOfRange
    | TableIndexOutOfRange
    | MemoryIndexOutOfRange
    | LocalIndexOutOfRange
    | GlobalIndexOutOfRange
    | LabelIndexOutOfRange
    | TypeIndexOutOfRange
    | ResultTypeDoesntMatch
    | TypeMismatch { actual :: Arrow, expected :: Arrow }
    | InvalidResultArity
    | InvalidConstantExpr
    | InvalidStartFunctionType
    | ImportedGlobalIsNotConst
    | ExportedGlobalIsNotConst
    | GlobalIsImmutable
    deriving (Show, Eq)

data ValidationResult = Invalid ValidationError | Valid

instance Semigroup ValidationResult where
  (<>) = mappend

instance Monoid ValidationResult where
    mempty = Valid
    mappend Valid vr = vr
    mappend vr Valid = vr
    mappend vr _ = vr

toEither :: ValidationResult -> Either ValidationError ()
toEither (Invalid e) = Left e
toEither Valid = Right ()

isValid :: ValidationResult -> Bool
isValid Valid = True
isValid (Invalid reason) = Debug.trace ("Module mismatched with reason " ++ show reason) $ False

type Validator = Module -> ValidationResult

data VType =
    Val ValueType
    | Var
    | Any
    deriving (Show, Eq)

type End = [VType]

empty :: [ValueType]
empty = []

class ToEnd a where
    toEnd :: a -> [VType]

instance ToEnd VType where
    toEnd val = [val]

instance ToEnd ValueType where
    toEnd val = [Val val]

instance ToEnd [ValueType] where
    toEnd = map Val

instance ToEnd [VType] where
    toEnd = id

data Arrow = Arrow End End deriving (Show, Eq)

(==>) :: (ToEnd a, ToEnd b) => a -> b -> Arrow
(==>) a b = Arrow (toEnd a) (toEnd b)

asArrow :: FuncType -> Arrow
asArrow (FuncType params results) = Arrow (map Val params) (map Val results)

isArrowMatch :: Arrow -> Arrow -> Bool
isArrowMatch (f `Arrow` t) ( f' `Arrow` t') = isEndMatch f f' && isEndMatch t t'
    where
        isEndMatch :: End -> End -> Bool
        isEndMatch (Any:l) (Any:r) =
            let (leftTail, rightTail) = unzip $ zip (takeWhile (/= Any) $ reverse l) (takeWhile (/= Any) $ reverse r) in
            isEndMatch (reverse leftTail) (reverse rightTail)
        isEndMatch (Any:l) r =
            let (leftTail, rightTail) = unzip $ zip (takeWhile (/= Any) $ reverse l) (takeWhile (/= Any) $ reverse r) in
            isEndMatch (reverse leftTail) (reverse rightTail)
        isEndMatch l (Any:r) =
            let (leftTail, rightTail) = unzip $ zip (takeWhile (/= Any) $ reverse l) (takeWhile (/= Any) $ reverse r) in
            isEndMatch (reverse leftTail) (reverse rightTail)
        isEndMatch (Var:l) (x:r) =
            let subst = replace Var x in
            isEndMatch (subst l) (subst r)
        isEndMatch (x:l) (Var:r) =
            let subst = replace Var x in
            isEndMatch (subst l) (subst r)
        isEndMatch (Val v:l) (Val v':r) = v == v' && isEndMatch l r
        isEndMatch [] [] = True
        isEndMatch _ _ = False

data Ctx = Ctx {
    types :: [FuncType],
    funcs :: [FuncType],
    tables :: [TableType],
    mems :: [Limit],
    globals :: [GlobalType],
    locals :: [ValueType],
    labels :: [Maybe ValueType],
    returns :: Maybe ValueType,
    importedGlobals :: Natural
} deriving (Show, Eq)

type Checker = ReaderT Ctx (Except ValidationError)

freshVar :: Checker VType
freshVar = return Var

runChecker :: Ctx -> Checker a -> Either ValidationError a
runChecker ctx = runExcept . flip runReaderT ctx

runCheckerV :: Ctx -> Checker ValidationResult -> ValidationResult
runCheckerV ctx = either Invalid id . runChecker ctx

(!?) :: [a] -> Natural -> Maybe a
(!?) (x:_) 0 = Just x
(!?) (_:rest) n = rest !? (n - 1)
(!?) [] _ = Nothing

safeHead :: [a] -> Maybe a
safeHead (x: _) = Just x
safeHead [] = Nothing

maybeToEither :: ValidationError -> Maybe a -> Checker a
maybeToEither _ (Just a) = return a
maybeToEither l Nothing = throwError l

asType :: GlobalType -> VType
asType (Const v) = Val v
asType (Mut v) = Val v

shouldBeMut :: GlobalType -> Checker ()
shouldBeMut (Mut _) = pure ()
shouldBeMut (Const v) = throwError GlobalIsImmutable

getLabel :: LabelIndex -> Checker (Maybe ValueType)
getLabel lbl = do
    Ctx { labels } <- ask
    case labels !? lbl of
        Nothing -> throwError LabelIndexOutOfRange
        Just v -> return v

withLabel :: [ValueType] -> Checker a -> Checker a
withLabel result = withReaderT (\ctx -> ctx { labels = safeHead result : labels ctx })

isMemArgValid :: Int -> MemArg -> Checker ()
isMemArgValid sizeInBytes MemArg { align } = if 2 ^ align <= sizeInBytes then pure () else throwError AlignmentOverflow

checkMemoryInstr :: Int -> MemArg -> Checker ()
checkMemoryInstr size memarg = do
    isMemArgValid size memarg
    Ctx { mems } <- ask 
    if length mems < 1 then throwError MemoryIndexOutOfRange else pure ()

getInstrType :: Instruction Natural -> Checker Arrow
getInstrType Unreachable = return $ Any ==> Any
getInstrType Nop = return $ empty ==> empty
getInstrType Block { resultType, body } = do
    let blockType = empty ==> resultType
    t <- withLabel resultType $ getExpressionType body
    if isArrowMatch t blockType
    then return $ empty ==> resultType
    else throwError $ TypeMismatch t blockType
getInstrType Loop { resultType, body } = do
    let blockType = empty ==> resultType
    t <- withLabel [] $ getExpressionType body
    if isArrowMatch t blockType
    then return $ empty ==> resultType
    else throwError $ TypeMismatch t blockType
getInstrType If { resultType, true, false } = do
    let blockType = empty ==> resultType
    l <- withLabel resultType $ getExpressionType true
    r <- withLabel resultType $ getExpressionType false
    if isArrowMatch l blockType
    then (if isArrowMatch r blockType then (return $ I32 ==> resultType) else (throwError $ TypeMismatch r blockType))
    else throwError $ TypeMismatch l blockType
getInstrType (Br lbl) = do
    r <- map Val . maybeToList <$> getLabel lbl
    return $ (Any : r) ==> Any
getInstrType (BrIf lbl) = do
    r <- map Val . maybeToList <$> getLabel lbl
    return $ (r ++ [Val I32]) ==> r
getInstrType (BrTable lbls lbl) = do
    r <- getLabel lbl
    rs <- mapM getLabel lbls
    if all (== r) rs
    then return $ ([Any] ++ (map Val $ maybeToList r) ++ [Val I32]) ==> Any
    else throwError ResultTypeDoesntMatch
getInstrType Return = do
    Ctx { returns } <- ask
    return $ (Any : (map Val $ maybeToList returns)) ==> Any
getInstrType (Call fun) = do
    Ctx { funcs } <- ask
    maybeToEither FunctionIndexOutOfRange $ asArrow <$> funcs !? fun
getInstrType (CallIndirect sign) = do
    Ctx { types, tables } <- ask
    if length tables < 1
    then throwError TableIndexOutOfRange
    else do
        Arrow from to <- maybeToEither TypeIndexOutOfRange $ asArrow <$> types !? sign
        return $ (from ++ [Val I32]) ==> to
getInstrType Drop = do
    var <- freshVar
    return $ var ==> empty
getInstrType Select = do
    var <- freshVar
    return $ [var, var, Val I32] ==> var
getInstrType (GetLocal local) = do
    Ctx { locals }  <- ask
    t <- maybeToEither LocalIndexOutOfRange $ locals !? local
    return $ empty ==> Val t
getInstrType (SetLocal local) = do
    Ctx { locals } <- ask
    t <- maybeToEither LocalIndexOutOfRange $ locals !? local
    return $ Val t ==> empty
getInstrType (TeeLocal local) = do
    Ctx { locals } <- ask
    t <- maybeToEither LocalIndexOutOfRange $ locals !? local
    return $ Val t ==> Val t
getInstrType (GetGlobal global) = do
    Ctx { globals } <- ask
    t <- maybeToEither GlobalIndexOutOfRange $ asType <$> globals !? global
    return $ empty ==> t
getInstrType (SetGlobal global) = do
    Ctx { globals } <- ask
    t <- maybeToEither GlobalIndexOutOfRange $ asType <$> globals !? global
    shouldBeMut $ globals !! fromIntegral global
    return $ t ==> empty
getInstrType (I32Load memarg) = do
    checkMemoryInstr 4 memarg
    return $ I32 ==> I32
getInstrType (I64Load memarg) = do
    checkMemoryInstr 8 memarg
    return $ I32 ==> I64
getInstrType (F32Load memarg) = do
    checkMemoryInstr 4 memarg
    return $ I32 ==> F32
getInstrType (F64Load memarg) = do
    checkMemoryInstr 8 memarg
    return $ I32 ==> F64
getInstrType (I32Load8S memarg) = do
    checkMemoryInstr 1 memarg
    return $ I32 ==> I32
getInstrType (I32Load8U memarg) = do
    checkMemoryInstr 1 memarg
    return $ I32 ==> I32
getInstrType (I32Load16S memarg) = do
    checkMemoryInstr 2 memarg
    return $ I32 ==> I32
getInstrType (I32Load16U memarg) = do
    checkMemoryInstr 2 memarg
    return $ I32 ==> I32
getInstrType (I64Load8S memarg) = do
    checkMemoryInstr 1 memarg
    return $ I32 ==> I64
getInstrType (I64Load8U memarg) = do
    checkMemoryInstr 1 memarg
    return $ I32 ==> I64
getInstrType (I64Load16S memarg) = do
    checkMemoryInstr 2 memarg
    return $ I32 ==> I64
getInstrType (I64Load16U memarg) = do
    checkMemoryInstr 2 memarg
    return $ I32 ==> I64
getInstrType (I64Load32S memarg) = do
    checkMemoryInstr 4 memarg
    return $ I32 ==> I64
getInstrType (I64Load32U memarg) = do
    checkMemoryInstr 8 memarg
    return $ I32 ==> I64
getInstrType (I32Store memarg) = do
    checkMemoryInstr 4 memarg
    return $ [I32, I32] ==> empty
getInstrType (I64Store memarg) = do
    checkMemoryInstr 8 memarg
    return $ [I32, I64] ==> empty
getInstrType (F32Store memarg) = do
    checkMemoryInstr 4 memarg
    return $ [I32, F32] ==> empty
getInstrType (F64Store memarg) = do
    checkMemoryInstr 8 memarg
    return $ [I32, F64] ==> empty
getInstrType (I32Store8 memarg) = do
    checkMemoryInstr 1 memarg
    return $ [I32, I32] ==> empty
getInstrType (I32Store16 memarg) = do
    checkMemoryInstr 2 memarg
    return $ [I32, I32] ==> empty
getInstrType (I64Store8 memarg) = do
    checkMemoryInstr 1 memarg
    return $ [I32, I64] ==> empty
getInstrType (I64Store16 memarg) = do
    checkMemoryInstr 2 memarg
    return $ [I32, I64] ==> empty
getInstrType (I64Store32 memarg) = do
    checkMemoryInstr 4 memarg
    return $ [I32, I64] ==> empty
getInstrType CurrentMemory = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError MemoryIndexOutOfRange else return $ empty ==> I32
getInstrType GrowMemory = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError MemoryIndexOutOfRange else return $ I32 ==> I32
getInstrType (I32Const _) = return $ empty ==> I32
getInstrType (I64Const _) = return $ empty ==> I64
getInstrType (F32Const _) = return $ empty ==> F32
getInstrType (F64Const _) = return $ empty ==> F64
getInstrType (IUnOp BS32 _) = return $ I32 ==> I32
getInstrType (IUnOp BS64 _) = return $ I64 ==> I64
getInstrType (IBinOp BS32 _) = return $ [I32, I32] ==> I32
getInstrType (IBinOp BS64 _) = return $ [I64, I64] ==> I64
getInstrType I32Eqz = return $ I32 ==> I32
getInstrType I64Eqz = return $ I64 ==> I32
getInstrType (IRelOp BS32 _) = return $ [I32, I32] ==> I32
getInstrType (IRelOp BS64 _) = return $ [I64, I64] ==> I32
getInstrType (FUnOp BS32 _) = return $ F32 ==> F32
getInstrType (FUnOp BS64 _) = return $ F64 ==> F64
getInstrType (FBinOp BS32 _) = return $ [F32, F32] ==> F32
getInstrType (FBinOp BS64 _) = return $ [F64, F64] ==> F64
getInstrType (FRelOp BS32 _) = return $ [F32, F32] ==> I32
getInstrType (FRelOp BS64 _) = return $ [F64, F64] ==> I32
getInstrType I32WrapI64 = return $ I64 ==> I32
getInstrType (ITruncFU BS32 BS32) = return $ F32 ==> I32
getInstrType (ITruncFU BS32 BS64) = return $ F64 ==> I32
getInstrType (ITruncFU BS64 BS32) = return $ F32 ==> I64
getInstrType (ITruncFU BS64 BS64) = return $ F64 ==> I64
getInstrType (ITruncFS BS32 BS32) = return $ F32 ==> I32
getInstrType (ITruncFS BS32 BS64) = return $ F64 ==> I32
getInstrType (ITruncFS BS64 BS32) = return $ F32 ==> I64
getInstrType (ITruncFS BS64 BS64) = return $ F64 ==> I64
getInstrType I64ExtendSI32 = return $ I32 ==> I64
getInstrType I64ExtendUI32 = return $ I32 ==> I64
getInstrType (FConvertIU BS32 BS32) = return $ I32 ==> F32
getInstrType (FConvertIU BS32 BS64) = return $ I64 ==> F32
getInstrType (FConvertIU BS64 BS32) = return $ I32 ==> F64
getInstrType (FConvertIU BS64 BS64) = return $ I64 ==> F64
getInstrType (FConvertIS BS32 BS32) = return $ I32 ==> F32
getInstrType (FConvertIS BS32 BS64) = return $ I64 ==> F32
getInstrType (FConvertIS BS64 BS32) = return $ I32 ==> F64
getInstrType (FConvertIS BS64 BS64) = return $ I64 ==> F64
getInstrType F32DemoteF64 = return $ F64 ==> F32
getInstrType F64PromoteF32 = return $ F32 ==> F64
getInstrType (IReinterpretF BS32) = return $ F32 ==> I32
getInstrType (IReinterpretF BS64) = return $ F64 ==> I64
getInstrType (FReinterpretI BS32) = return $ I32 ==> F32
getInstrType (FReinterpretI BS64) = return $ I64 ==> F64


replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace x y (v:r) = (if x == v then y else v) : replace x y r

getExpressionType :: Expression -> Checker Arrow
getExpressionType = fmap ([] `Arrow`) . foldM go []
    where
        go :: [VType] -> Instruction Natural -> Checker [VType]
        go stack instr = do
            (f `Arrow` t) <- getInstrType instr
            matchStack stack (reverse f) t
        
        matchStack :: [VType] -> [VType] -> [VType] -> Checker [VType]
        matchStack stack@(Any:_) _arg res = return $ res ++ stack
        matchStack (Val v:stack) (Val v':args) res =
            if v == v'
            then matchStack stack args res
            else throwError $ TypeMismatch ((reverse $ Val v':args) `Arrow` res) ([] `Arrow` (Val v:stack))
        matchStack _ (Any:_) res = return $ res
        matchStack (Val v:stack) (Var:args) res =
            let subst = replace Var (Val v) in
            matchStack stack (subst args) (subst res)
        matchStack stack [] res = return $ res ++ stack
        matchStack [] args res = throwError $ TypeMismatch ((reverse args) `Arrow` res) ([] `Arrow` [])
        matchStack _ _ _ = error "inconsistent checker state"

isConstExpression :: Expression -> Checker ()
isConstExpression [] = pure ()
isConstExpression ((I32Const _):rest) = isConstExpression rest
isConstExpression ((I64Const _):rest) = isConstExpression rest
isConstExpression ((F32Const _):rest) = isConstExpression rest
isConstExpression ((F64Const _):rest) = isConstExpression rest
isConstExpression ((GetGlobal idx):rest) = do
    Ctx {globals, importedGlobals} <- ask
    if importedGlobals <= idx
        then throwError GlobalIndexOutOfRange
        else pure ()
    case globals !! fromIntegral idx of
        Const _ -> isConstExpression rest
        Mut _ -> throwError InvalidConstantExpr
isConstExpression _ = throwError InvalidConstantExpr

getFuncTypes :: Module -> [FuncType]
getFuncTypes Module {types, functions, imports} =
    let funImports = catMaybes $ map getFuncType imports in
    funImports ++ map ((types !!) . fromIntegral . funcType) functions
    where
        getFuncType (Import _ _ (ImportFunc typeIdx)) = Just $ types !! (fromIntegral typeIdx)
        getFuncType _ = Nothing

ctxFromModule :: [ValueType] -> [Maybe ValueType] -> Maybe ValueType -> Module -> Ctx
ctxFromModule locals labels returns m@Module {types, tables, mems, globals, imports} =
    let tableImports = catMaybes $ map getTableType imports in
    let memsImports = catMaybes $ map getMemType imports in
    let globalImports = catMaybes $ map getGlobalType imports in
    Ctx {
        types,
        funcs = getFuncTypes m,
        tables = tableImports ++ map (\(Table t) -> t) tables,
        mems = memsImports ++ map (\(Memory l) -> l) mems,
        globals = globalImports ++ map (\(Global g _) -> g) globals,
        locals,
        labels,
        returns,
        importedGlobals = fromIntegral $ length globalImports
    }
    where
        getTableType (Import _ _ (ImportTable tableType)) = Just tableType
        getTableType _ = Nothing

        getMemType (Import _ _ (ImportMemory lim)) = Just lim
        getMemType _ = Nothing

        getGlobalType (Import _ _ (ImportGlobal gl)) = Just gl
        getGlobalType _ = Nothing

isFunctionValid :: Function -> Validator
isFunctionValid Function {funcType, localTypes = locals, body} mod@Module {types} =
    if fromIntegral funcType < length types
    then
        let FuncType params results = types !! fromIntegral funcType in
        if length results > 1
        then Invalid InvalidResultArity
        else
            let r = safeHead results in
            let ctx = ctxFromModule (params ++ locals) [r] r mod in
            runCheckerV ctx $ do
              arr <- getExpressionType body
              if isArrowMatch arr (empty ==> results)
              then pure Valid
              else throwError $ TypeMismatch arr (empty ==> results)
    else Invalid TypeIndexOutOfRange

functionsShouldBeValid :: Validator
functionsShouldBeValid mod@Module {functions} =
    foldMap (flip isFunctionValid mod) functions

tablesShouldBeValid :: Validator
tablesShouldBeValid Module { imports, tables } =
    let tableImports = filter isTableImport imports in
    let res = foldMap (\Import { desc = ImportTable t } -> isValidTableType t) tableImports in
    let res' = foldl' (\r (Table t) -> r <> isValidTableType t) res tables in
    if length tableImports + length tables <= 1
        then res'
        else Invalid MoreThanOneTable
    where
        isValidTableType :: TableType -> ValidationResult
        isValidTableType (TableType (Limit min max) _) =
            if min <= fromMaybe min max
            then Valid
            else Invalid InvalidTableType

memoryShouldBeValid :: Validator
memoryShouldBeValid Module { imports, mems } =
    let memImports = filter isMemImport imports in
    let importsValid = foldMap (\Import { desc = ImportMemory l } -> isValidLimit l) memImports in
    let memsValid = foldMap (\(Memory l) -> isValidLimit l) mems in
    if length memImports + length mems <= 1
        then importsValid <> memsValid
        else Invalid MoreThanOneMemory

isValidLimit :: Limit -> ValidationResult
isValidLimit (Limit min max) =
  let minMax = if min <= fromMaybe min max then Valid else Invalid MinMoreThanMaxInMemoryLimit in
  let maxLim = if fromMaybe min max <= 65536 then Valid else Invalid MemoryLimitExceeded in
  minMax <> maxLim

globalsShouldBeValid :: Validator
globalsShouldBeValid m@Module { imports, globals } =
    let ctx = ctxFromModule [] [] Nothing m in
    foldMap (isGlobalValid ctx) globals
    where
        getGlobalType :: GlobalType -> ValueType
        getGlobalType (Const vt) = vt
        getGlobalType (Mut vt) = vt

        isGlobalValid :: Ctx -> Global -> ValidationResult
        isGlobalValid ctx (Global gt init) = runCheckerV ctx $ do
            isConstExpression init
            t <- getExpressionType init
            let expected = empty ==> getGlobalType gt
            if isArrowMatch expected t then pure Valid else throwError $ TypeMismatch t expected

elemsShouldBeValid :: Validator
elemsShouldBeValid m@Module { elems, functions, tables, imports } =
    let ctx = ctxFromModule [] [] Nothing m in
    foldMap (isElemValid ctx) elems
    where
        isElemValid :: Ctx -> ElemSegment -> ValidationResult
        isElemValid ctx (ElemSegment tableIdx offset funs) =
            let check = runCheckerV ctx $ do
                    isConstExpression offset
                    t <- getExpressionType offset
                    if isArrowMatch (empty ==> I32) t
                    then pure Valid
                    else throwError $ TypeMismatch t (empty ==> I32)
            in
            let tableImports = filter isTableImport imports in
            let isTableIndexValid =
                    if tableIdx < (fromIntegral $ length tableImports + length tables)
                    then Valid
                    else Invalid TableIndexOutOfRange
            in
            let funImports = filter isFuncImport imports in
            let funsLength = fromIntegral $ length functions + length funImports in
            let isFunsValid = foldMap (\i -> if i < funsLength then Valid else Invalid FunctionIndexOutOfRange) funs in
            check <> isFunsValid <> isTableIndexValid

datasShouldBeValid :: Validator
datasShouldBeValid m@Module { datas, mems, imports } =
    let ctx = ctxFromModule [] [] Nothing m in
    foldMap (isDataValid ctx) datas
    where
        isDataValid :: Ctx -> DataSegment -> ValidationResult
        isDataValid ctx (DataSegment memIdx offset _) =
            let check = runCheckerV ctx $ do
                    isConstExpression offset
                    t <- getExpressionType offset
                    if isArrowMatch (empty ==> I32) t
                    then pure Valid
                    else throwError $ TypeMismatch t (empty ==> I32)
            in
            let memImports = filter isMemImport imports in
            if memIdx < (fromIntegral $ length memImports + length mems)
            then check
            else Invalid MemoryIndexOutOfRange

startShouldBeValid :: Validator
startShouldBeValid Module { start = Nothing } = Valid
startShouldBeValid m@Module { start = Just (StartFunction idx) } =
    let types = getFuncTypes m in
    let i = fromIntegral idx in
    if length types > i
    then if FuncType [] [] == types !! i then Valid else Invalid InvalidStartFunctionType
    else Invalid FunctionIndexOutOfRange

exportsShouldBeValid :: Validator
exportsShouldBeValid Module { exports, imports, functions, mems, tables, globals } =
    areExportNamesUnique <> foldMap isExportValid exports
    where
        funcImports = filter isFuncImport imports
        tableImports = filter isTableImport imports
        memImports = filter isMemImport imports
        globalImports = filter isGlobalImport imports

        isExportValid :: Export -> ValidationResult
        isExportValid (Export _ (ExportFunc funIdx)) =
            if fromIntegral funIdx < length funcImports + length functions then Valid else Invalid FunctionIndexOutOfRange
        isExportValid (Export _ (ExportTable tableIdx)) =
            if fromIntegral tableIdx < length tableImports + length tables then Valid else Invalid TableIndexOutOfRange
        isExportValid (Export _ (ExportMemory memIdx)) =
            if fromIntegral memIdx < length memImports + length mems then Valid else Invalid MemoryIndexOutOfRange
        isExportValid (Export _ (ExportGlobal globalIdx)) =
            if fromIntegral globalIdx < length globalImports + length globals
            then (
                if fromIntegral globalIdx >= length globalImports
                then (
                    case globals !! (fromIntegral globalIdx - length globalImports) of
                        (Global (Mut _) _) -> Invalid ExportedGlobalIsNotConst
                        _ -> Valid
                )
                else Valid
            )
            else Invalid GlobalIndexOutOfRange

        areExportNamesUnique :: ValidationResult
        areExportNamesUnique =
            case foldl' go (Set.empty, []) exports of
                (_, []) -> Valid
                (_, dup) -> Invalid $ DuplicatedExportNames dup
            where
                go :: (Set.Set TL.Text, [String]) -> Export -> (Set.Set TL.Text, [String])
                go (set, dup) (Export name _) =
                    if Set.member name set
                    then (set, show name : dup)
                    else (Set.insert name set, dup)

importsShouldBeValid :: Validator
importsShouldBeValid Module { imports, types } =
    foldMap isImportValid imports
    where
        isImportValid :: Import -> ValidationResult
        isImportValid (Import _ _ (ImportFunc typeIdx)) =
            if fromIntegral typeIdx < length types
            then Valid
            else Invalid TypeIndexOutOfRange
        isImportValid (Import _ _ (ImportTable _)) = Valid -- checked in tables section
        isImportValid (Import _ _ (ImportMemory _)) = Valid -- checked in mems section
        isImportValid (Import _ _ (ImportGlobal (Const _))) = Valid
        isImportValid (Import _ _ (ImportGlobal (Mut _))) = Invalid ImportedGlobalIsNotConst

typesShouldBeValid :: Validator
typesShouldBeValid Module { types } = foldMap isTypeValid types
    where
        isTypeValid :: FuncType -> ValidationResult
        isTypeValid FuncType { results } = if length results <= 1 then Valid else Invalid InvalidResultArity

newtype ValidModule = ValidModule { getModule :: Module } deriving (Show, Eq)

validate :: Module -> Either ValidationError ValidModule
validate mod = const (ValidModule mod) <$> toEither (foldMap ($ mod) validators)
    where
        validators :: [Validator]
        validators = [
                typesShouldBeValid,
                functionsShouldBeValid,
                tablesShouldBeValid,
                memoryShouldBeValid,
                globalsShouldBeValid,
                elemsShouldBeValid,
                datasShouldBeValid,
                startShouldBeValid,
                exportsShouldBeValid,
                importsShouldBeValid
            ]
