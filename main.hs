module Main where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Time       (UTCTime, getCurrentTime)
import           System.Directory (doesFileExist)
import           System.IO       (hFlush, stdout)
import           Control.Exception (catch, IOException)
import           Text.Read       (readMaybe)
import           Data.Maybe      (mapMaybe)
import           Data.List       (foldl', foldl1')

------------------------------------------------------------
-- DADOS
------------------------------------------------------------

data Item = Item
  { itemID     :: String
  , nome       :: String
  , quantidade :: Int
  , categoria  :: String
  } deriving (Show, Read, Eq)

type Inventario = Map String Item

data AcaoLog
  = Add
  | Remove
  | Update
  | List
  | Report
  | QueryFail
  deriving (Show, Read, Eq)

data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read, Eq)

data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  } deriving (Show, Read, Eq)

type ResultadoOperacao = (Inventario, LogEntry)

mkLog :: UTCTime -> AcaoLog -> String -> StatusLog -> LogEntry
mkLog t ac det st = LogEntry t ac det st

------------------------------------------------------------
-- FUNÇÕES PURAS (Manipular Itens do Inventário)
------------------------------------------------------------

addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem t item inv =
  if M.member (itemID item) inv
    then Left "Item duplicado."
    else
      let inv' = M.insert (itemID item) item inv
          det  = "id=" ++ itemID item ++ "; qtd=" ++ show (quantidade item)
          logE = mkLog t Add det Sucesso
      in Right (inv', logE)

removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem t iid qtd inv
  | qtd <= 0 = Left "Quantidade invalida."
  | otherwise =
      case M.lookup iid inv of
        Nothing -> Left "Item nao encontrado."
        Just it ->
          if quantidade it < qtd
            then Left "Estoque insuficiente."
            else
              let novoItem = it { quantidade = quantidade it - qtd }
                  inv'     = M.insert iid novoItem inv
                  det      = "id=" ++ iid ++ "; delta=" ++ show (-qtd)
                  logE     = mkLog t Remove det Sucesso
              in Right (inv', logE)

updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty t iid novaQtd inv
  | novaQtd < 0 = Left "Quantidade nao pode ser negativa."
  | otherwise =
      case M.lookup iid inv of
        Nothing -> Left "Item nao encontrado."
        Just it ->
          let novoItem = it { quantidade = novaQtd }
              inv'     = M.insert iid novoItem inv
              det      = "id=" ++ iid ++ "; novaQtd=" ++ show novaQtd
              logE     = mkLog t Update det Sucesso
          in Right (inv', logE)

------------------------------------------------------------
-- FUNÇÕES PURAS (Análise de Logs)
------------------------------------------------------------

extrairId :: LogEntry -> Maybe String
extrairId le =
  let d = detalhes le
  in case d of
      ('i':'d':'=':resto) ->
        let ident = takeWhile (\c -> c /= ';' && c /= ' ') resto
        in if null ident then Nothing else Just ident
      _ -> Nothing

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem iid =
  filter (\le -> extrairId le == Just iid)

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro =
  filter (\le -> case status le of Falha _ -> True; _ -> False)

itemMaisMovimentado :: [LogEntry] -> Maybe String
itemMaisMovimentado logs =
  case M.toList contagem of
    [] -> Nothing
    xs -> Just (fst (foldl1' maxSnd xs))
  where
    ids = mapMaybe extrairId logs
    contagem = foldl' (\m x -> M.insertWith (+) x 1 m) M.empty ids
    maxSnd a b = if snd b > snd a then b else a

ultimasMovimentacoes :: Int -> [LogEntry] -> [LogEntry]
ultimasMovimentacoes n xs =
  let rev = reverse xs
  in reverse (take n rev)

------------------------------------------------------------
-- PERSISTÊNCIA (Inventario.dat / Auditoria.log)
------------------------------------------------------------

arquivoInventario :: FilePath
arquivoInventario = "Inventario.dat"

arquivoLog :: FilePath
arquivoLog = "Auditoria.log"

lerShowRead :: Read a => FilePath -> IO (Maybe a)
lerShowRead fp =
  catch (readMaybe <$> readFile fp) (\(_ :: IOException) -> return Nothing)

carregarInventario :: IO Inventario
carregarInventario = do
  mInv <- lerShowRead arquivoInventario
  case mInv of
    Just inv -> return inv
    Nothing  -> return M.empty

carregarLogs :: IO [LogEntry]
carregarLogs = do
  existe <- doesFileExist arquivoLog
  if not existe
    then return []
    else do
      conteudo <- readFile arquivoLog
      let linhas = lines conteudo
      return (mapMaybe readMaybe linhas)

salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile arquivoInventario (show inv)

appendLog :: LogEntry -> IO ()
appendLog le = appendFile arquivoLog (show le ++ "\n")

------------------------------------------------------------
-- LOOP PRINCIPAL (main IO ())
------------------------------------------------------------

perguntar :: String -> IO String
perguntar msg = putStr msg >> hFlush stdout >> getLine

mostrarMenu :: IO ()
mostrarMenu = do
  putStrLn "1. Adicionar item"
  putStrLn "2. Remover quantidade"
  putStrLn "3. Atualizar quantidade"
  putStrLn "4. Listar itens"
  putStrLn "5. Report"
  putStrLn "0. Sair"
  putStr   "Opcao: "
  hFlush stdout

loop :: Inventario -> IO ()
loop inv = do
  mostrarMenu
  opcao <- getLine
  case opcao of
    "1" -> opAdd inv
    "2" -> opRemove inv
    "3" -> opUpdate inv
    "4" -> opList inv
    "5" -> opReport inv
    "0" -> putStrLn "Encerrando."
    _   -> do
      now <- getCurrentTime
      appendLog (mkLog now QueryFail ("comando=" ++ opcao) (Falha "Opcao invalida"))
      putStrLn "Opcao invalida."
      loop inv

------------------------------------------------------------
-- OPERAÇÕES DE I/O (Chamam funções puras)
------------------------------------------------------------

opAdd :: Inventario -> IO ()
opAdd inv = do
  iid  <- perguntar "ID: "
  nm   <- perguntar "Nome: "
  qStr <- perguntar "Quantidade: "
  cat  <- perguntar "Categoria: "
  now  <- getCurrentTime

  case readMaybe qStr of
    Nothing -> do
      appendLog (mkLog now QueryFail ("id=" ++ iid) (Falha "Quantidade invalida"))
      putStrLn "Quantidade invalida."
      loop inv

    Just q -> do
      let item = Item iid nm q cat
      case addItem now item inv of
        Left e -> do
          appendLog (mkLog now Add ("id=" ++ iid ++ "; erro=" ++ e) (Falha e))
          putStrLn e
          loop inv
        Right (inv', logE) -> do
          salvarInventario inv'
          appendLog logE
          putStrLn "Item adicionado."
          loop inv'

opRemove :: Inventario -> IO ()
opRemove inv = do
  iid  <- perguntar "ID: "
  qStr <- perguntar "Qtd remover: "
  now  <- getCurrentTime

  case readMaybe qStr of
    Nothing -> do
      appendLog (mkLog now QueryFail ("id=" ++ iid) (Falha "Quantidade invalida"))
      putStrLn "Quantidade invalida."
      loop inv

    Just q -> do
      case removeItem now iid q inv of
        Left e -> do
          appendLog (mkLog now Remove ("id=" ++ iid ++ "; erro=" ++ e) (Falha e))
          putStrLn e
          loop inv
        Right (inv', logE) -> do
          salvarInventario inv'
          appendLog logE
          putStrLn "Remocao realizada."
          loop inv'

opUpdate :: Inventario -> IO ()
opUpdate inv = do
  iid  <- perguntar "ID: "
  qStr <- perguntar "Nova qtd: "
  now  <- getCurrentTime

  case readMaybe qStr of
    Nothing -> do
      appendLog (mkLog now QueryFail ("id=" ++ iid) (Falha "Quantidade invalida"))
      putStrLn "Quantidade invalida."
      loop inv

    Just q -> do
      case updateQty now iid q inv of
        Left e -> do
          appendLog (mkLog now Update ("id=" ++ iid ++ "; erro=" ++ e) (Falha e))
          putStrLn e
          loop inv
        Right (inv', logE) -> do
          salvarInventario inv'
          appendLog logE
          putStrLn "Quantidade atualizada."
          loop inv'

opList :: Inventario -> IO ()
opList inv = do
  now <- getCurrentTime
  appendLog (mkLog now List "" Sucesso)
  mapM_ print (M.elems inv)
  loop inv

opReport :: Inventario -> IO ()
opReport inv = do
  now <- getCurrentTime
  appendLog (mkLog now Report "" Sucesso)
  logs <- carregarLogs
  putStrLn "\nErros:"
  mapM_ print (logsDeErro logs)
  putStrLn "\nUltimas movimentacoes:"
  mapM_ print (ultimasMovimentacoes 20 logs)
  putStrLn "\nItem mais movimentado:"
  print (itemMaisMovimentado logs)
  loop inv

------------------------------------------------------------
-- MAIN
------------------------------------------------------------

main :: IO ()
main = do
  inv <- carregarInventario
  loop inv
