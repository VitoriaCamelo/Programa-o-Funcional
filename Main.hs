import Text.Printf
import Data.Char (ord)
import System.Exit (exitSuccess)

-- Definições Gerais --
arvore :: String
arvore = "#"
rua :: String
rua = " " 
escola :: String
escola = "@"
portal :: String
portal = "?"
alunoNorte :: String
alunoNorte = "^"
alunoSul :: String
alunoSul = "v"
alunoLeste :: String
alunoLeste = ">"
alunoOeste :: String
alunoOeste = "<"

-- Configurações dos Jogos --
montagem1 :: [[String]]
montagem1 = [[arvore, arvore, rua, portal], [arvore, arvore, rua, arvore], [arvore, arvore, rua, arvore], [arvore, alunoNorte, rua, arvore]]
aluno1 :: (Int, Int)
aluno1 = (1,0)
obstaculos1 :: [(Int, Int)]
obstaculos1 = [(0,0), (0,1), (0,2), (0,3), (1,1), (1,2), (1,3), (3,0), (3,1), (3,2), (1,3)]
alvo1 :: (Int, Int)
alvo1 = (3,3)

montagem2 :: [[String]]
montagem2 = [[arvore, portal, rua, arvore], [arvore, arvore, rua, arvore], [arvore, rua, rua, arvore], [alunoNorte, rua, arvore, arvore]]
aluno2 :: (Int, Int)
aluno2 = (0,0)
obstaculos2 :: [(Int, Int)]
obstaculos2 = [(0,1), (0,2), (0,3), (1,2), (1,3), (2,0), (3,0), (3,1), (3,2), (3,3)]
alvo2 :: (Int, Int)
alvo2 = (1,3)

montagem3 :: [[String]]
montagem3 = [[arvore, rua, rua, rua], [alunoNorte, rua, arvore, rua], [arvore, arvore, rua, rua], [escola, rua, rua, arvore]]
aluno3 :: (Int, Int)
aluno3 = (0,2)
obstaculos3 :: [(Int, Int)]
obstaculos3 = [(0,1), (0,3), (1,1), (2,2), (3,0)]
alvo3 :: (Int, Int)
alvo3 = (0,0)

-- Lógica de Montagem --
printLinha :: [String] -> IO ()
printLinha linha = printf "|%-3s%-3s%-3s%-3s|\n" (linha!!0) (linha!!1) (linha!!2) (linha!!3)

cenario :: [[String]] -> IO ()
cenario (x:y:z:t:_) = do
  printf " ____________\n"
  printLinha x
  printLinha y
  printLinha z
  printLinha t
  printf " ------------\n"

-- Lógica de Movimentação --
data Direcao = Norte | Sul | Leste | Oeste -- para onde o aluno pode estar olhando
  deriving (Eq, Show)
data Command = Forward Int | Backward Int | TurnLeft | TurnRight  | Stop 
  deriving (Eq, Show)

atribuiElemento :: Int -> (Int, Int) -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> String
atribuiElemento fase (l,c) direcao (x,y) obs alvo 
  | (c,l) == (x,y) =
    if direcao == Norte then
      alunoNorte
    else if direcao == Sul then
      alunoSul
    else if direcao == Leste then
      alunoLeste
    else 
      alunoOeste
  | (c,l) == alvo = 
    if fase == 0 then portal
    else escola
  | (c,l) `elem` obs = arvore
  | otherwise = rua

criarMatriz :: Int -> Int -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> [[String]]
criarMatriz fase dimensao direcao (x,y) obstaculos alvo  = [criarLinha dimensao linha 0 | linha <- reverse [0..dimensao-1]]
   where 
     criarLinha d l c
       | c == dimensao-1 = [atribuiElemento fase (l,c) direcao (x,y) obstaculos alvo]
       | otherwise = atribuiElemento fase (l,c) direcao (x,y) obstaculos alvo : criarLinha d l (c+1)

remontar :: Int -> Int -> (Int, (Int, Int), [[String]]) -> Int -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO (Int, (Int, Int), [[String]]) 
remontar fase mostrarBool (resposta, novaPosicao, montagemAntiga) dimensao direcao (x, y) obstaculos alvo 
  | resposta == 1 = printf "\n-> Movimento inválido\n" >> return (1, novaPosicao, montagemAntiga)
  | resposta == 2 = printf "\n-> Você completou mais uma fase\n" >> return (2, novaPosicao, [[]])
  | otherwise = do
      let novoCenario = criarMatriz fase dimensao direcao (x,y) obstaculos alvo
      printf "\n-> Sem colisão:\n"
      if mostrarBool == 1 then
        cenario novoCenario >> return (0, novaPosicao, novoCenario)
      else return (0, novaPosicao, novoCenario)

calcularPosicao :: Command -> Direcao -> (Int, Int) -> (Int, Int)
calcularPosicao (Forward _) direcao (x, y)  
 | direcao == Norte = (x, y+1)
 | direcao == Sul = (x, y-1)
 | direcao == Oeste = (x-1, y)
 | direcao == Leste = (x+1, y)
calcularPosicao (Backward _) direcao (x, y)  
 | direcao == Norte = (x, y-1)
 | direcao == Sul = (x, y+1)
 | direcao == Oeste = (x+1, y)
 | direcao == Leste = (x-1, y)

verificarColisao :: Int -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> Int
verificarColisao dimensao (x,y) [] alvo 
  | x == dimensao || y == dimensao = 1
  | x == -1 || y == -1 = 1
  | (x,y) == alvo = 2
  | otherwise = 0
verificarColisao dimensao (x,y) (primeiro:xs) alvo
  | x == dimensao || y == dimensao = 1
  | x == -1 || y == -1 = 1
  | xs == [] && primeiro /= (x,y) = 0
  | xs == [] && primeiro == (x,y) = 1
  | (x,y) == primeiro = 1
  | (x,y) == alvo = 2
  | otherwise = verificarColisao dimensao (x,y) xs alvo

trataComposto :: Int -> Int -> Command -> [[String]] -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO (Int, (Int, Int), [[String]])
trataComposto fase dimensao (Forward n) montagem direcao (x,y) obstaculos alvo =  repetir (x,y) 1 montagem 
  where 
    repetir (x,y) k montagem =
      if k<n then 
        let novaPosicao = calcularPosicao (Forward n) direcao (x, y) 
            retorno = verificarColisao dimensao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then 
            remontar fase 0 (1, (x,y), montagem) dimensao direcao (x,y) obstaculos alvo
          else if retorno == 2 then 
            remontar fase 0 (2, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo
          else 
            remontar fase 1 (0, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo 
            >> repetir novaPosicao (k+1) (criarMatriz fase dimensao direcao novaPosicao obstaculos alvo)
      else
        let novaPosicao = calcularPosicao (Forward n) direcao (x, y) 
            retorno = verificarColisao dimensao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then 
            remontar fase 0 (1, (x,y), montagem) dimensao direcao (x,y) obstaculos alvo
          else if retorno == 2 then 
            remontar fase 0 (2, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo
          else remontar fase 0 (0, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo

trataComposto fase dimensao (Backward n) montagem direcao (x,y) obstaculos alvo =  repetir (x,y) 1 montagem 
  where 
    repetir (x,y) k montagem =
      if k<n then 
        let novaPosicao = calcularPosicao (Backward n) direcao (x, y) 
            retorno = verificarColisao dimensao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then 
            remontar fase 0 (1, (x,y), montagem) dimensao direcao (x,y) obstaculos alvo
          else if retorno == 2 then 
            remontar fase 0 (2, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo
          else 
            remontar fase 1 (0, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo 
            >> repetir novaPosicao (k+1) (criarMatriz fase dimensao direcao novaPosicao obstaculos alvo)
      else
        let novaPosicao = calcularPosicao (Backward n) direcao (x, y) 
            retorno = verificarColisao dimensao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then 
            remontar fase 0 (1, (x,y), montagem) dimensao direcao (x,y) obstaculos alvo
          else if retorno == 2 then 
            remontar fase 0 (2, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo
          else remontar fase 0 (0, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo

verificaDirecao :: Direcao -> Command -> Direcao
verificaDirecao Norte TurnLeft = Oeste
verificaDirecao Norte TurnRight = Leste
verificaDirecao Sul TurnLeft = Leste
verificaDirecao Sul TurnRight = Oeste
verificaDirecao Leste TurnLeft = Norte
verificaDirecao Leste TurnRight = Sul
verificaDirecao Oeste TurnLeft = Sul
verificaDirecao Oeste TurnRight = Norte
  
trataSimples :: Command -> [[String]] -> [[String]]
trataSimples TurnLeft montagem = 
   map (map (\x -> 
    if x == alunoNorte then alunoOeste 
    else if x == alunoSul then alunoLeste 
    else if x == alunoLeste then alunoNorte 
    else if x == alunoOeste then alunoSul 
    else x)) montagem
trataSimples TurnRight montagem = 
   map (map (\x -> 
     if x == alunoNorte then alunoLeste 
     else if x == alunoSul then alunoOeste 
     else if x == alunoLeste then alunoSul 
     else if x == alunoOeste then alunoNorte 
     else x )) montagem

-- Fluxo do Jogo --
menu :: IO Command
menu = do
  putStrLn "\nOpções:"
  putStrLn "\n[1] Para Frente \t\t[2] Para Trás"
  putStrLn "\n[3] Virar à Esquerda \t[4] Virar à Direita"
  putStrLn "\n[5] Parar"
  putStrLn "\nEscolha um comando:"
  comandoStr <- getLine                
  let comando = read comandoStr :: Int
  if comando == 1 || comando == 2 then do
    putStrLn "\nQuantos passos?"
    passosStr <- getLine
    let passos = read passosStr :: Int
    if comando == 1 then 
      return (Forward passos)
    else 
      return (Backward passos)
  else if comando == 3 then 
    return TurnLeft
  else if comando == 4 then
    return TurnRight
  else
    return Stop

verificaFrenteTras :: Command -> Bool
verificaFrenteTras (Forward _) = True
verificaFrenteTras (Backward _) = True
verificaFrenteTras _ = False

jogo :: Int -> Int -> [[String]] -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO()
jogo fase dimensao montagem direcao aluno obstaculos alvo = do
  cenario montagem
  comando <- menu
  if comando == Stop then do
    putStrLn "\nAté a próxima!"
    exitSuccess
  else if verificaFrenteTras comando then do
    (resposta, alunoNovo, cenarioNovo) <- trataComposto fase dimensao comando montagem direcao aluno obstaculos alvo
    if resposta == 1 then do
       jogo fase dimensao cenarioNovo direcao alunoNovo obstaculos alvo
    else if resposta == 2 then do return()
    else do jogo fase dimensao cenarioNovo direcao alunoNovo obstaculos alvo
  else do
    let montagemAtualizada = trataSimples comando montagem
        direcaoAtualizada = verificaDirecao direcao comando
    jogo fase dimensao montagemAtualizada direcaoAtualizada aluno obstaculos alvo 

-- Fluxo Principal --
main = do
  putStrLn "-- Seja bem-vinda(o) ao jogo Chegando ao CI! --"
  putStrLn "\nVocê precisa chegar ao portal (elemento '?') de cada etapa até alcançar o CI (elemento '@') na última fase."
  putStrLn "\nVocê pode assumir 4 direções: Norte (^), Sul (v), Leste (>) e Oeste (<)."
  putStrLn "\nEvite as árvores: #"
  putStrLn "\nFase 1: Apenas começando"
  jogo 0 4 montagem1 Norte aluno1 obstaculos1 alvo1
  putStrLn "\nFase 2: Algumas voltas"
  jogo 0 4 montagem2 Norte aluno2 obstaculos2 alvo2
  putStrLn "\nFase 3: Quase lá"
  jogo 1 4 montagem3 Norte aluno3 obstaculos3 alvo3
  putStrLn "\n Você finalmente chegou ao CI" 

-- Doodle --
trataComando :: String -> Int -> Int -> [[String]] -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO()
trataComando [] fase dimensao montagem direcao aluno obstaculos alvo = doodle fase dimensao montagem direcao aluno obstaculos alvo
trataComando (comando:comandos) fase dimensao montagem direcao aluno obstaculos alvo = do 
  let comandoInt = ord comando - ord '0'
  let comandoTratado = if comandoInt == 1 then Forward 1 else if comandoInt == 2 then Backward 1 else if comandoInt == 3 then TurnLeft else if comandoInt == 4 then TurnRight else Stop
  if comandoTratado == Stop then do
    putStrLn "\nAté a próxima!"
    exitSuccess
  else if verificaFrenteTras comandoTratado then do
    (resposta, alunoNovo, cenarioNovo) <- trataComposto fase dimensao comandoTratado montagem direcao aluno obstaculos alvo
    if resposta == 2 then do return()
    else do cenario cenarioNovo >> trataComando comandos fase dimensao cenarioNovo direcao alunoNovo obstaculos alvo
  else 
    let montagemAtualizada = trataSimples comandoTratado montagem
        direcaoAtualizada = verificaDirecao direcao comandoTratado
    in 
      cenario montagemAtualizada
      >> trataComando comandos fase dimensao montagemAtualizada direcaoAtualizada aluno obstaculos alvo 

doodle :: Int -> Int -> [[String]] -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO()
doodle fase dimensao montagem direcao aluno obstaculos alvo = do
  --cenario montagem
  putStrLn "\nOpções:"
  putStrLn "\n[1] Para Frente \t\t[2] Para Trás"
  putStrLn "\n[3] Virar à Esquerda \t[4] Virar à Direita"
  putStrLn "\n[5] Parar"
  putStrLn "\nQual a sequência de execução que você deseja?"
  comando <- getLine                
  trataComando comando fase dimensao montagem direcao aluno obstaculos alvo

main2 = do
  putStrLn "-- Seja bem-vinda(o) ao jogo Chegando ao CI! --"
  putStrLn "\nVocê precisa chegar ao portal (elemento '?') de cada etapa até alcançar o CI (elemento '@') na última fase."
  putStrLn "\nVocê pode assumir 4 direções: Norte (^), Sul (v), Leste (>) e Oeste (<)."
  putStrLn "\nEvite as árvores: #"
  putStrLn "\nFase 1: Apenas começando"
  cenario montagem1
  doodle 0 4 montagem1 Norte aluno1 obstaculos1 alvo1
  putStrLn "\nFase 2: Algumas voltas"
  cenario montagem2
  doodle 0 4 montagem2 Norte aluno2 obstaculos2 alvo2
  putStrLn "\nFase 3: Quase lá"
  cenario montagem3
  doodle 1 4 montagem3 Norte aluno3 obstaculos3 alvo3
  putStrLn "\n Você finalmente chegou ao CI" 
