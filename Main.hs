import Text.Printf
import System.Exit (exitSuccess)

-- Definições Gerais --
arvore :: String
arvore = "#"
rua :: String
rua = " " 
escola :: String
escola = "@"
alunoNorte :: String
alunoNorte = "^"
alunoSul :: String
alunoSul = "v"
alunoLeste :: String
alunoLeste = ">"
alunoOeste :: String
alunoOeste = "<"
-- ideia: criar o portal

-- Configurações dos Jogos --
montagem1 :: [[String]]
montagem1 = [[arvore, arvore, rua, escola], [arvore, arvore, rua, arvore], [arvore, arvore, rua, arvore], [arvore, alunoNorte, rua, arvore]]
aluno1 :: (Int, Int)
aluno1 = (1,0)
obstaculos1 :: [(Int, Int)]
obstaculos1 = [(0,0), (0,1), (0,2), (0,3), (1,1), (1,2), (1,3), (3,0), (3,1), (3,2), (1,3)]
alvo1 :: (Int, Int)
alvo1 = (3,3)

montagem2 :: [[String]]
montagem2 = [[arvore, escola, rua, arvore], [arvore, arvore, rua, arvore], [arvore, rua, rua, arvore], [alunoNorte, rua, arvore, arvore]]
aluno2 :: (Int, Int)
aluno2 = (0,0)
obstaculos2 :: [(Int, Int)]
obstaculos2 = [(0,1), (0,2), (0,3), (1,2), (1,3), (2,0), (3,0), (3,1), (3,2), (3,3)]
alvo2 :: (Int, Int)
alvo2 = (1,3)

montagem3 :: [[String]]
montagem3 = [[arvore, escola, rua, arvore], [arvore, arvore, rua, arvore], [arvore, rua, rua, arvore], [alunoNorte, rua, arvore, arvore]]
aluno3 :: (Int, Int)
aluno3 = (0,0)
obstaculos3 :: [(Int, Int)]
obstaculos3 = [(0,1), (0,2), (0,3), (1,2), (1,3), (2,0), (3,0), (3,1), (3,2), (3,3)]
alvo3 :: (Int, Int)
alvo3 = (1,3)

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
  printf " ____________\n"

-- Lógica de Movimentação --
data Direcao = Norte | Sul | Leste | Oeste -- para onde o aluno pode estar olhando
  deriving (Eq, Show)
data Command = Forward Int | Backward Int | TurnLeft | TurnRight  | Stop 
  deriving (Eq, Show)

atribuiElemento :: (Int, Int) -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> String
atribuiElemento (l,c) direcao (x,y) obs alvo 
  | (c,l) == (x,y) =
    if direcao == Norte then
      alunoNorte
    else if direcao == Sul then
      alunoSul
    else if direcao == Leste then
      alunoLeste
    else 
      alunoOeste
  | (c,l) == alvo = escola
  | (c,l) `elem` obs = arvore
  | otherwise = rua

criarMatriz :: Int -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> [[String]]
criarMatriz dimensao direcao (x,y) obstaculos alvo  = [criarLinha dimensao linha 0 | linha <- reverse [0..dimensao-1]]
   where 
     criarLinha d l c
       | c == dimensao-1 = [atribuiElemento (l,c) direcao (x,y) obstaculos alvo]
       | otherwise = atribuiElemento (l,c) direcao (x,y) obstaculos alvo : criarLinha d l (c+1)

remontar :: (Int, (Int, Int), [[String]]) -> Int -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO (Int, (Int, Int), [[String]]) 
remontar (resposta, novaPosicao, montagemAntiga) dimensao direcao (x, y) obstaculos alvo 
  | resposta == 1 = printf "Movimento inválido\n" >> return (1, novaPosicao, montagemAntiga)
  | resposta == 2 = printf "Parabéns, você chegou ao CI\n" >> return (2, novaPosicao, [[]])
  | otherwise = do
      let novoCenario = criarMatriz dimensao direcao (x,y) obstaculos alvo
      printf "Sem colisão\n"
      cenario novoCenario
      return (0, novaPosicao, novoCenario)

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

verificarColisao :: (Int, Int) -> [(Int, Int)] -> (Int, Int) -> Int
verificarColisao (x,y) [] alvo 
  | (x,y) == alvo = 2
  | otherwise = 0
verificarColisao (x,y) (primeiro:xs) alvo 
  | xs == [] && primeiro /= (x,y) = 0
  | xs == [] && primeiro == (x,y) = 1
  | (x,y) == primeiro = 1
  | (x,y) == alvo = 2
  | otherwise = verificarColisao (x,y) xs alvo

-- tratar passo 1
trataComposto :: Int -> Command -> [[String]] -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO (Int, (Int, Int), [[String]])
trataComposto dimensao (Forward n) montagem direcao (x,y) obstaculos alvo =  repetir (x,y) 1 montagem 
  where 
    repetir (x,y) k montagem =
      if k<n then 
        let novaPosicao = calcularPosicao (Forward n) direcao (x, y) 
            retorno = verificarColisao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then 
            remontar (1, (x,y), montagem) dimensao direcao (x,y) obstaculos alvo
          else if retorno == 2 then 
            remontar (2, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo
          else 
            remontar (0, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo 
            >> repetir novaPosicao (k+1) (criarMatriz dimensao direcao novaPosicao obstaculos alvo)
      else
        let novaPosicao = calcularPosicao (Forward n) direcao (x, y) 
            retorno = verificarColisao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then 
            remontar (1, (x,y), montagem) dimensao direcao (x,y) obstaculos alvo
          else if retorno == 2 then 
            remontar (2, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo
          else remontar (0, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo

trataComposto dimensao (Backward n) montagem direcao (x,y) obstaculos alvo =  repetir (x,y) 1 montagem 
  where 
    repetir (x,y) k montagem =
      if k<n then 
        let novaPosicao = calcularPosicao (Backward n) direcao (x, y) 
            retorno = verificarColisao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then 
            remontar (1, (x,y), montagem) dimensao direcao (x,y) obstaculos alvo
          else if retorno == 2 then 
            remontar (2, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo
          else 
            remontar (0, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo 
            >> repetir novaPosicao (k+1) (criarMatriz dimensao direcao novaPosicao obstaculos alvo)
      else
        let novaPosicao = calcularPosicao (Backward n) direcao (x, y) 
            retorno = verificarColisao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then 
            remontar (1, (x,y), montagem) dimensao direcao (x,y) obstaculos alvo
          else if retorno == 2 then 
            remontar (2, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo
          else remontar (0, novaPosicao, montagem) dimensao direcao novaPosicao obstaculos alvo


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

jogo :: Int -> [[String]] -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO()
jogo dimensao montagem direcao aluno obstaculos alvo = do
  cenario montagem
  comando <- menu
  if comando == Stop then do
    putStrLn "\nAté a próxima!"
    exitSuccess
  else if verificaFrenteTras comando then do
    (resposta, alunoNovo, cenarioNovo) <- trataComposto dimensao comando montagem direcao aluno obstaculos alvo
    if resposta == 1 then do
       jogo dimensao cenarioNovo direcao alunoNovo obstaculos alvo
    else if resposta == 2 then do return()
    else do jogo dimensao cenarioNovo direcao alunoNovo obstaculos alvo
  else do
    let montagemAtualizada = trataSimples comando montagem
        direcaoAtualizada = verificaDirecao direcao comando
    jogo dimensao montagemAtualizada direcaoAtualizada aluno obstaculos alvo 

-- Fluxo Principal --
main = do
  putStrLn "-- Seja bem-vinda(o) ao jogo Chegando ao CI! --"
  putStrLn "\nFase 1: Apenas começando"
  jogo 4 montagem1 Norte aluno1 obstaculos1 alvo1
  putStrLn "\nFase 2: Algumas voltas"
  jogo 4 montagem2 Norte aluno2 obstaculos2 alvo2
  putStrLn "\nFase 3: Quase lá"
  jogo 4 montagem3 Norte aluno3 obstaculos3 alvo3
  putStrLn "\n " -- corrigir mensagem final
  -- completar
