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

-- Configurações dos Jogos --
montagem1 :: [[String]]
montagem1 = [[arvore, arvore, rua, escola], [arvore, arvore, rua, arvore], [arvore, arvore, rua, arvore], [arvore, alunoNorte, rua, arvore]]
aluno1 :: (Int, Int)
aluno1 = (1,0)
obstaculos1 :: [(Int, Int)]
obstaculos1 = [(0,0), (2,0), (3,0), (0,1), (1,1), (3,1), (0,2), (1,2), (3,2), (0,3), (1,3)]
alvo1 :: (Int, Int)
alvo1 = (3,3)

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

remontar :: (Int, (Int, Int)) -> Int -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO (Int, (Int, Int)) 
remontar (resposta, novaPosicao) dimensao direcao (x, y) obstaculos alvo 
  | resposta == 1 = printf "Movimento inválido\n" >> return (1, novaPosicao)
  | resposta == 2 = printf "Parabéns, você chegou ao CI\n" >> return (2, novaPosicao)
  | otherwise = printf "Sem colisão\n" >> cenario (criarMatriz dimensao direcao (x,y) obstaculos alvo) >> return (0, novaPosicao)

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

trataComposto :: Int -> Command -> [[String]] -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO (Int, (Int, Int))
trataComposto dimensao (Forward n) montagem direcao (x,y) obstaculos alvo =  repetir (x,y) 1
  where 
    repetir (x,y) k =
      if k<n then 
        let novaPosicao = calcularPosicao (Forward n) direcao (x, y) 
            retorno = verificarColisao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then 
            remontar (1, (x,y)) dimensao direcao novaPosicao obstaculos alvo
          else if retorno == 2 then 
            remontar (2, novaPosicao) dimensao direcao novaPosicao obstaculos alvo
          else 
            remontar (0, novaPosicao) dimensao direcao novaPosicao obstaculos alvo >> repetir novaPosicao (k+1)
      else
        let novaPosicao = calcularPosicao (Forward n) direcao (x, y) 
            retorno = verificarColisao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then 
            remontar (1, (x,y)) dimensao direcao novaPosicao obstaculos alvo
          else if retorno == 2 then 
            remontar (2, novaPosicao) dimensao direcao novaPosicao obstaculos alvo
          else remontar (0, novaPosicao) dimensao direcao novaPosicao obstaculos alvo
trataComposto dimensao (Backward n) montagem direcao (x,y) obstaculos alvo =  repetir (x,y) 1
  where 
    repetir (x,y) k =
      if k<n then 
        let novaPosicao = calcularPosicao (Backward n) direcao (x, y) 
            retorno = verificarColisao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then 
            remontar (1, (x,y)) dimensao direcao novaPosicao obstaculos alvo
          else if retorno == 2 then 
            remontar (2, novaPosicao) dimensao direcao novaPosicao obstaculos alvo
          else 
            remontar (0, novaPosicao) dimensao direcao novaPosicao obstaculos alvo >> repetir novaPosicao (k+1)
      else
        let novaPosicao = calcularPosicao (Backward n) direcao (x, y) 
            retorno = verificarColisao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then 
            remontar (1, (x,y)) dimensao direcao novaPosicao obstaculos alvo
          else if retorno == 2 then 
            remontar (2, novaPosicao) dimensao direcao novaPosicao obstaculos alvo
          else remontar (0, novaPosicao) dimensao direcao novaPosicao obstaculos alvo


verificaDirecao :: [[String]] -> Direcao
verificaDirecao ((x:xs):[]) 
  | x == alunoNorte = Norte
  | x == alunoSul = Sul
  | x == alunoLeste = Leste
  | x == alunoOeste = Oeste
  | otherwise = verificaDirecao (xs:[])
verificaDirecao ([]:xs) = verificaDirecao xs
verificaDirecao ((x:xs):xss)
  | x == alunoNorte = Norte
  | x == alunoSul = Sul
  | x == alunoLeste = Leste
  | x == alunoOeste = Oeste
  | otherwise = verificaDirecao (xs:xss)
  
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
    (resposta, aluno) <- trataComposto dimensao comando montagem direcao aluno obstaculos alvo
    if resposta == 1 then do
       jogo dimensao montagem direcao aluno obstaculos alvo
    else if resposta == 2 then do return()
    else do jogo dimensao montagem direcao aluno obstaculos alvo
  else do
    let montagem = trataSimples comando montagem
    print "oi"
    let direcao = verificaDirecao montagem
    jogo dimensao montagem direcao aluno obstaculos alvo 

-- Fluxo Principal --
main = do
  putStrLn "-- Seja bem-vinda(o) ao jogo Chegando ao CI! --"
  putStrLn "\nFase 1:"
  jogo 4 montagem1 Norte aluno1 obstaculos1 alvo1
  putStrLn "\nFase 2:"
  -- completar
