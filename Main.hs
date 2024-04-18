import Text.Printf

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
obstaculos1 = [(0,0), (0,1), (0,2), (0,3), (1,1), (1,2), (1,3), (3,1), (3,2)]
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

-- atribuiElemento :: (Int, Int) -> [(Int, Int)] -> (Int, Int) -> [[String]] 

remontar :: Int -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO() -- chamar cenario
remontar dimensao direcao (x, y) obstaculos (alvoX, alvoY) = criarLinha 1 1
  where 
    criarLinha l c 
      | c == dimensao = (atribuiElemento (l,c) (x,y) obstaculos alvo):[criarLinha l+1 1] 
      | l == dimensao = (criarLinha l 1):[]
      | otherwise = (atribuiElemento (l,c) (x,y) obstaculos alvo):criarLinha l c+1

verificaDirecao :: [[String]] -> Direcao 
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

verificarColisao :: (Int, Int) -> [(Int, Int)] -> (Int, Int) -> Int -- chamar funcao de montagem
verificarColisao (x,y) [] alvo 
  | (x,y) == alvo = 2
  | otherwise = 0
verificarColisao (x,y) (primeiro:xs) alvo 
  | xs == [] && primeiro /= (x,y) = 0
  | xs == [] && primeiro == (x,y) = 1
  | (x,y) == primeiro = 1
  | (x,y) == alvo = 2
  | otherwise = verificarColisao (x,y) xs alvo
  
trataComposto :: Command -> [[String]] -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> (Int, (Int, Int))
trataComposto (Forward n) montagem direcao (x,y) obstaculos alvo =  repetir (x,y) 1
  where 
    repetir (x,y) k =
      if k<n then 
        let novaPosicao = calcularPosicao (Forward n) direcao (x, y) 
            retorno = verificarColisao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then (1, novaPosicao)
          else if retorno == 2 then (2, novaPosicao)
          else repetir novaPosicao (k+1)
      else
        let novaPosicao = calcularPosicao (Forward n) direcao (x, y) 
            retorno = verificarColisao novaPosicao obstaculos alvo 
        in
          if retorno == 1 then (1, novaPosicao)
          else if retorno == 2 then (2, novaPosicao) 
          else (0, novaPosicao)
          
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

jogo :: [[String]] -> Direcao -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO()
jogo montagem direcao aluno obstaculos alvo = do
  cenario montagem
  comando <- menu
  if comando == Stop then do
    putStrLn "\nAté a próxima!"
  else if verificaFrenteTras comando then do
    let (resposta, aluno) = trataComposto comando montagem direcao aluno obstaculos alvo
    if resposta == 1 then do
       print "Movimento inválido"
    else if resposta == 2 then do return()
    else do jogo montagem direcao aluno obstaculos alvo
  else do
    let montagem = trataSimples comando montagem
    let direcao = verificaDirecao montagem
    jogo montagem direcao aluno obstaculos alvo 

-- Fluxo Principal --
main = do
  putStrLn "-- Seja bem-vinda(o) ao jogo Chegando ao CI! --"
  putStrLn "\nFase 1:"
  jogo montagem1 Norte aluno1 obstaculos1 alvo1
  putStrLn "\nFase 2:"
  -- completar
