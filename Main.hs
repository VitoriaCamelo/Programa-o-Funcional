import Text.Printf

data Direcao = Norte | Sul | Leste | Oeste -- para onde o robô pode estar olhando
  deriving (Show)
data Command = Forward Int | Backward Int | TurnLeft | TurnRight  | Stop -- comandos possíveis
  deriving (Eq, Show)

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

montagem1 :: [[String]]
montagem1 = [[arvore, arvore, rua, escola], [arvore, arvore, rua, arvore], [arvore, arvore, rua, arvore], [arvore, alunoNorte, rua, arvore]]

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

trataComando :: Command -> [[String]] -> [[String]]
trataComando TurnLeft montagem = trataSimples TurnLeft montagem
trataComando TurnRight montagem = trataSimples TurnRight montagem


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

jogo :: [[String]] -> IO()
jogo montagem = do
  cenario montagem
  comando <- menu
  if comando == Stop then do
    putStrLn "\nAté a próxima!"
  else do
    jogo (trataComando comando montagem)
  
main = do
  putStrLn "-- Seja bem-vinda(o) ao jogo Chegando ao CI! --"
  putStrLn "\nFase 1:"
  jogo montagem1
