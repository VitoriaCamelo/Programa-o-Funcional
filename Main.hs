-- recebe um comando e devolve a posição final
basico :: Command -> (Int, Int, Direcao, [Command]) 
basico (Forward n) = (0, n, Norte, [])               -- se comando foi Forward, incrementa y
basico (Backward n) = (0, -n, Norte, [])             -- se comando foi Backward, decrementa y
basico _ = (0, 0, Norte, [])                       -- se TurnLeft ou TurnRight, posição não muda

-- posicionar recebe posição atual, direção e lista de comandos 
-- e devolve posição final, direção e lista de comandos
posicionar :: (Int, Int, Direcao, [Command]) -> (Int, Int, Direcao, [Command])
-- caso base: sem comandos, permanecer na posição recebida
posicionar (x, y, direcao, []) = (x, y, direcao, []) 
-- se estiver olhando para o norte e for para a frente, incrementa y
posicionar (x, y, Norte, (Forward n):xs) = posicionar (x, y+n, Norte, xs)
-- se estiver olhando para o norte e for para trás, decrementa y
posicionar (x, y, Norte, (Backward n):xs) = posicionar (x, y-n, Norte, xs)
-- se estiver olhando para o sul e for para a frente, decrementa y
posicionar (x, y, Sul, (Forward n):xs) = posicionar (x, y-n, Sul, xs)
-- se estiver olhando para o sul e for para trás, incrementa y
posicionar (x, y, Sul, (Backward n):xs) = posicionar (x, y+n, Sul, xs)
-- se estiver olhando para o leste e for para a frente, incrementa x
posicionar (x, y, Leste, (Forward n):xs) = posicionar (x+n, y, Leste, xs)
-- se estiver olhando para o leste e for para trás, decrementa x
posicionar (x, y, Leste, (Backward n):xs) = posicionar (x-n, y, Leste, xs)
-- se estiver olhando para o oeste e for para a frente, decrementa x
posicionar (x, y, Oeste, (Forward n):xs) = posicionar (x-n, y, Oeste, xs)
-- se estiver olhando para o oeste e for para trás, incrementa x
posicionar (x, y, Oeste, (Backward n):xs) = posicionar (x+n, y, Oeste, xs)

-- Ex.: Norte + TurnLeft = Oeste
posicionar (x, y, Norte, (TurnLeft):xs) = posicionar (x, y, Oeste, xs)
posicionar (x, y, Norte, (TurnRight):xs) = posicionar (x, y, Leste, xs)
posicionar (x, y, Sul, (TurnLeft):xs) = posicionar (x, y, Leste, xs)
posicionar (x, y, Sul, (TurnRight):xs) = posicionar (x, y, Oeste, xs)
posicionar (x, y, Leste, (TurnLeft):xs) = posicionar (x, y, Norte, xs)
posicionar (x, y, Leste, (TurnRight):xs) = posicionar (x, y, Sul, xs)
posicionar (x, y, Oeste, (TurnLeft):xs) = posicionar (x, y, Sul, xs)
posicionar (x, y, Oeste, (TurnRight):xs) = posicionar (x, y, Norte, xs)

data Direcao = Norte | Sul | Leste | Oeste       -- para onde o robô pode estar olhando
  deriving (Show)
data Command = Forward Int | Backward Int | TurnLeft | TurnRight  -- comandos possíveis
  deriving (Show)
destination :: [Command] -> (Int, Int, Direcao, [Command]) 
destination [] = (0, 0, Norte, [])    -- caso base: sem comandos, permanecer na posição original
destination (x:[]) = basico x         -- caso básico: apenas um comando foi recebido
destination (x:xs) = posicionar (0, 0, Norte, (x:xs)) -- caso composto: chama função posicionar

main = do
  print(destination [])
