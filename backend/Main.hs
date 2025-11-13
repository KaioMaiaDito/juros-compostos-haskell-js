-- ============================================================
--  Calculadora de Juros Compostos em Haskell
--  Sem nenhum import, usando apenas o Prelude.
--  Estilo igual ao das suas listas: funcoes puras + main com IO.
-- ============================================================

-- Funções base (mesma ideia da soma/dobro etc., agora em Double)

soma :: Double -> Double -> Double
soma x y = x + y

multiplica :: Double -> Double -> Double
multiplica x y = x * y

-- Juros compostos:
-- Montante M = P * (1 + i) ^ n
-- Aqui n é Double para evitar conversão com fromIntegral.
jurosCompostos :: Double -> Double -> Double -> Double
jurosCompostos principal taxa periodos =
  let umMaisTaxa = soma 1.0 taxa
  in multiplica principal (umMaisTaxa ** periodos)

-- Calcula também os juros totais (montante - principal)
calculaJurosTotais :: Double -> Double -> Double -> Double
calculaJurosTotais principal taxa periodos =
  let montante = jurosCompostos principal taxa periodos
  in montante - principal

-- Gera uma String em formato JSON com o resultado
-- Exemplo:
-- {"futureValue": 3138.42, "totalInterest": 2138.42}
geraJsonResposta :: Double -> Double -> Double -> String
geraJsonResposta principal taxa periodos =
  let montante     = jurosCompostos principal taxa periodos
      jurosTotais  = calculaJurosTotais principal taxa periodos
      parteFV      = "\"futureValue\": " ++ show montante
      parteJuros   = "\"totalInterest\": " ++ show jurosTotais
  in "{" ++ parteFV ++ ", " ++ parteJuros ++ "}"

-- (Opcional) Gera um JSON de "request" só para mostrar na tela
geraJsonRequisicao :: Double -> Double -> Double -> String
geraJsonRequisicao principal taxa periodos =
  let p = "\"principal\": " ++ show principal
      r = "\"rate\": "      ++ show taxa
      n = "\"periods\": "   ++ show periodos
  in "{" ++ p ++ ", " ++ r ++ ", " ++ n ++ "}"

-- Programa principal
-- Aqui, em vez de decodificar o JSON, nós assumimos que
-- alguém (por exemplo o JavaScript) já transformou o JSON
-- em números e só estamos lendo esses números.
main :: IO ()
main = do
  putStrLn "== Calculadora de Juros Compostos (Haskell) =="
  putStrLn "Digite o valor principal:"
  principal <- readLn :: IO Double

  putStrLn "Digite a taxa por periodo (ex.: 0.1 para 10%):"
  taxa <- readLn :: IO Double

  putStrLn "Digite o numero de periodos:"
  periodos <- readLn :: IO Double

  -- Mostra o JSON que REPRESENTA a requisição
  putStrLn "JSON de requisicao (simulado):"
  let jsonReq = geraJsonRequisicao principal taxa periodos
  putStrLn jsonReq

  -- Mostra o JSON de resposta (que seria devolvido ao frontend)
  putStrLn "JSON de resposta:"
  let jsonResp = geraJsonResposta principal taxa periodos
  putStrLn jsonResp
