{- |
  Módulo: Update
  Tarefa 3 – Atualizar informação após cada jogo

  Responsável por:
    - Atualizar as estatísticas (Ganhos, Perdidos, AVE) dos jogadores num Torneio AVE.
    - Atualizar o vencedor num jogo emparelhado de Eliminação Direta.
-}

module Update
  ( updateAVE
  , updateElim
  ) where

import Fileread
  ( Jogador
  , TorneioAVE
  , ResultadoAVE
  , ResultadosAVE
  , TorneioElim
  , ResultadoElim
  , ResultadosElim
  )

-----------------------------------------------------------
-- Funções Auxiliares (AVE)
-----------------------------------------------------------

-- | Calcula o AVE (Average) de um jogador.
-- Devolve 0.0 se o total de jogos (ganhos + perdidos) for zero,
-- prevenindo a divisão por zero.
--
-- Recebe:
--   * g (Int) - Número total de ganhos (frames/partidas).
--   * p (Int) - Número total de perdidos (frames/partidas).
-- Devolve:
--   * Double - O valor do AVE (ganhos / total de jogos).
calcAve :: Int -> Int -> Double
calcAve g p =
  let total = g + p
  in if total == 0
       then 0.0
       else fromIntegral g / fromIntegral total

-- | Atualiza os registos de um jogador específico com base no resultado de um jogo AVE.
--
-- Recebe:
--   * res (ResultadoAVE) - O resultado do jogo (J1, J2, Score1, Score2).
--   * jogador (Jogador)  - O registo atual do jogador a ser verificado.
-- Devolve:
--   * Jogador - O registo do jogador com os Ganhos, Perdidos e AVE atualizados,
--               ou o registo original se não for um dos jogadores no resultado.
updateJogadorAVE :: ResultadoAVE -> Jogador -> Jogador
updateJogadorAVE (j1, j2, s1, s2) (nome, ganhos, perdidos, aveAntiga)
  | nome == j1 =
      let ganhos'   = ganhos   + s1
          perdidos' = perdidos + s2
      in (nome, ganhos', perdidos', calcAve ganhos' perdidos')
  | nome == j2 =
      let ganhos'   = ganhos   + s2
          perdidos' = perdidos + s1
      in (nome, ganhos', perdidos', calcAve ganhos' perdidos')
  | otherwise = (nome, ganhos, perdidos, aveAntiga)

-----------------------------------------------------------
-- 1. Atualização para torneios AVE
-----------------------------------------------------------

-- | updateAVE
-- Recebe o resultado de um jogo AVE, o torneio atual e os resultados passados,
-- devolvendo as estruturas de dados atualizadas.
--
-- Recebe:
--   * res (ResultadoAVE) - Resultado do jogo.
--   * torneio (TorneioAVE) - A estrutura do torneio.
--   * resultadosAntigos (ResultadosAVE) - A lista de resultados até agora.
-- Devolve:
--   * (TorneioAVE, ResultadosAVE) - Torneio com jogadores atualizados e lista de resultados com o novo jogo adicionado.
updateAVE :: ResultadoAVE -> TorneioAVE -> ResultadosAVE -> (TorneioAVE, ResultadosAVE)
updateAVE res@(j1, j2, s1, s2) (nomeT, nRondas, jogadores) resultadosAntigos =
  let -- Aplica a atualização a todos os jogadores
      jogadoresAtualizados = map (updateJogadorAVE res) jogadores
      -- Adiciona o novo resultado à lista histórica
      resultadosNovos      = resultadosAntigos ++ [res]
      torneioAtualizado    = (nomeT, nRondas, jogadoresAtualizados)
  in (torneioAtualizado, resultadosNovos)

-----------------------------------------------------------
-- 2. Atualização para torneios de Eliminação Direta
-----------------------------------------------------------

-- | updateElim
-- Recebe o resultado de um jogo de eliminação direta e atualiza o vencedor na lista
-- de ResultadosElim. O TorneioElim não é alterado.
--
-- Recebe:
--   * (eqARes, eqBRes, vencedorNovo) (ResultadoElim) - O jogo com o vencedor.
--   * torneio (TorneioElim) - A estrutura do torneio (inalterada).
--   * resultadosAntigos (ResultadosElim) - A lista de jogos emparelhados.
-- Devolve:
--   * (TorneioElim, ResultadosElim) - Torneio inalterado e lista de jogos com o vencedor atualizado.
updateElim :: ResultadoElim -> TorneioElim -> ResultadosElim -> (TorneioElim, ResultadosElim)
updateElim (eqARes, eqBRes, vencedorNovo) torneio resultadosAntigos =
  let -- Função auxiliar que encontra o jogo correspondente e substitui o vencedor
      atualizaJogo (a, b, vencedorAntigo)
        | a == eqARes && b == eqBRes = (a, b, vencedorNovo)
        | otherwise                  = (a, b, vencedorAntigo)

      -- Aplica a atualização para encontrar e corrigir o vencedor na lista
      resultadosNovos = map atualizaJogo resultadosAntigos
  in (torneio, resultadosNovos)