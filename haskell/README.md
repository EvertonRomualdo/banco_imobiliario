# 🏠 Banco Imobiliário Terminal Edition

> Um jogo de tabuleiro clássico implementado em Haskell com interface em terminal, suporte a bots e ranking persistente.

---

## 🎯 Objetivo do Projeto

O objetivo principal é simular a experiência de jogar Banco Imobiliário com:

- Regras da versão tradicional e variações populares entre jogadores  
- Interface totalmente em terminal  
- Estatísticas salvas em arquivos `.txt`  
- Ranking entre jogadores baseado em desempenho  
- Suporte a bots com comportamento semi-inteligente  

---

## 📦 O que o projeto contém

- 🎲 Simulação completa do jogo com movimentação, sorte de dados, compra e venda de propriedades, pagamento de aluguel, prisão, impostos e construções  
- 👥 Suporte a até 4 jogadores (misturando humanos e bots)  
- 💼 Estrutura de banco que aplica as regras, administra turnos e processa ações  
- 🏗️ Sistema de construções com casas e hotéis  
- 🧠 Algoritmo de bots com decisões baseadas em aleatoriedade e heurísticas  
- 📊 Sistema de ranking persistente com estatísticas como:
  - Nome do jogador  
  - Número de vitórias e derrotas  
  - Saldo total acumulado  
  - Quantidade de propriedades no fim das partidas  
- 🖥️ Interface interativa em terminal, com menus, exibição do tabuleiro e ações por turno

---

## 📁 Estrutura de Pastas

```plaintext
banco-imobiliario/
│
├── app/
│   └── Main.hs                        -- Ponto de entrada do jogo
│
├── src/
│   ├── Game/
│   │   ├── Tabuleiro.hs              -- Definição das casas, movimentação, ações de casa
│   │   ├── Jogador.hs                -- Estrutura e lógica dos jogadores
│   │   ├── Casa.hs                   -- Tipos de casa, valores, construções
│   │   ├── Banco.hs                  -- Regras do banco, transações, validações
│   │   ├── Dado.hs                   -- Função para rolar o dado
│   │   ├── Bot.hs                    -- Lógica para comportamento dos bots
│   │   └── Turno.hs                  -- Alternância de turno, lógica principal do jogo
│
│   ├── Interface/
│   │   ├── Terminal.hs               -- Impressão do tabuleiro, menu, status do jogo
│   │   └── Input.hs                  -- Leitura e validação de entrada do usuário
│
│   ├── Ranking/
│   │   ├── Estatisticas.hs           -- Atualização de vitórias, derrotas, propriedades
│   │   └── Persistencia.hs           -- Leitura/gravação em arquivos .txt
│
│   └── Utils/
│       └── Helpers.hs                -- Funções auxiliares (formatar texto, etc.)
│
├── data/
│   ├── ranking.txt                   -- Ranking persistido
│   └── jogadores/                    -- Arquivos .txt com estatísticas individuais
│       └── jogador1.txt              -- Exemplo
│
├── test/
│   └── ...                           -- Testes unitários (opcional)
│
├── stack.yaml / cabal.project        -- Configuração do projeto Haskell
├── banco-imobiliario.cabal           -- Declaração do executável, módulos etc.
└── README.md                         -- Instruções e visão geral do projeto

## 🛠 Tecnologias e Ferramentas

- **Linguagem:** Haskell  
- **Execução:** Terminal (modo texto)  
- **Persistência:** Arquivos `.txt`  
- **Arquitetura:** Organização modular com separação entre lógica do jogo, interface e persistência  
