# 🏠 Banco Imobiliário Terminal Edition

Um jogo de tabuleiro clássico implementado em **Haskell** com interface de texto no terminal, suporte a ranking e funcionalidades completas para partidas locais.

---

## 🎯 Objetivo do Projeto

Recriar a experiência do jogo Banco Imobiliário no terminal com:

- 🎲 Regras tradicionais e variações populares  
- ⏱️ Sistema de turnos e movimentação com dados  
- 🏘️ Compra, venda e construção de casas/hotéis  
- 💰 Impostos, prisões e casas especiais  
- 🧾 Menu interativo e cadastro de jogadores  
- 🏆 Ranking persistente por jogador  

---

## 📆 Funcionalidades

- 🌐 Interface terminal interativa (menus, entrada do usuário)  
- 👥 Cadastro de jogadores (de 2 a 4 participantes)  
- 🔄 Turnos alternados, com opções durante o turno:  
  - 🎲 Rolar dado e andar  
  - 💳 Ver saldo  
  - 🏘️ Ver propriedades  
  - 📍 Ver casa atual  
  - 🔢 Ver rodada atual  
- 🏢 Construção de casas e hotéis  
- 💸 Compra de propriedades e pagamento de aluguel  
- 🛑 Prisão e bloqueios temporários  
- 🏅 Ranking com vitórias e derrotas por jogador (salvo em arquivo)  

---

## 📦 Estrutura de Pastas

```text
banco-imobiliario/
├── app/
│   └── Main.hs                  -- Ponto de entrada do jogo e menu principal
├── src/
│   ├── Game/
│   │   ├── Board.hs             -- Tabuleiro e casas do jogo
│   │   ├── BoardHouse.hs        -- Tipos de casas e propriedades
│   │   ├── Player.hs            -- Estrutura e regras dos jogadores
│   │   ├── GameState.hs         -- Estado geral da partida
│   │   └── GameLoop.hs          -- Lógica principal do turno
│   ├── Ranking/
│   │   └── Ranking.hs           -- Leitura e gravação do ranking
│   └── MyLib.hs                 -- Lógica do loop geral do jogo
├── data/
│   └── ranking.txt              -- Ranking persistido com vitórias e derrotas
├── banco-imobiliario.cabal     -- Configuração do projeto Cabal
└── README.md                    -- Este arquivo
```

---

## 📚 Dependências

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) >= 9.6.3  
- [Cabal](https://www.haskell.org/cabal/) >= 3.8  

### Bibliotecas do Hackage:

- `base >= 4.18 && < 4.19`  
- `directory`  
- `random`  

💡 **Dica:** Você pode instalar o GHC e o Cabal usando o [GHCup](https://www.haskell.org/ghcup/).

---

##💻 Como Executar

1. **Clone o projeto**

```bash
  git clone https://github.com/seu-usuario/banco-imobiliario.git
  cd banco-imobiliario/haskell
```

2. **Instale as dependências**

```bash
  cabal update
  cabal build
```

3. **Execute o jogo**

```bash
  cabal run banco-imobiliario
```

##🔹 Exemplo de Execução
🎲 Bem-vindo ao Banco Imobiliário Terminal!

📋 Menu:
1. Cadastrar jogadores
2. Sair
Escolha uma opção: 1
Quantos jogadores? (2 a 4): 2
Nome do jogador 1: Alice
Nome do jogador 2: Bob

Iniciando rodada 1...
🔹 Alice rolou 5 e foi para a posição 5
Deseja comprar Cidade Vermelha? (s/n): s
...

## 📊 Ranking

Os dados de desempenho dos jogadores são salvos no arquivo `data/ranking.txt`. Ao final de cada partida:

- 🥇 O vencedor é registrado  
- 😓 Os derrotados também têm suas estatísticas atualizadas  
- 🧾 Os dados incluem vitórias, derrotas, saldo acumulado e propriedades finais  

---

## 📄 Licença
Este projeto está sob a Licença MIT. Consulte o arquivo LICENSE para mais detalhes.
