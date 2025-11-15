# RA2-24
PUCPR - Pontifícia Universidade Católica do Paraná\
Programação Lógica e Funcional\
Frank Coelho de Alcantara\
Eduardo Teodoro Moreira de Souza - Teodorooh

## Modo de Uso
Abrir o Link: https://onlinegdb.com/cEFkez6vC \
E clicar em "Run", botão verde na parte superior esquerda da tela ou F9.

Exemplo de uso:\
Apertar - F9\
Digitar - 1 e apertar "Enter"\
Digitar - 1 e apertar "Enter"\
Digitar - Frank e apertar "Enter"\
Digitar - 1 e apertar "Enter"\
Digitar - PUCPR e apertar "Enter"\
Digitar - 4\
E você poderá ver no terminal uma lista contendo um novo item.

## Cenários de Teste
1) F9 #Iniciou o sistema.

2) 1, ID: 11
Nome: Microfone
Quantidade: 10
Categoria: Periferico #Item adicionado.

3) 1, ID: 12
Nome: Mouse
Quantidade: 10
Categoria: Periferico #Item adicionado.

4) 1, ID: 13
Nome: Fone
Quantidade: 10
Categoria: Periferico #Item adicionado.

5) 0, #Encerrando o sistema.

6) F9, #Iniciou o sistema.
4, Itens carregados: 13
Arquivos criados.

7) 0, #Encerrou o sistema.
Enter,
F9, #Iniciou o sistema.

9) 4, #Itens carregados corretamente.

10) 1, ID: 14
Nome: Teclado
Quantidade: 10
Categoria: Periferico #Item adicionado.

11) 2, 
ID: 14
Qtd remover: 15 # Estoque insuficiente e exibiu erro corretamente.

12) Inventario.dat ainda mostra 10 unidades.

13) Auditoria.log contem o registro da falha.

14) 5, #Comando report

15) Relatorio gerado exibe a tentativa de entrada referente a falha no Cenário 2 
