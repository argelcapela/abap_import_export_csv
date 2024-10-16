<h1 align="center"> abap_import_export_csv </h1>

## :memo: Descrição
Programa ABAP que permite importar ou exportar arquivos CSV facilmente. Um grande diferencial é a capacidade de identificar a tabela apenas digitando seu nome em uma caixa de texto na tela de seleção. O que torna o programa muito mais versátil.

## 📷 Preview
<img src=".readme/tela_de_selecao.png" alt="Tela de Seleção" width="600px">
<a href="https://youtu.be/hZGbBWXTuTE">Vídeo de Demonstração do abap_import_export_csv</a>

## :books: Funcionalidades
* Importar arquivo CSV para tabela.
* Exportar tabela para arquivo CSV.
* Deletar todos os registros de uma tabela através do nome.
* Exibir ALV de uma tabela através do nome.

## :rocket: Rodando o projeto
<a href="https://github.com/argelcapela/abap_import_export_csv">Código Fonte no Github: abap_import_export_csv</a>
1) Acesse a transação SE38, crie um programa chamado zimport_export_csv e copie o código fonte do programa.
2) Ainda na SE38, editando o programa criado anteriormente, acesse os elementos de texto do programa. Copie e cole os elementos de texto 1 e 2, nas seções de text elements e selection texts.
3) Acesse a transação SE37, crie uma função chamada zf_gera_tabela e copie o código fonte da função. (É necessário criar um grupo de função antes de criar uma função, para isso acesse a transação SE80 e crie o grupo.)
<br>
<b>Configurando Import, Changing e Exceptions na SE37:</b><br>
<img src=".readme/config_se37_import.png" width="600px">
<img src=".readme/config_se37_changing.png" width="600px">
<img src=".readme/config_se37_exceptions.png" width="600px">
4) Depois que tudo for criado, ative o programa, função e grupo de função. Depois execute e seja feliz. 🙏😁

## :wrench: Tecnologias utilizadas
<div style="display: inline_block"><br>
    <img src="./.readme/abap_logo.webp" align="center" alt="abap logo" width="150" src="">
</div>

## 🧑‍🏫 Aprendizados
* Trabalhar com estruturas e tabelas dinâmicas
* Trabalhar com Field Symbols ("Ponteiros")
* Entender de modo mais profundo as funções no ABAP


## :handshake: Colaboradores
<table>
  <tr>
    <td align="center">
      <a href="http://github.com/argelcapela">
        <img src="https://avatars.githubusercontent.com/u/79276276?s=400&u=055b803f4708d59eaf50208ba601f85844125757&v=4" width="100px;" alt="Foto de Argel Capela!"/><br>
        <sub>
          <b>Argel Capela</b>
        </sub>
      </a>
    </td>
  </tr>
</table>

## :dart: Status do projeto
* Concluído em versão 1.0

## 🦟 Erros/Feedback
Identificou algum erro ou tem alguma sugestão? Por favor me informe, fico feliz em seguir aprendendo mais e melhorando.

## 🔐 Licença
<a href="LICENSE">MIT, Copyright (c) 2024 Argel Capela</a>
