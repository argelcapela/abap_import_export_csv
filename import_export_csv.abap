********************************************************************************
* Empresa         : XXXXXXXXXXXXXXX                                            *
* Cliente         : XXXXXXXXXXXXXXX                                            *
* Modulo          : XXXXXXXXXXXXXXX                                            *
* Titulo          : XXXXXXXXXXXXXXX                                            *
* Programa        : zimport_export_csv                                         *
* Transação       : XXXXXXXXXXXXXXX                                            *
* Tipo Programa   : XXXXXXXXXXXXXXX                                            *
* Funcional       : XXXXXXXXXXXXXXX                                            *
* Desenvolvedor   : Argel Capela dos Santos                                    *
* Data Criação    : XXXXXXXXXXXXXXX                                            *
*------------------------------------------------------------------------------*
*                           [HISTÓRICO]                                	       *
*------------------------------------------------------------------------------*
* Ult Modif   Autor          Chamado      Descrição                            *
* XXXXXXXXXX  XXXXXXXXXXXX   XXXXXXXXXX   XXXXXXXXXXX                          *
*------------------------------------------------------------------------------*
REPORT zimport_export_csv.

*----------------------------------------------------------------------*
* Tabelas Internas                                                     *
*----------------------------------------------------------------------*
DATA: t_csv_bruto           TYPE truxs_t_text_data,
      t_arquivo_selecionado TYPE filetable,
      t_csv                 TYPE TABLE OF string,
      t_tabela_gerada       TYPE REF TO data,
      t_tabela_gerada_final TYPE REF TO data.

*----------------------------------------------------------------------*
* Work Areas                                                           *
*----------------------------------------------------------------------*
DATA: w_linha_dados_lidos       TYPE REF TO data,
      w_linha_dados_lidos_final TYPE REF TO data,
      w_arquivo_selecionado     LIKE LINE OF t_arquivo_selecionado.

*----------------------------------------------------------------------*
* Variáveis                                                            *
*----------------------------------------------------------------------*
DATA: l_finaliza_linhas TYPE cl_rsda_csv_converter=>char VALUE '"',
      l_separa_campos   TYPE cl_rsda_csv_converter=>char VALUE ',',
      l_mensagem        TYPE string,
      l_mensagem_erro   TYPE string,
      l_rc              TYPE i,
      l_resposta        TYPE c,
      l_nome_tabela     TYPE dd02l-tabname,
      l_root_exception  TYPE REF TO cx_root,
      l_linha_csv       TYPE string,
      l_campo_csv       TYPE string,
      l_csv_converter   TYPE REF TO cl_rsda_csv_converter,
      l_salv_table      TYPE REF TO cl_salv_table.

*----------------------------------------------------------------------*
* Field-Symbols                                                        *
*----------------------------------------------------------------------*
FIELD-SYMBOLS: <f_dados_lidos>             TYPE ANY TABLE,
               <f_linha_dados_lidos>       TYPE any,
               <f_dados_lidos_final>       TYPE ANY TABLE,
               <f_linha_dados_lidos_final> TYPE any.

*----------------------------------------------------------------------*
* Tela de Seleção                                                      *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_path  TYPE string,
              p_table TYPE string.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_import RADIOBUTTON GROUP rad DEFAULT 'X' USER-COMMAND zm_muda_seletor.
  SELECTION-SCREEN COMMENT /1(66) TEXT-003.
  PARAMETERS: p_export RADIOBUTTON GROUP rad.
  SELECTION-SCREEN COMMENT /1(66) TEXT-004.
  PARAMETERS: p_erase  RADIOBUTTON GROUP rad.
  SELECTION-SCREEN COMMENT /1(66) TEXT-005.
  PARAMETERS: p_alv  RADIOBUTTON GROUP rad.
SELECTION-SCREEN END OF BLOCK b2.

* Ativa o Seletor de Arquivo/Pasta para o parâmetro p_path
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  IF p_import = 'X'.
    PERFORM zf_abri_selecao_arquivo.
  ELSEIF p_export = 'X'.
    PERFORM zf_abri_selecao_pasta.
  ENDIF.

* Desabilita p_path, se o radiobutton p_erase estiver selecionado
AT SELECTION-SCREEN OUTPUT.
  IF p_erase = 'X' OR p_alv = 'X'.
    LOOP AT SCREEN.
      IF screen-name = 'P_PATH'.
        p_path = ''.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

START-OF-SELECTION.

*----------------------------------------------------------------------*
* Configuração Inicial                                                 *
*----------------------------------------------------------------------*
  l_nome_tabela     = p_table.
  l_finaliza_linhas  = '"'.
  l_separa_campos   = ','.

*----------------------------------------------------------------------*
* Execução                                                             *
*----------------------------------------------------------------------*
  IF p_import = 'X'.
    PERFORM zf_valida_parametros_import.
  ELSEIF p_export = 'X'.
    PERFORM zf_valida_parametros_export.
  ELSEIF p_erase = 'X'.
    PERFORM zf_valida_parametros_erase.
  ELSEIF p_alv = 'X'.
    PERFORM zf_exibe_tabela.
  ENDIF.

*----------------------------------------------------------------------*
* FORMs                                                                *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form zf_abri_selecao_arquivo                                        *
*&---------------------------------------------------------------------*
*& Abre Janela para seleção de um arquivo no computador.               *
*&---------------------------------------------------------------------*
FORM zf_abri_selecao_arquivo.

  TRY.

      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          window_title            = 'Arquivo CSV'
          file_filter             = 'CSV Files (*.csv)|*.csv'
          initial_directory       = 'c:\'
        CHANGING
          file_table              = t_arquivo_selecionado
          rc                      = l_rc
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          not_supported_by_gui    = 4
          OTHERS                  = 5.

      IF sy-subrc = 0.
        READ TABLE t_arquivo_selecionado INTO w_arquivo_selecionado INDEX 1.
        p_path = w_arquivo_selecionado-filename.
      ENDIF.

    CATCH cx_root INTO l_root_exception.

      CLEAR l_mensagem_erro.
      l_mensagem_erro = l_root_exception->get_text( ).
      CONCATENATE TEXT-006 l_mensagem_erro  INTO l_mensagem_erro.
      MESSAGE l_mensagem_erro TYPE 'I'.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_abri_selecao_pasta                                          *
*&---------------------------------------------------------------------*
*& Abre janela que permite selecionar uma pasta do computador.         *
*&---------------------------------------------------------------------*
FORM zf_abri_selecao_pasta.

  TRY.

      CALL METHOD cl_gui_frontend_services=>directory_browse
        EXPORTING
          window_title    = 'Selecionar diretório de exportação.'
          initial_folder  = 'c:\'
        CHANGING
          selected_folder = p_path.

*   Gera um path completo de exportação baseado na pasta selecionada e no nome de tabela informado
      IF p_table IS NOT INITIAL.
        CONCATENATE p_path '\' p_table '.csv' INTO p_path.
      ELSE.
        CONCATENATE p_path '\tabela_exportada.csv' INTO p_path.
      ENDIF.

    CATCH cx_root INTO l_root_exception.

      CLEAR l_mensagem_erro.
      l_mensagem_erro = l_root_exception->get_text( ).
      CONCATENATE TEXT-007 l_mensagem_erro  INTO l_mensagem_erro.
      MESSAGE l_mensagem_erro TYPE 'I'.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_valida_parametros_import.                                   *
*&---------------------------------------------------------------------*
*& Valida se os parâmetros da tela de seleção (p_table e p_path)       *
*& estão preenchidos corretamente, antes de tentar importar um arquivo *
*& csv.                                                                *
*&---------------------------------------------------------------------*
FORM zf_valida_parametros_import.

* Verifica se o parâmetro p_path está vazio
  IF p_path IS INITIAL.
    MESSAGE TEXT-008 TYPE 'I'.

* Verifica se o parâmetro p_table está vazio
  ELSEIF p_table IS INITIAL.
    MESSAGE TEXT-009 TYPE 'I'.

* Verifica se a tabela é uma tabela z
  ELSEIF p_table(1) <> 'z' AND p_table(1) <> 'Z'.
    MESSAGE TEXT-010 TYPE 'I'.

  ELSE.
    PERFORM zf_le_csv.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_le_csv                                                      *
*&---------------------------------------------------------------------*
*& Carrega o arquivo CSV que foi informado no parâmetro p_path         *
*& e cria uma tabela interna em que cada registro é uma linha desse    *
*& arquivo. Também gera uma tabela interna do tipo da tabela informada *
*& no parâmetro p_table, dinamicamente, em preparação para o proce-    *
*& ssamento do CSV.                                                    *
*&---------------------------------------------------------------------*
FORM zf_le_csv.

  TRY.
*   Gera dinamicamente, uma tabela interna, com o nome que foi informado no parâmetro p_table.
      CALL FUNCTION 'ZF_GERA_TABELA'
        EXPORTING
          p_nome_tabela   = p_table
          p_tipo_dado     = 'string'
        CHANGING
          t_tabela_gerada = t_tabela_gerada
        EXCEPTIONS
          OTHERS          = 1.

      IF sy-subrc = 0.
*         Cria uma referência para a tabela interna gerada anteriormente, no Field Symbol <f_dados_lidos>.
        ASSIGN t_tabela_gerada->* TO <f_dados_lidos>.

*       Cria dinamicamente a variável w_linha_dados_lidos, que representa uma linha da tabela interna, apontada por <f_dados_lidos>.
        CREATE DATA w_linha_dados_lidos LIKE LINE OF <f_dados_lidos>.

*       Cria uma referência para a variável w_linha_dados_lidos, no field-symbol f_linha_dados_lidos.
        ASSIGN w_linha_dados_lidos->* TO <f_linha_dados_lidos>.

*       Carrega o arquivo csv do PC para o SAP. Cria uma tabela interna, em que cada registro dessa tabela, representa uma linha do arquivo CSV. Sem nenhum separação por enquanto.
        CALL FUNCTION 'GUI_UPLOAD'
          EXPORTING
            filename = p_path
            filetype = 'ASC'
          TABLES
            data_tab = t_csv_bruto
          EXCEPTIONS
            OTHERS   = 1.

        IF sy-subrc = 0.
          PERFORM zf_processa_csv.
        ELSE.
          MESSAGE TEXT-011 TYPE 'I'.
        ENDIF.

      ENDIF.

    CATCH cx_root INTO l_root_exception.

      CLEAR l_mensagem_erro.
      l_mensagem_erro = l_root_exception->get_text( ).
      CONCATENATE TEXT-012 l_mensagem_erro  INTO l_mensagem_erro.
      MESSAGE l_mensagem_erro TYPE 'I'.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_processa_csv                                                *
*&---------------------------------------------------------------------*
*& Percorre a tabela interna t_csv_bruto, que contem as linhas do CSV, *
*& tranforma cada linha em uma estrutura igual a tabela que deseja-se  *
*& importar, e ao final, cria-se uma tabela interna com todas essas    *
*& linhas estruturadas.                                                *
*&---------------------------------------------------------------------*
FORM zf_processa_csv.

  TRY.
*     Cria uma variável para conversão de CSV. Define os caracteres de separação entre campos, e finalização de cada linha.
      l_csv_converter = cl_rsda_csv_converter=>create( i_delimiter = l_finaliza_linhas
                                                       i_separator = l_separa_campos ).

      IF sy-subrc = 0.

*       Percorre as linhas do arquivo CSV, armazenadas na tabela interna t_csv_bruto.
        LOOP AT t_csv_bruto INTO DATA(w_linha_csv_bruto).

*         Transforma a string que representa a linha do arquivo csv, em uma estrutura.
*         Ou seja, segui as regras definidas pelo conversor criado anteriormente para:
*             1) Criar uma estrutura, igual a tabela informada no parâmetro p_table.
*             2) Separar os campos, dessa linha do CSV e atribuir sequencialmente aos campos da estrutura criada.
          CALL METHOD l_csv_converter->csv_to_structure
            EXPORTING
              i_data   = w_linha_csv_bruto        " Work Area que representa uma linha do arquivo CSV lido.
            IMPORTING
              e_s_data = <f_linha_dados_lidos>.   " Ponteiro que representa a estrutura da tabela para qual deseja-se importar os dados do CSV.

*         Após a conversão de String para Estrutura. Insere essa estrutura como um registro na tabela interna referenciada por <f_dados_lidos>, ou seja, t_tabela_gerada.
          INSERT <f_linha_dados_lidos> INTO TABLE <f_dados_lidos>.

        ENDLOOP.

*       Se tudo ocorrer bem. Segui para a importação dos dados lidos na tabela desejada.
        PERFORM zf_insere_dados_tabela.
      ENDIF.

    CATCH cx_root INTO l_root_exception.

      CLEAR l_mensagem_erro.
      l_mensagem_erro = l_root_exception->get_text( ).
      CONCATENATE TEXT-013 l_mensagem_erro  INTO l_mensagem_erro.
      MESSAGE l_mensagem_erro TYPE 'I'.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_insere_dados_tabela                                         *
*&---------------------------------------------------------------------*
*& Insere os dados do arquivo CSV no banco de dados.                   *
*&---------------------------------------------------------------------*
FORM zf_insere_dados_tabela.

  TRY.

*   Gera tabela interna identica a tabela informada no parâmetro p_table
      CALL FUNCTION 'ZF_GERA_TABELA'
        EXPORTING
          p_nome_tabela   = p_table
          p_tipo_dado     = 'final'
        CHANGING
          t_tabela_gerada = t_tabela_gerada_final
        EXCEPTIONS
          OTHERS          = 1.

      IF sy-subrc = 0.

*       Passa a referência da tabela gerada, para o ponteiro(Field-Symbol) <f_dados_lidos_final>
        ASSIGN t_tabela_gerada_final->* TO <f_dados_lidos_final>.

*       Cria um Work Area, que é a estrutura da tabela interna t_tabela_gerada_final.
        CREATE DATA w_linha_dados_lidos LIKE LINE OF <f_dados_lidos_final>.

*       Passa a referência da estrutura criada anteriormente para o ponteiro(Field-Symbol) <f_linha_dados_lidos_final>
        ASSIGN w_linha_dados_lidos->* TO <f_linha_dados_lidos_final>.

*       Transfere os dados da tabela interna de strings: t_tabela_gerada(<f_dados_lidos>). Para a tabela interna: t_tabela_gerada_final(<f_dados_lidos_final>).
*        Convertendo os dados de String para os tipos de dados originais da tabela informada no parâmetro p_table, se necessário.
        LOOP AT <f_dados_lidos> ASSIGNING <f_linha_dados_lidos>.

          MOVE-CORRESPONDING <f_linha_dados_lidos> TO <f_linha_dados_lidos_final>.
          INSERT <f_linha_dados_lidos_final> INTO TABLE <f_dados_lidos_final>.

        ENDLOOP.

*       Insere a tabela interna, que contem os dados do arquivo CSV, na tabela transparente que foi informada em p_table. Concluindo a importação.
        INSERT (l_nome_tabela) FROM TABLE <f_dados_lidos_final>.

*       Confirma as alterações no Banco de Dados.
        COMMIT WORK.

*       Mensagens de sucesso ou de erro
        IF sy-dbcnt = 0.
          MESSAGE TEXT-014 TYPE 'I'.
        ELSE.
          MESSAGE TEXT-015 TYPE 'I'.
        ENDIF.
      ENDIF.

    CATCH cx_root INTO l_root_exception.

*     Cancela as alterações no banco de dados. Caso alguma excessão ocorra.
      ROLLBACK WORK.

      CLEAR l_mensagem_erro.
      l_mensagem_erro = l_root_exception->get_text( ).
      CONCATENATE TEXT-014 l_mensagem_erro  INTO l_mensagem_erro SEPARATED BY space.
      MESSAGE l_mensagem_erro TYPE 'I'.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_valida_parametros_export.                                   *
*&---------------------------------------------------------------------*
*& Valida se os parâmetros da tela de seleção (p_table e p_path)       *
*& estão preenchidos corretamente, antes de tentar exportar uma        *
*& tabela transparente.                                                *
*&---------------------------------------------------------------------*
FORM zf_valida_parametros_export.

* Verifica se o parâmetro p_path está vazio
  IF p_path IS INITIAL.
    MESSAGE TEXT-008 TYPE 'I'.

* Verifica se o parâmetro p_table está vazio
  ELSEIF p_table IS INITIAL.
    MESSAGE TEXT-009 TYPE 'I'.

  ELSE.
    PERFORM zf_exporta_tabela.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_exporta_tabela                                              *
*&---------------------------------------------------------------------*
*& Exporta os dados de uma tabela transparente para um arquivo CSV.    *
*&---------------------------------------------------------------------*
FORM zf_exporta_tabela.

  TRY.

*   Gera tabela interna, com a estrutura da tabela informada em p_table. Em preparação para a exportação.
      CALL FUNCTION 'ZF_GERA_TABELA'
        EXPORTING
          p_nome_tabela   = p_table
          p_tipo_dado     = 'clone'
        CHANGING
          t_tabela_gerada = t_tabela_gerada
        EXCEPTIONS
          OTHERS          = 1.

      IF sy-subrc = 0.

*         Cria uma referência para a tabela interna gerada anteriormente, no Field Symbol <f_dados_lidos>.
        ASSIGN t_tabela_gerada->* TO <f_dados_lidos>.

*         Seleciona todos os dados da tabela transparente informada em p_table e armazena na tabela interna criada anteriormente.
        SELECT *
          FROM (l_nome_tabela)
          INTO CORRESPONDING FIELDS OF TABLE <f_dados_lidos>.

*         Percorre a tabela interna gerada e cria uma tabela interna de strings, t_csv. O loop transforma para registro em uma string, com os valores dos campos separados
*          pelo caractere separador.
        LOOP AT <f_dados_lidos> ASSIGNING <f_linha_dados_lidos>.

          DO.
*
            ASSIGN COMPONENT sy-index OF STRUCTURE <f_linha_dados_lidos> TO FIELD-SYMBOL(<f_campo>).

*             Se registro não tem mais campos. Sai do loop e parte para o próximo registro da tabela.
            IF sy-subrc <> 0.
              EXIT.
            ENDIF.

*             Se campo for uma string com virgula, coloca entre parenteses
            CLEAR l_campo_csv.
            l_campo_csv = <f_campo>.
            IF l_campo_csv CA sy-abcde.
              IF sy-subrc = 0.

                IF l_campo_csv CA ','.
                  CONCATENATE '"' l_campo_csv '"' INTO l_campo_csv.
                ENDIF.

              ENDIF.
            ENDIF.

*             Se for o primeiro campo do registro. Apenas atribua seu valor na variável l_linha_csv.
            IF sy-index = 1.
              l_linha_csv = l_campo_csv.
*             Se não for o primeiro campo, concatena o valor que já existe na l_linha_csv com o valor do próximo campo do registro, separado por um caracter separador.
            ELSE.
              CONDENSE l_campo_csv.
              CONCATENATE l_linha_csv l_campo_csv INTO l_linha_csv SEPARATED BY l_separa_campos.
            ENDIF.

          ENDDO.

          APPEND l_linha_csv TO t_csv.

        ENDLOOP.

*         Cria o arquivo CSV com base na tabela interna t_csv montada anteriormente. E baixa o arquivo CSV no diretório informado no parâmetro p_path.
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename                = p_path
          TABLES
            data_tab                = t_csv
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            OTHERS                  = 22.

        IF sy-subrc = 0.
          CLEAR l_mensagem.
          CONCATENATE TEXT-016 p_path '.' p_table INTO l_mensagem SEPARATED BY space.
          MESSAGE l_mensagem TYPE 'I'.
        ELSE.
          MESSAGE TEXT-017 TYPE 'I'.
        ENDIF.
      ENDIF.

    CATCH cx_root INTO l_root_exception.

      CLEAR l_mensagem_erro.
      l_mensagem_erro = l_root_exception->get_text( ).
      CONCATENATE TEXT-018 l_mensagem_erro  INTO l_mensagem_erro SEPARATED BY ' '.
      MESSAGE l_mensagem_erro TYPE 'I'.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_valida_parametros_erase.                                    *
*&---------------------------------------------------------------------*
*& Valida se o parâmetro p_table está preenchido corretamente. Antes   *
*& de tentar deletar os registros de uma tabela transparente.          *
*&---------------------------------------------------------------------*
FORM zf_valida_parametros_erase.

  IF p_table IS INITIAL.
    MESSAGE TEXT-009 TYPE 'I'.
  ELSEIF p_table(1) <> 'z' AND p_table(1) <> 'Z'.
    MESSAGE TEXT-010 TYPE 'I'.
  ELSE.
    PERFORM zf_deleta_registros.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_deleta_registros                                            *
*&---------------------------------------------------------------------*
*& Deleta os registros de uma tabela transparente autorizada.          *
*&---------------------------------------------------------------------*
FORM zf_deleta_registros.

  CONCATENATE TEXT-019 p_table INTO l_mensagem SEPARATED BY ' '.

* Chama função para exibir mensagem de confirmação na tela.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Resetar Tabela'
      text_question         = l_mensagem
      default_button        = 'N'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      display_cancel_button = ''
    IMPORTING
      answer                = l_resposta
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF l_resposta = 1.

*   Deleta todos os registros da tabela informada no parâmetro p_table.
    DELETE FROM (l_nome_tabela).

    IF sy-dbcnt = 0.
      MESSAGE TEXT-020 TYPE 'I'.
    ELSE.
      MESSAGE TEXT-021 TYPE 'I'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_exibe_tabela                                                *
*&---------------------------------------------------------------------*
*& Exibe a tabela usando ALV.                                           *
*&---------------------------------------------------------------------*
FORM zf_exibe_tabela.

  IF p_table IS INITIAL.
    MESSAGE TEXT-022 TYPE 'I'.
  ELSE.
    TRY.

*       Gera tabela interna, com a estrutura da tabela informada em p_table. Em preparação para a exportação.
        CALL FUNCTION 'ZF_GERA_TABELA'
          EXPORTING
            p_nome_tabela   = p_table
            p_tipo_dado     = 'clone'
          CHANGING
            t_tabela_gerada = t_tabela_gerada
          EXCEPTIONS
            OTHERS          = 1.

        IF sy-subrc = 0.

*             Cria uma referência para a tabela interna gerada anteriormente, no Field Symbol <f_dados_lidos>.
          ASSIGN t_tabela_gerada->* TO <f_dados_lidos>.

*             Seleciona todos os dados da tabela transparente informada em p_table e armazena na tabela interna criada anteriormente.
          SELECT *
            FROM (l_nome_tabela)
            INTO CORRESPONDING FIELDS OF TABLE <f_dados_lidos>.

*             Executa ALV tendo como dados a tabela criada dinamicamente os dados
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = l_salv_table
            CHANGING
              t_table      = <f_dados_lidos> ).

          IF sy-subrc = 0.
            l_salv_table->display( ).
          ELSE.
            MESSAGE TEXT-023 TYPE 'I'.
            LEAVE LIST-PROCESSING.
          ENDIF.
        ENDIF.

      CATCH cx_root INTO l_root_exception.

        CLEAR l_mensagem_erro.
        l_mensagem_erro = l_root_exception->get_text( ).
        CONCATENATE TEXT-024 l_mensagem_erro  INTO l_mensagem_erro SEPARATED BY ' '.
        MESSAGE l_mensagem_erro TYPE 'I'.

    ENDTRY.

  ENDIF.

ENDFORM.