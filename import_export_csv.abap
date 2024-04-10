********************************************************************************
* Empresa         : XXXXXXXXXXXXXXX                                            *
* Cliente         : XXXXXXXXXXXXXXX                                            *
* Modulo          : XXXXXXXXXXXXXXX                                            *
* Titulo          : XXXXXXXXXXXXXXX                                            *
* Programa        : zimport_export_csv                                         *
* Transação       : XXXXXXXXXXXXXXX                                            *
* Tipo Programa   : XXXXXXXXXXXXXXX                                            *
* Funcional       : XXXXXXXXXXXXXXX                                            *
* Desenvolvedor   : XXXXXXXXXXXXXXX                                            *
* Data Criação    : XXXXXXXXXXXXXXX                                            *
*------------------------------------------------------------------------------*
*                           [HISTÓRICO]                                	       *
*------------------------------------------------------------------------------*
* Ult Modif   Autor          Chamado      Descrição                            *
* XXXXXXXXXX  XXXXXXXXXXXX   XXXXXXXXXX   XXXXXXXXXXX                          *
*------------------------------------------------------------------------------*
REPORT zimport_export_csv.

*----------------------------------------------------------------------*
* Estruturas                                                           *
*----------------------------------------------------------------------*
DATA: y_estrutura_gerada TYPE REF TO cl_abap_structdescr,
      y_estrutura_tabela TYPE REF TO cl_abap_structdescr.

*----------------------------------------------------------------------*
* Tabelas Internas                                                       *
*----------------------------------------------------------------------*
DATA: t_descricao_tabela_gerada      TYPE REF TO cl_abap_tabledescr,
      t_tabela_csv       TYPE TABLE OF zestudo_empresas,
      t_csv_bruto         TYPE truxs_t_text_data,
      t_filetable        TYPE filetable,
      t_descricao_estrutura_gerada TYPE cl_abap_structdescr=>component_table,
      t_csv              TYPE TABLE OF string.

*----------------------------------------------------------------------*
* Work Areas                                                            *
*----------------------------------------------------------------------*
DATA: w_tabela_csv             TYPE zestudo_empresas,
      w_row                    TYPE REF TO data,
      w_filetable              LIKE LINE OF t_filetable,
      t_tabela_gerada                   TYPE REF TO data,
      w_campo_estrutura_gerada LIKE LINE OF t_descricao_estrutura_gerada.

*----------------------------------------------------------------------*
* Variáveis                                                            *
*----------------------------------------------------------------------*
DATA: l_delimiter          TYPE cl_rsda_csv_converter=>char VALUE '"',
      l_field_separator    TYPE cl_rsda_csv_converter=>char VALUE ',',
      l_mensagem           TYPE string,
      l_mensagem_erro      TYPE string,
      l_rc                 TYPE i, " código de retorno da seleção do arquivo csv
      l_resposta           TYPE c,
      l_tabname            TYPE dd02l-tabname,
      l_root_exception     TYPE REF TO cx_root,
      l_nome_tabela        TYPE ddobjname,
      l_informacoes_tabela TYPE TABLE OF dfies,
      l_linha_csv          TYPE string,
      l_campo_csv          TYPE string.

*----------------------------------------------------------------------*
* Field-Symbols                                                        *
*----------------------------------------------------------------------*
FIELD-SYMBOLS: <f_dados_lidos>             TYPE ANY TABLE,
               <fw_tabela_csv_row> TYPE any.

*----------------------------------------------------------------------*
* Tela de Seleção                                                      *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_path  TYPE string,
              p_table TYPE string.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:
    p_import RADIOBUTTON GROUP rad DEFAULT 'X' USER-COMMAND zm_muda_seletor,
    p_export RADIOBUTTON GROUP rad.
  PARAMETERS: p_erase  RADIOBUTTON GROUP rad.
  SELECTION-SCREEN COMMENT /1(60) TEXT-007.
SELECTION-SCREEN END OF BLOCK b2.

* Ativa o Seletor de Arquivo/Pasta para o parâmetro p_path
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  IF p_import = 'X'.
    PERFORM zf_abri_selecao_arquivo.
  ELSE.
    PERFORM zf_abri_selecao_pasta.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
* disable checkbox when select erase radio button...

START-OF-SELECTION.

*----------------------------------------------------------------------*
* Execução                                                             *
*----------------------------------------------------------------------*
  IF p_import = 'X'.
    PERFORM zf_valida_parametros_import.
  ELSEIF p_export = 'X'.
    PERFORM zf_valida_parametros_export.
  ELSE.
    PERFORM zf_valida_parametros_erase.
  ENDIF.

*----------------------------------------------------------------------*
* FORMs                                                                *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form zf_abri_selecao_arquivo                                       *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_abri_selecao_arquivo.

  TRY.

      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          window_title            = 'Arquivo CSV'
          file_filter             = 'CSV Files (*.csv)|*.csv'
          initial_directory       = 'c:\'
        CHANGING
          file_table              = t_filetable
          rc                      = l_rc
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          not_supported_by_gui    = 4
          OTHERS                  = 5.

      IF sy-subrc = 0.
        READ TABLE t_filetable INTO w_filetable INDEX 1.
        p_path = w_filetable-filename.
      ENDIF.

    CATCH cx_root INTO l_root_exception.

      CLEAR l_mensagem_erro.
      l_mensagem_erro = l_root_exception->get_text( ).
      CONCATENATE 'Erro ao tentar selecionar arquivo: ' l_mensagem_erro  INTO l_mensagem_erro.
      MESSAGE l_mensagem_erro TYPE 'I'.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_abri_selecao_pasta                                         *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_abri_selecao_pasta.

  TRY.

      CALL METHOD cl_gui_frontend_services=>directory_browse
        EXPORTING
          window_title    = 'Selecionar diretório de exportação'
          initial_folder  = 'c:\'
        CHANGING
          selected_folder = p_path.

    CATCH cx_root INTO l_root_exception.

      CLEAR l_mensagem_erro.
      l_mensagem_erro = l_root_exception->get_text( ).
      CONCATENATE 'Erro ao tentar selecionar pasta: ' l_mensagem_erro  INTO l_mensagem_erro.
      MESSAGE l_mensagem_erro TYPE 'I'.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_gera_tabela.                                                *
*&---------------------------------------------------------------------*
*& Gera dinamicamente, uma tabela interna conforme o nome da tabela    *
*& informado no parâmetro p_table.                                     *
*&                                                                     *
*& - tipo_dado -> Pode ser 'string' ou 'clone'.                        *
*&    - 'string': Gera tabela com todos os elementos como string.      *
*&    - 'clone' ou qualquer outro valor: Gera tabela identica.         *
*&---------------------------------------------------------------------*
FORM zf_gera_tabela USING p_nome_tabela p_tipo_dado.

  TRY.

      l_nome_tabela = p_nome_tabela.

*     Verifica se a tabela existe e obtem informações sobre a tabela. (Nome dos campos, elemento de dado, etc.)
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname   = l_nome_tabela                                     " Nome da tabela digitada pelo usuário
        TABLES
          dfies_tab = l_informacoes_tabela                              " Tabela para armazenar detalhes dos campos
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      IF sy-subrc = 0.

*       Se a tabela existir. Percorre as informações obtidas e constrói uma estrutura, com base nessa tabela.
        LOOP AT l_informacoes_tabela INTO DATA(w_informacao_tabela).

          IF w_informacao_tabela-fieldname = 'MANDT'.
            CONTINUE.
          ENDIF.

*         Define o nome dos campos da estrutura a ser gerada dinamicamente.
          w_campo_estrutura_gerada-name = w_informacao_tabela-fieldname.

          IF p_tipo_dado = 'string'.
*           Define o elemento de dado dos campos da estrutura a ser gerada como string.
            w_campo_estrutura_gerada-type = cl_abap_elemdescr=>get_string( ).
          ELSE.
*           Define o elemento de dado dos campos da estrutura a ser gerada,identificos a tabela informada.
            TRY.
                w_campo_estrutura_gerada-type ?= cl_abap_elemdescr=>describe_by_name( w_informacao_tabela-rollname ).
              CATCH cx_root.
                w_campo_estrutura_gerada-type = cl_abap_elemdescr=>get_string( ). " Por padrão, defina como string em caso de falha ao criar o elemento
            ENDTRY.
          ENDIF.

          APPEND w_campo_estrutura_gerada TO t_descricao_estrutura_gerada.

        ENDLOOP.

*       Após definir como a estrutura a ser gerada deve ser. Cria a estrutura.
        y_estrutura_gerada = cl_abap_structdescr=>create( t_descricao_estrutura_gerada ).

*       Cria a descrição para a criação de uma tabela interna dinamica, com base na estrutura gerada anteriormente.
        t_descricao_tabela_gerada = cl_abap_tabledescr=>create( p_line_type = y_estrutura_gerada
                                                                p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                                p_unique = abap_false ).

*       Cria uma tabela interna dinamicamente, com base na descricao da tabela, gerada anteriormente.
        CREATE DATA t_tabela_gerada TYPE HANDLE t_descricao_tabela_gerada.

*       Cria uma referência para a tabela interna gerada anteriormente, no Field Symbol <f_dados_lidos>.
        ASSIGN t_tabela_gerada->* TO <f_dados_lidos>.

      ELSE.
        MESSAGE 'Tabela Inválida!' TYPE 'I'.
      ENDIF.

    CATCH cx_root INTO l_root_exception.

      CLEAR l_mensagem_erro.
      l_mensagem_erro = l_root_exception->get_text( ).
      CONCATENATE 'Erro ao processar a tabela informada: ' l_mensagem_erro  INTO l_mensagem_erro.
      MESSAGE l_mensagem_erro TYPE 'I'.

  ENDTRY.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form zf_valida_parametros_import.                                  *
*&---------------------------------------------------------------------*
*& Valida se os parâmetros da tela de seleção (p_table e p_path)
*& estão preenchidos corretamente, antes de tentar importar um arquivo
*& csv.
*&---------------------------------------------------------------------*
FORM zf_valida_parametros_import.

* Verifica se o parâmetro p_path está vazio
  IF p_path IS INITIAL.
    MESSAGE 'Selecione um arquivo csv valido.' TYPE 'I'.

* Verifica se o parâmetro p_table está vazio
  ELSEIF p_table IS INITIAL.
    MESSAGE 'Digite o nome de uma tabela autorizada para importação de arquivos CSV.' TYPE 'I'.

* Verifica se a tabela é uma tabela z
  ELSEIF p_table(1) <> 'z' AND p_table(1) <> 'Z'.
    MESSAGE 'Utilize esse programa apenas em tabelas Z.' TYPE 'I'.

  ELSE.
    PERFORM zf_gera_tabela USING p_table 'string'.
    PERFORM zf_le_csv.
    PERFORM zf_processa_csv.
    PERFORM zf_insere_dados_tabela.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_le_csv                                                     *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_le_csv.

* cria dinamicamente uma variável w_row que tem como tipo a estrutura de f_dados_lidos
  CREATE DATA w_row LIKE LINE OF <f_dados_lidos>.

* cria uma referência(ponteiro) para um registro da tabela t_descricao_tabela_gerada
  ASSIGN w_row->* TO <fw_tabela_csv_row>.

* Carrega o arquivo csv do PC para o SAP.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = p_path
      filetype = 'ASC'
    TABLES
      data_tab = t_csv_bruto.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_processa_csv                                               *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_processa_csv.
  DATA(o_csv_converter) = cl_rsda_csv_converter=>create( i_delimiter = l_delimiter i_separator =
 l_field_separator ).

  IF sy-subrc = 0.
*   Loop pelas linhas do arquivo csv
    LOOP AT t_csv_bruto INTO DATA(w_csv_line).

*     Separa linha e joga na estrutura fw_tabela_csv_row
      CALL METHOD o_csv_converter->csv_to_structure
        EXPORTING
          i_data   = w_csv_line
        IMPORTING
          e_s_data = <fw_tabela_csv_row>.

      INSERT <fw_tabela_csv_row> INTO TABLE <f_dados_lidos>.

    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_insere_dados_tabela                                     *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_insere_dados_tabela.

  LOOP AT <f_dados_lidos> ASSIGNING <fw_tabela_csv_row>.

    MOVE-CORRESPONDING <fw_tabela_csv_row> TO w_tabela_csv.
    APPEND w_tabela_csv TO t_tabela_csv.

  ENDLOOP.

  TRY.
      IF p_table <> 'ZESTUDO_EMPRESAS'.
        EXIT.
      ENDIF.


      l_tabname = p_table.

      INSERT (l_tabname) FROM TABLE t_tabela_csv.

      COMMIT WORK.

      IF sy-dbcnt = 0.
        MESSAGE TEXT-005 TYPE 'I'.
      ELSE.
        MESSAGE TEXT-003 TYPE 'I'.
      ENDIF.

    CATCH cx_root INTO DATA(lo_exception).

      ROLLBACK WORK.
      l_mensagem_erro = lo_exception->get_text( ).
      CONCATENATE TEXT-005
                  ' '
                  l_mensagem_erro
                  INTO l_mensagem SEPARATED BY space.
      MESSAGE l_mensagem TYPE 'I'.
      LEAVE LIST-PROCESSING.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_valida_parametros_export.                                  *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_valida_parametros_export.

* Verifica se o parâmetro p_path está vazio
  IF p_path IS INITIAL.
    MESSAGE 'Selecione um arquivo csv valido.' TYPE 'I'.

* Verifica se o parâmetro p_table está vazio
  ELSEIF p_table IS INITIAL.
    MESSAGE 'Digite o nome de uma tabela autorizada para importação de arquivos CSV.' TYPE 'I'.

* Verifica se a tabela é uma tabela z
  ELSEIF p_table(1) <> 'z' AND p_table(1) <> 'Z'.
    MESSAGE 'Utilize esse programa apenas em tabelas Z.' TYPE 'I'.

  ELSE.
    PERFORM zf_gera_tabela USING p_table 'clone'.
    PERFORM zf_exporta_tabela.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_exporta_tabela                                             *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_exporta_tabela.

  TRY.

      l_tabname = p_table.

      SELECT *
        FROM (l_tabname)
        INTO CORRESPONDING FIELDS OF TABLE <f_dados_lidos>.

      "CONSTRUCT THE TARGET TABLE FOR DOWNLOAD.SEPARATE VALUE WITH COMMAS
      LOOP AT <f_dados_lidos> ASSIGNING FIELD-SYMBOL(<fs_line>).

        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fs_value>).
          IF sy-subrc NE 0.
            EXIT.
          ENDIF.
          IF sy-index EQ 1.
            l_linha_csv = <fs_value>.
          ELSE.
            l_campo_csv = <fs_value>.
            CONDENSE l_campo_csv.
            CONCATENATE l_linha_csv l_campo_csv INTO l_linha_csv SEPARATED BY l_field_separator.
          ENDIF.
        ENDDO.

        APPEND l_linha_csv TO t_csv.

      ENDLOOP.

      "DOWNLOAD THE TABLE INTO CSV FILE
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

*      CLEAR l_mensagem.
*      CONCATENATE 'Tabela' p_table 'foi exportada com sucesso.' INTO l_mensagem SEPARATED BY ' '.
*      MESSAGE l_mensagem TYPE 'I'.

    CATCH cx_root INTO l_root_exception.

      CLEAR l_mensagem_erro.
      l_mensagem_erro = l_root_exception->get_text( ).
      CONCATENATE 'Erro na exportação da tabela:' l_mensagem_erro  INTO l_mensagem_erro SEPARATED BY ' '.
      MESSAGE l_mensagem_erro TYPE 'I'.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_valida_parametros_erase.                                  *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_valida_parametros_erase.

  IF p_table IS INITIAL OR ( p_table(1) <> 'z' AND p_table(1) <> 'Z' ) .
    MESSAGE 'Digite o nome de uma tabela z existente e segura para deletar os registros.' TYPE 'I'.
  ELSE.
    PERFORM zf_deleta_registros.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_deleta_registros                                           *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_deleta_registros.

  CONCATENATE 'Deseja realmente excluir os registros da tabela' p_table '?' INTO l_mensagem SEPARATED BY ' '.

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

    IF p_table <> 'ZESTUDO_EMPRESAS'.
      EXIT.
    ENDIF.

    l_tabname = p_table.
    DELETE FROM (l_tabname).

    IF sy-dbcnt = 0.
      MESSAGE 'Nenhuma alteração foi realizada na tabela!' TYPE 'I'.
    ELSE.
      MESSAGE 'Registros deletados com sucesso!' TYPE 'I'.
    ENDIF.
  ENDIF.

ENDFORM.