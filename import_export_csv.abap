*------------------------------------------------------------------------------*
* Empresa: XXXXXX                                                              *
* Cliente: XXXXXX                                                              *
* Modulo: XX                                                                   *
* Titulo: XXXXXXXXXXXXX                                                        *
* Programa: ZIMPORT_EXPORT_CSV                                                 *
* Transação: XXXXXXX                                                           *
* Tipo Programa: XXXXXX                                                        *
* Funcional: XXXXXXXX                                                          *
* Desenvolvedor(a): Argel Capela dos Santos - ARSANTOS                         *
* Data Criação: 03/02/2024                                           	         *
*------------------------------------------------------------------------------*
*                           [HISTÓRICO]                                	       *
*------------------------------------------------------------------------------*
* Ult Modif   Autor          Chamado      Descrição                            *
* 03/02/2024  Argel Capela   XXXXXXXXXX   Codificação inicial                  *
*------------------------------------------------------------------------------*
REPORT zimport_export_csv.

*----------------------------------------------------------------------*
* Types                                                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF y_enterprise,
         company_id           TYPE string,
         company_name         TYPE string,
         company_country      TYPE string,
         company_sales        TYPE string,
         company_profits      TYPE string,
         company_assets       TYPE string,
         company_market_value TYPE string,
       END OF y_enterprise.

*----------------------------------------------------------------------*
* Tabela Interna                                                       *
*----------------------------------------------------------------------*
DATA: t_arquivo_csv TYPE TABLE OF y_enterprise,
      t_tabela_csv  TYPE TABLE OF ztb_enterprise2,
      t_raw_data    TYPE truxs_t_text_data,
      t_filetable   TYPE filetable.

*----------------------------------------------------------------------*
* Work Area                                                            *
*----------------------------------------------------------------------*
DATA: w_tabela_csv TYPE ztb_enterprise2,
      w_row        TYPE REF TO data,
      w_filetable  LIKE LINE OF t_filetable.

*----------------------------------------------------------------------*
* Variáveis                                                            *
*----------------------------------------------------------------------*
DATA: l_delimiter       TYPE cl_rsda_csv_converter=>char VALUE '"',
      l_field_separator TYPE cl_rsda_csv_converter=>char VALUE ',',
      l_message         TYPE string,
      l_error_message   TYPE string,
      l_rc              TYPE i, " código de retorno da seleção do arquivo csv
      l_resposta        TYPE c.

*----------------------------------------------------------------------*
* Objetos                                                              *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Field-Symbols                                                        *
*----------------------------------------------------------------------*
FIELD-SYMBOLS: <f_tab>             TYPE ANY TABLE,
               <fw_tabela_csv_row> TYPE any.

*----------------------------------------------------------------------*
* Selection Screen                                                     *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file  TYPE string,
              p_table TYPE string.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  PARAMETERS:
    p_import RADIOBUTTON GROUP rad  DEFAULT 'X',
    p_export RADIOBUTTON GROUP rad.

  PARAMETERS: p_erase  RADIOBUTTON GROUP rad.
  SELECTION-SCREEN COMMENT /1(40) TEXT-007.

SELECTION-SCREEN END OF BLOCK b2.

* Abre o seletor de arquivo
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title      = 'Arquivo CSV'
*     default_extension =
*     default_filename  =
      file_filter       = 'CSV Files (*.csv)|*.csv'
*     with_encoding     =
      initial_directory = 'c:\'
*     multiselection    =
    CHANGING
      file_table        = t_filetable
      rc                = l_rc
*     user_action       =
*     file_encoding     =
*  EXCEPTIONS
*     file_open_dialog_failed = 1
*     cntl_error        = 2
*     error_no_gui      = 3
*     not_supported_by_gui    = 4
*     others            = 5
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ELSE.
    READ TABLE t_filetable INTO w_filetable INDEX 1.
    p_file = w_filetable-filename.
  ENDIF.

*----------------------------------------------------------------------*
* Execução                                                             *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE 'X'.
    WHEN p_import.
      PERFORM zf_validar_parametros_import.

    WHEN p_export.
      MESSAGE 'exportar tabela para csv' TYPE 'I'.

   WHEN p_erase.
      PERFORM zf_validar_parametros_erase.

  ENDCASE.

START-OF-SELECTION.

*----------------------------------------------------------------------*
* FORMs                                                                *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form zf_validar_parametros_import.                                  *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_validar_parametros_import.

* Verifica se o parâmetro p_file está vazio
  IF p_file IS INITIAL.
    MESSAGE 'Selecione um arquivo csv valido.' TYPE 'I'.

* Verifica se o parâmetro p_table está vazio
  ELSEIF p_table IS INITIAL.
    MESSAGE 'Digite o nome de uma tabela autorizada para importação de arquivos CSV.' TYPE 'I'.

* Verifica se a tabela é uma tabela z
  ELSEIF p_table(1) <> 'z' AND p_table(1) <> 'Z'.
    MESSAGE 'Utilize esse programa apenas em tabelas Z.' TYPE 'I'.

  ELSE.

    PERFORM zf_ler_csv.
    PERFORM zf_processar_csv.
    PERFORM zf_inserir_dados_na_tabela.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_ler_csv                                                     *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_ler_csv.

* f_tab agora é uma referência(ponteiro) para t_arquivo_csv
  ASSIGN t_arquivo_csv  TO <f_tab>.

* cria dinamicamente uma variável w_row que tem como tipo a estrutura de f_tab
  CREATE DATA w_row LIKE LINE OF <f_tab>.

* cria uma referência(ponteiro) para um registro da tabela t_arquivo_csv
  ASSIGN w_row->* TO <fw_tabela_csv_row>.

* upload arquivo csv
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = p_file
      filetype = 'ASC'
    TABLES
      data_tab = t_raw_data.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_processar_csv                                               *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_processar_csv.
  DATA(o_csv_converter) = cl_rsda_csv_converter=>create( i_delimiter = l_delimiter i_separator =
 l_field_separator ).

  IF sy-subrc = 0.
*   Loop pelas linhas do arquivo csv
    LOOP AT t_raw_data INTO DATA(w_csv_line).

*     Separa linha e joga na estrutura fw_tabela_csv_row
      CALL METHOD o_csv_converter->csv_to_structure
        EXPORTING
          i_data   = w_csv_line
        IMPORTING
          e_s_data = <fw_tabela_csv_row>.

      INSERT <fw_tabela_csv_row> INTO TABLE t_arquivo_csv.

    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_inserir_dados_na_tabela                                     *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_inserir_dados_na_tabela.

  LOOP AT t_arquivo_csv INTO DATA(w_arquivo_csv).

    MOVE-CORRESPONDING w_arquivo_csv TO w_tabela_csv.
    APPEND w_tabela_csv TO t_tabela_csv.

  ENDLOOP.

  TRY.

*     Nome da Tabela que deseja carregar o arquivo CSV
      INSERT ztb_enterprise2 FROM TABLE t_tabela_csv.
      COMMIT WORK.

      MESSAGE TEXT-003 TYPE 'I'.

    CATCH cx_root INTO DATA(lo_exception).

      ROLLBACK WORK.
      l_error_message = lo_exception->get_text( ).
      CONCATENATE TEXT-003
                  l_error_message
                  INTO l_message SEPARATED BY space.
      MESSAGE l_message TYPE 'I'.
      LEAVE LIST-PROCESSING.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_validar_parametros_erase.                                  *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_validar_parametros_erase.

  IF p_table IS INITIAL OR ( p_table(1) <> 'z' AND p_table(1) <> 'Z' ) .
      MESSAGE 'Digite o nome de uma tabela z existente e segura para deletar os registros.' TYPE 'I'.
  ELSE.
      PERFORM zf_deletar_registros.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_deletar_registros                                           *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_deletar_registros.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Resetar Tabela'
      text_question         = 'Deseja realmente excluir os registros dessa tabela?'
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
    DELETE FROM ztb_enterprise2.

    IF sy-dbcnt = 0.
      MESSAGE 'Nenhuma alteração foi realizada na tabela!' TYPE 'I'.
    ELSE.
      MESSAGE 'Registros deletados com sucesso!' TYPE 'I'.
    ENDIF.
  ENDIF.

ENDFORM.