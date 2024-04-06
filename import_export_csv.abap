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
DATA: t_enterprise_csv TYPE TABLE OF y_enterprise,
      t_enterprise     TYPE TABLE OF ztb_enterprise,
      t_raw_data       TYPE truxs_t_text_data.

*----------------------------------------------------------------------*
* Work Area                                                            *
*----------------------------------------------------------------------*
DATA: w_enterprise TYPE ztb_enterprise,
      w_row        TYPE REF TO data.

*----------------------------------------------------------------------*
* Variáveis                                                            *
*----------------------------------------------------------------------*
DATA: l_delimiter       TYPE cl_rsda_csv_converter=>char VALUE '"',
      l_field_separator TYPE cl_rsda_csv_converter=>char VALUE ',',
      l_message         TYPE string,
      l_error_message   TYPE string.

*----------------------------------------------------------------------*
* Objetos                                                              *
*----------------------------------------------------------------------*
DATA(o_csv_converter) = cl_rsda_csv_converter=>create( i_delimiter = l_delimiter i_separator =
 l_field_separator ).

*----------------------------------------------------------------------*
* Field-Symbols                                                        *
*----------------------------------------------------------------------*
FIELD-SYMBOLS: <f_tab>             TYPE ANY TABLE,
               <fw_enterprise_row> TYPE any.

*----------------------------------------------------------------------*
* Selection Screen                                                     *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_path   TYPE string.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

*----------------------------------------------------------------------*
* Execução                                                             *
*----------------------------------------------------------------------*
  PERFORM zf_ler_csv.
  PERFORM zf_processar_csv.
  PERFORM zf_inserir_dados_na_tabela.

*----------------------------------------------------------------------*
* FORMs                                                                *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form zf_ler_csv                                                     *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_ler_csv.

* f_tab agora é uma referência(ponteiro) para t_enterprise_csv
  ASSIGN t_enterprise_csv  TO <f_tab>.

* cria dinamicamente uma variável w_row que tem como tipo a estrutura de f_tab
  CREATE DATA w_row LIKE LINE OF <f_tab>.

* cria uma referência(ponteiro) para um registro da tabela t_enterprise_csv
  ASSIGN w_row->* TO <fw_enterprise_row>.

* upload arquivo csv
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = p_path
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

  IF sy-subrc = 0.
*   Loop pelas linhas do arquivo csv
    LOOP AT t_raw_data INTO DATA(w_csv_line).

*     Separa linha e joga na estrutura fw_enterprise_row
      CALL METHOD o_csv_converter->csv_to_structure
        EXPORTING
          i_data   = w_csv_line
        IMPORTING
          e_s_data = <fw_enterprise_row>.

      INSERT <fw_enterprise_row> INTO TABLE t_enterprise_csv.

    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_inserir_dados_na_tabela                                     *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_inserir_dados_na_tabela.

  LOOP AT t_enterprise_csv INTO DATA(w_enterprise_csv).

    MOVE-CORRESPONDING w_enterprise_csv TO w_enterprise.
    APPEND w_enterprise TO t_enterprise.

  ENDLOOP.

  TRY.

*            Nome da Tabela que deseja carregar o arquivo CSV
      INSERT ztb_enterprise FROM TABLE t_enterprise.
      COMMIT WORK.

      MESSAGE TEXT-002 TYPE 'I' DISPLAY LIKE 'E'.

    CATCH cx_root INTO DATA(lo_exception).

      ROLLBACK WORK.
      l_error_message = lo_exception->get_text( ).
      CONCATENATE TEXT-003
                  l_error_message
                  INTO l_message SEPARATED BY space.
      MESSAGE l_message TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.

  ENDTRY.

ENDFORM.