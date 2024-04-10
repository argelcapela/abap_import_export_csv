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
DATA: y_estrutura_tabela_str TYPE REF TO cl_abap_structdescr,
      y_estrutura_tabela     TYPE REF TO cl_abap_structdescr.

*----------------------------------------------------------------------*
* Tabela Interna                                                       *
*----------------------------------------------------------------------*
DATA: t_arquivo_csv TYPE REF TO cl_abap_tabledescr,
      t_tabela_csv  TYPE TABLE OF zestudo_empresas,
      t_raw_data    TYPE truxs_t_text_data,
      t_filetable   TYPE filetable.

*----------------------------------------------------------------------*
* Work Area                                                            *
*----------------------------------------------------------------------*
DATA: w_tabela_csv TYPE zestudo_empresas,
      w_row        TYPE REF TO data,
      w_filetable  LIKE LINE OF t_filetable,
      w_data       TYPE REF TO data.

*----------------------------------------------------------------------*
* Variáveis                                                            *
*----------------------------------------------------------------------*
DATA: l_delimiter       TYPE cl_rsda_csv_converter=>char VALUE '"',
      l_field_separator TYPE cl_rsda_csv_converter=>char VALUE ',',
      l_message         TYPE string,
      l_error_message   TYPE string,
      l_rc              TYPE i, " código de retorno da seleção do arquivo csv
      l_resposta        TYPE c,
      l_tabname         TYPE dd02l-tabname,
      radio             TYPE i VALUE 2.

*----------------------------------------------------------------------*
* Field-Symbols                                                        *
*----------------------------------------------------------------------*
FIELD-SYMBOLS: <f_tab>             TYPE ANY TABLE,
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

*----------------------------------------------------------------------*
* Seletor de Arquivo/Pasta                                             *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  IF p_import = 'X'.
    PERFORM zf_abrir_selecao_arquivo.
  ELSE.
    PERFORM zf_abrir_selecao_pasta.
  ENDIF.

START-OF-SELECTION.

*----------------------------------------------------------------------*
* Execução                                                          *
*----------------------------------------------------------------------*
  IF p_import = 'X'.
    PERFORM zf_validar_parametros_import.
  ELSEIF p_export = 'X'.
    PERFORM zf_validar_parametros_export.
  ELSE.
    PERFORM zf_validar_parametros_erase.
  ENDIF.

*----------------------------------------------------------------------*
* FORMs                                                                *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form zf_abrir_selecao_arquivo                                       *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_abrir_selecao_arquivo.
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
    p_path = w_filetable-filename.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_abrir_selecao_pasta                                         *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_abrir_selecao_pasta.
  DATA: pasta TYPE string.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = 'Selecionar Pasta de exportação'
      initial_folder  = 'c:\'
    CHANGING
      selected_folder = pasta.

  IF sy-subrc = 0.
    p_path = pasta.
  ELSE.
    MESSAGE 'Erro ao seleciona diretório' TYPE 'I'.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form zf_validar_parametros_import.                                  *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_gera_tabela_str.

  DATA: l_nome_tabela_digitada TYPE ddobjname.
  l_nome_tabela_digitada = p_table.


  DATA: lt_dfies TYPE TABLE OF dfies.

  DATA: t_estrutura       TYPE cl_abap_structdescr=>component_table,
        w_campo_estrutura LIKE LINE OF t_estrutura.

  " Obtém os detalhes dos campos da tabela
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = l_nome_tabela_digitada  " Nome da tabela digitada pelo usuário
    TABLES
      dfies_tab = lt_dfies      " Tabela para armazenar detalhes dos campos
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  IF sy-subrc = 0.

* Prepara tabela de components t_estrutura para gerar a estrutura da tabela digitada com todos os campos do tipo string
    LOOP AT lt_dfies INTO DATA(ls_dfies).

      IF ls_dfies-fieldname = 'MANDT'.
        CONTINUE.
      ENDIF.
      w_campo_estrutura-name = ls_dfies-fieldname.
      w_campo_estrutura-type = cl_abap_elemdescr=>get_string( ).
      APPEND w_campo_estrutura TO t_estrutura.

    ENDLOOP.

* Cria estrutura dinamicamente
    y_estrutura_tabela_str = cl_abap_structdescr=>create( t_estrutura ).

    t_arquivo_csv = cl_abap_tabledescr=>create( p_line_type = y_estrutura_tabela_str
                                                p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                p_unique = abap_false ).

    TRY.
        CREATE DATA w_data TYPE HANDLE t_arquivo_csv.
      CATCH cx_sy_create_data_error.
    ENDTRY.

    TRY.
        ASSIGN w_data->* TO <f_tab>.
      CATCH cx_root.
    ENDTRY.

  ELSE.
    MESSAGE 'Tabela não existe!' TYPE 'I'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_validar_parametros_import.                                  *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_validar_parametros_import.

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
    PERFORM zf_gera_tabela_str.
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
*  ASSIGN t_arquivo_csv  TO <f_tab>.

* cria dinamicamente uma variável w_row que tem como tipo a estrutura de f_tab
  CREATE DATA w_row LIKE LINE OF <f_tab>.

* cria uma referência(ponteiro) para um registro da tabela t_arquivo_csv
  ASSIGN w_row->* TO <fw_tabela_csv_row>.

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

      INSERT <fw_tabela_csv_row> INTO TABLE <f_tab>.

    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_inserir_dados_na_tabela                                     *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_inserir_dados_na_tabela.

  LOOP AT <f_tab> ASSIGNING <fw_tabela_csv_row>.

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
      l_error_message = lo_exception->get_text( ).
      CONCATENATE TEXT-005
                  ' '
                  l_error_message
                  INTO l_message SEPARATED BY space.
      MESSAGE l_message TYPE 'I'.
      LEAVE LIST-PROCESSING.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_validar_parametros_export.                                  *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_validar_parametros_export.

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
    PERFORM zf_gera_tabela.
    PERFORM zf_exporta_tabela.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_gera_tabela.                                                *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_gera_tabela.

  DATA: l_nome_tabela_digitada TYPE ddobjname.
  l_nome_tabela_digitada = p_table.


  DATA: lt_dfies TYPE TABLE OF dfies.

  DATA: t_estrutura       TYPE cl_abap_structdescr=>component_table,
        w_campo_estrutura LIKE LINE OF t_estrutura.

  " Obtém os detalhes dos campos da tabela
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = l_nome_tabela_digitada  " Nome da tabela digitada pelo usuário
    TABLES
      dfies_tab = lt_dfies      " Tabela para armazenar detalhes dos campos
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  IF sy-subrc = 0.

* Prepara tabela de components t_estrutura para gerar a estrutura da tabela digitada com todos os campos do tipo string
    LOOP AT lt_dfies INTO DATA(ls_dfies).

      IF ls_dfies-fieldname = 'MANDT'.
        CONTINUE.
      ENDIF.
      w_campo_estrutura-name = ls_dfies-fieldname.

      TRY.
          w_campo_estrutura-type ?= cl_abap_elemdescr=>describe_by_name( ls_dfies-rollname ).
        CATCH cx_root.
          " Trate exceções conforme necessário
          w_campo_estrutura-type = cl_abap_elemdescr=>get_string( ). " Por padrão, defina como string em caso de falha ao criar o elemento
      ENDTRY.

      APPEND w_campo_estrutura TO t_estrutura.

    ENDLOOP.

* Cria estrutura dinamicamente
    y_estrutura_tabela_str = cl_abap_structdescr=>create( t_estrutura ).

    t_arquivo_csv = cl_abap_tabledescr=>create( p_line_type = y_estrutura_tabela_str
                                                p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                p_unique = abap_false ).

    TRY.
        CREATE DATA w_data TYPE HANDLE t_arquivo_csv.
      CATCH cx_sy_create_data_error.
    ENDTRY.

    TRY.
        ASSIGN w_data->* TO <f_tab>.

        l_tabname = p_table.

        SELECT *
          FROM (l_tabname)
          INTO CORRESPONDING FIELDS OF TABLE <f_tab>.

      CATCH cx_root.
    ENDTRY.

  ELSE.
    MESSAGE 'Tabela não existe!' TYPE 'I'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_exporta_tabela                                             *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM zf_exporta_tabela.

  DATA: lt_csv    TYPE TABLE OF string,
        lv_row    TYPE string,
        lv_string TYPE string.

  "CONSTRUCT THE TARGET TABLE FOR DOWNLOAD.SEPARATE VALUE WITH COMMAS
  LOOP AT <f_tab> ASSIGNING FIELD-SYMBOL(<fs_line>).

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fs_value>).
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
      IF sy-index EQ 1.
        lv_row = <fs_value>.
      ELSE.
        lv_string = <fs_value>.
        CONDENSE lv_string.
        CONCATENATE lv_row lv_string INTO lv_row SEPARATED BY l_field_separator.
      ENDIF.
    ENDDO.

    APPEND lv_row TO lt_csv.

  ENDLOOP.

  MESSAGE p_path TYPE 'I'.

  "DOWNLOAD THE TABLE INTO CSV FILE
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = p_path
    TABLES
      data_tab                = lt_csv
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