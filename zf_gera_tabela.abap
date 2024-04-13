FUNCTION zf_gera_tabela.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_NOME_TABELA) TYPE  STRING
*"     VALUE(P_TIPO_DADO) TYPE  STRING
*"  CHANGING
*"     REFERENCE(T_TABELA_GERADA) TYPE REF TO  DATA
*"  EXCEPTIONS
*"      ZFGTE_TABELA_INVALIDA
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* Estrutura                                                            *
*----------------------------------------------------------------------*
  DATA: y_descricao_estrutura_gerada   TYPE REF TO cl_abap_structdescr.

*----------------------------------------------------------------------*
* Tabelas Interna                                                      *
*----------------------------------------------------------------------*
  DATA: t_descricao_tabela_gerada      TYPE REF TO cl_abap_tabledescr,
        t_componentes_estrutura_gerada TYPE cl_abap_structdescr=>component_table.

*----------------------------------------------------------------------*
* Work Area                                                            *
*----------------------------------------------------------------------*
  DATA: w_componentes_estrutura_gerada LIKE LINE OF t_componentes_estrutura_gerada.

*----------------------------------------------------------------------*
* Variável                                                             *
*----------------------------------------------------------------------*
  DATA: l_informacoes_tabela TYPE TABLE OF dfies,
        l_nome_tabela        TYPE ddobjname,
        l_root_exception     TYPE REF TO cx_root,
        l_mensagem_erro      TYPE string.

*----------------------------------------------------------------------*
* Execução                                                             *
*----------------------------------------------------------------------*
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

      IF w_informacao_tabela-fieldname = 'MANDT' AND p_tipo_dado <> 'final'.
        CONTINUE.
      ENDIF.

*         Define o nome dos campos da estrutura a ser gerada dinamicamente.
      w_componentes_estrutura_gerada-name = w_informacao_tabela-fieldname.

      IF p_tipo_dado = 'string'.
*           Define o elemento de dado dos campos da estrutura a ser gerada como string.
        w_componentes_estrutura_gerada-type = cl_abap_elemdescr=>get_string( ).
      ELSE.
*           Define o elemento de dado dos campos da estrutura a ser gerada,identificos a tabela informada.
        TRY.
            w_componentes_estrutura_gerada-type ?= cl_abap_elemdescr=>describe_by_name( w_informacao_tabela-rollname ).
          CATCH cx_root.
            w_componentes_estrutura_gerada-type = cl_abap_elemdescr=>get_string( ). " Por padrão, defina como string em caso de falha ao criar o elemento
        ENDTRY.
      ENDIF.

      APPEND w_componentes_estrutura_gerada TO t_componentes_estrutura_gerada.

    ENDLOOP.

*       Cria a descrição para a criação de uma estrutura dinamicamente, na tabela de descricao de componentes da estrutura..
    y_descricao_estrutura_gerada = cl_abap_structdescr=>create( t_componentes_estrutura_gerada ).

*       Cria a descrição para a criação de uma tabela interna dinamicamente, com base na estrutura gerada anteriormente.
    t_descricao_tabela_gerada = cl_abap_tabledescr=>create( p_line_type = y_descricao_estrutura_gerada
                                                            p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                            p_unique = abap_false ).

*       Cria uma tabela interna dinamicamente, com base na descricao da tabela, gerada anteriormente.
    CREATE DATA t_tabela_gerada TYPE HANDLE t_descricao_tabela_gerada.

  ELSE.
    MESSAGE 'Tabela Inválida' TYPE 'I'.
    RAISE ZFGTE_TABELA_INVALIDA.
  ENDIF.

ENDFUNCTION.
