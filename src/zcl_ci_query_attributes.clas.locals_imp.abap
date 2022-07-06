CLASS lcx_parameter_error DEFINITION FINAL
    INHERITING FROM cx_no_check.
ENDCLASS.

CLASS lcx_bad_type DEFINITION FINAL INHERITING FROM cx_dynamic_check.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING p_previous TYPE REF TO cx_root OPTIONAL
                  p_type     TYPE REF TO cl_abap_typedescr
                  p_reason   TYPE csequence,

      if_message~get_text REDEFINITION.

  PRIVATE SECTION.
    DATA:
      reason_text TYPE string.

ENDCLASS.

CLASS lcx_bad_type IMPLEMENTATION.

  METHOD constructor.
    super->constructor( previous = p_previous ).

    IF p_type IS NOT INITIAL AND p_reason IS NOT INITIAL.
      reason_text = p_type->get_relative_name( ) && `: ` && p_reason.
    ELSEIF p_reason IS NOT INITIAL.
      reason_text = p_reason.
    ENDIF.
  ENDMETHOD.


  METHOD if_message~get_text.
    IF reason_text IS NOT INITIAL.
      result = reason_text.
    ELSE.
      result = super->if_message~get_text( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_code_generator DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS:
      render_select_option
        IMPORTING p_attribute     TYPE ZCL_CI_QUERY_ATTRIBUTES=>ty_sci_attent
                  p_table_handle  TYPE REF TO cl_abap_tabledescr
                  p_name_postfix  TYPE sychar07
                  p_screen_option TYPE csequence   OPTIONAL
        CHANGING  p_source        TYPE sci_include OPTIONAL
        RAISING   lcx_bad_type.

ENDCLASS.


CLASS lcl_code_generator IMPLEMENTATION.

  METHOD render_select_option.
    DATA:
      l_source_line LIKE LINE OF p_source,
      l_failure     TYPE REF TO cx_root,
      l_line_handle TYPE REF TO cl_abap_structdescr,
      l_low_handle  TYPE REF TO cl_abap_typedescr.

    DEFINE raise_bad_type.
      RAISE EXCEPTION TYPE lcx_bad_type
        EXPORTING p_previous = l_failure
                  p_reason   = &1
                  p_type     = p_table_handle.
    END-OF-DEFINITION.

    TRY.
        l_line_handle ?= p_table_handle->get_table_line_type( ).
      CATCH cx_sy_move_cast_error INTO l_failure.
        raise_bad_type 'SELECT-OPTION erfordert strukturierten Zeilentyp'(slt).
    ENDTRY.

    l_line_handle->get_component_type(
      EXPORTING  p_name = 'LOW'
      RECEIVING  p_descr_ref = l_low_handle
      EXCEPTIONS OTHERS = 0 ).
    IF l_low_handle IS INITIAL.
      raise_bad_type 'SELECT-OPTION erfordert Strukturkomponente LOW'(slo).
    ELSEIF l_low_handle->is_ddic_type( ) = abap_false.
      raise_bad_type 'Generierung erfordert DDIC-Typ'(dic).
    ENDIF.

    IF p_attribute-ref_field IS INITIAL.
          DATA(l_type_name) = l_low_handle->absolute_name.
      else.
    l_type_name = p_attribute-ref_field.
    ENDIF.

    REPLACE ALL OCCURRENCES OF '\TYPE=' IN l_type_name WITH space.
    l_source_line = |" { p_attribute-text }                                          |. INSERT l_source_line INTO TABLE p_source.
    l_source_line = |  DATA: F{ p_name_postfix } TYPE { l_type_name }.               |. INSERT l_source_line INTO TABLE p_source.
    l_source_line = |  SELECT-OPTIONS: X{ p_name_postfix } FOR F{ p_name_postfix }   |. INSERT l_source_line INTO TABLE p_source.

    IF p_attribute-search_help IS NOT INITIAL.
      l_source_line = |                MATCHCODE OBJECT { p_attribute-search_help } |. INSERT l_source_line INTO TABLE p_source.
    ENDIF.


    l_source_line = |                               MODIF ID SO { p_screen_option }. |. INSERT l_source_line INTO TABLE p_source.
    l_source_line = |  TYPES: T{ p_name_postfix } LIKE LINE OF X{ p_name_postfix }.  |. INSERT l_source_line INTO TABLE p_source.
    APPEND space TO p_source.

  ENDMETHOD.


ENDCLASS.
