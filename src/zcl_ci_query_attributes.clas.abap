class ZCL_CI_QUERY_ATTRIBUTES definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_SCI_ATTENT,
        kind         TYPE  sychar01,
        ref          TYPE  sci_refdat,
        text         TYPE  sychar40,
        obligatory   TYPE  flag,
        button_group TYPE  sychar04,
        id           TYPE  string,
        ref_field    TYPE  sychar200,
        search_help  TYPE  shlpname,
      END OF ty_SCI_ATTENT .
  types:
    tyt_SCI_ATTTAB TYPE TABLE OF ty_SCI_ATTENT WITH DEFAULT KEY .
  types:
    BEGIN OF ty_versioned_legacy_attributes,
        version           TYPE string,
        legacy_attributes TYPE tyt_SCI_ATTTAB,
      END OF ty_versioned_legacy_attributes .

  constants:
      "! Allowed values for the KIND field of the {@link SCI_ATTENT} structure used in the interface of {@link CL_CI_QUERY_ATTRIBUTES.METH:GENERIC}.
    BEGIN OF c_attribute_kinds,
        "! An elementary type with no special display properties
        elementary    TYPE sychar01 VALUE '',
        "! An elementary type representing a boolean value that should be displayed as a checkbox
        checkbox      TYPE sychar01 VALUE 'C',
        "! Irrelevant type, only the description this attribute will be shown on the UI, but no entry field of any kind
        dummy         TYPE sychar01 VALUE 'G',
        "! An elementary type representing a boolean value that should be displayed as a radio button exclusive with other buttons in the same {@link SCI_ATTENT.BUTTON_GROUP}
        radio_button  TYPE sychar01 VALUE 'R',
        "! A table type whose line type is the range of an elementary type
        select_option TYPE sychar01 VALUE 'S',
        "! A table type whose line type is an elementary type
        table         TYPE sychar01 VALUE 'T',
        "! An elementary type whose underlying ABAP dictionary domain has fixed values that should be displayed in a drop-down list box
        list_box      TYPE sychar01 VALUE 'L',
      END OF   c_attribute_kinds .

    "! <p>Generates and displays a modal dynpro subscreen for a list of attributes of a Code Inspector check</p>
    "! @parameter p_name | <p>Name of the check for which the screen is to be generated</p>
    "! @parameter p_title | <p>Title of the screen</p>
    "! @parameter p_attributes | <p>List of attributes. The user inputs on the screen will be directly written to the
    "! variables referenced by the data references in this table's REF column</p>
    "! @parameter p_message | <p>Message that the screen displays as a message of type 'I' when called. Intended to be used as an error message
    "! when the values set by the screen in a first call were not valid. </p>
    "! @parameter p_display | <p>If true, the screen is view-only</p>
    "! @parameter p_break | <p>If true, the user cancelled the screen</p>
  class-methods GENERIC
    importing
      !P_NAME type SCI_CHK
      !P_TITLE type C
      !P_ATTRIBUTES type TYT_SCI_ATTTAB
      !P_MESSAGE type C optional
      !P_DISPLAY type FLAG
    returning
      value(P_BREAK) type SYCHAR01 .
  class-methods CLASS_CONSTRUCTOR .
PROTECTED SECTION.
private section.

  types:
    t_checksum(30) TYPE c .
  types:
    BEGIN OF ENUM t_gui_state STRUCTURE gui_states,
      show_gui,
      get_attributes,
      set_attributes,
      after_set_attributes,
    END OF ENUM t_gui_state STRUCTURE gui_states .

  constants C_VERSION type SYCHAR04 value '0038' ##NO_TEXT.
  class-data GUI_STATE type T_GUI_STATE .
  class-data ATTRIBUTES type TYT_SCI_ATTTAB .
  class-data NEW_ATTRIBUTES type CL_CI_VARIANT_CONFIGURATION=>TY_ATTRIBUTE_MAP .
  class-data ERROR_MESSAGE type STRING .
  class-data MAPPING_TYPE type CL_CI_VARIANT_CONFIGURATION=>TY_MAPPING_TYPE .
  class-data ATTRIBUTE type TY_SCI_ATTENT .
  class-data VERSIONED_LEGACY_ATTRIBUTES type TY_VERSIONED_LEGACY_ATTRIBUTES .

  class-methods GENERATE_CHECKSUM
    importing
      !P_TITLE type C
      !P_ATTRIBUTES type TYT_SCI_ATTTAB
    returning
      value(P_CHECKSUM_AS_STR) type T_CHECKSUM .
  class-methods GENERATE_PROGRAM
    importing
      !P_TITLE type C
      !P_ATTRIBUTES type TYT_SCI_ATTTAB
      !P_CHECKSUM type T_CHECKSUM
      !P_PROGRAM type PROGRAM
    exceptions
      NO_DDIC_TYPE
      SYNTAX_ERROR
      UNKNOWN_KIND
      TEXT_TOO_LONG .
  class-methods CRC64
    importing
      !P_INPUT type ANY
    changing
      !P_CHECKSUM type SCI_CRC64 .
  class-methods MAP_TO_LEGACY
    importing
      !P_LEGACY_ATTRIBUTES type TYT_SCI_ATTTAB
      !P_CHECK_NAME type SCI_CHK .
ENDCLASS.



CLASS ZCL_CI_QUERY_ATTRIBUTES IMPLEMENTATION.


  METHOD CLASS_CONSTRUCTOR.
    gui_state = gui_states-show_gui.
  ENDMETHOD.


  METHOD CRC64 .
    DESCRIBE FIELD p_input LENGTH DATA(l_length) IN BYTE MODE.
    CALL 'ABAP_CRC64'
         ID 'INPUT'  FIELD p_input
         ID 'LENGTH' FIELD l_length
         ID 'CRC1'   FIELD p_checksum-i1
         ID 'CRC2'   FIELD p_checksum-i2.                "#EC CI_CCALL
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_parameter_error.
    ENDIF.
  ENDMETHOD.


  METHOD GENERATE_CHECKSUM.
    DATA l_checksum TYPE sci_crc64.
    crc64( EXPORTING p_input    = c_version
           CHANGING  p_checksum = l_checksum ).
    crc64( EXPORTING p_input = p_title
           CHANGING  p_checksum = l_checksum ).
    LOOP AT p_attributes REFERENCE INTO DATA(l_attribute).
      crc64( EXPORTING p_input    = l_attribute->text
             CHANGING  p_checksum = l_checksum ).
      crc64( EXPORTING p_input    = l_attribute->kind
             CHANGING  p_checksum = l_checksum ).
      DATA(l_data_type) = cl_abap_typedescr=>describe_by_data_ref( l_attribute->ref ).
      crc64( EXPORTING p_input    = l_data_type->type_kind
             CHANGING  p_checksum = l_checksum ).
      crc64( EXPORTING p_input    = l_data_type->absolute_name
             CHANGING  p_checksum = l_checksum ).
    ENDLOOP.
    p_checksum_as_str = |{ l_checksum-i1 }{ l_checksum-i2 }|.
  ENDMETHOD.


  METHOD GENERATE_PROGRAM.

    DEFINE app.
      APPEND &1 TO l_source  ##NO_TEXT.
    END-OF-DEFINITION.
    DEFINE app_line.
      APPEND l_line TO l_source.
    END-OF-DEFINITION.

    DATA:
      l_source       TYPE sci_include,
      l_line         LIKE LINE OF l_source,
      l_obligatory   TYPE sychar20,
      l_ref_type     TYPE REF TO cl_abap_typedescr,
      l_message(240) TYPE c,                                "#EC NEEDED
      l_eline        TYPE i,                                "#EC NEEDED
      l_word         TYPE sychar30,                         "#EC NEEDED
      l_name         TYPE sychar08 VALUE 'X',
      l_for_name     TYPE sychar08 VALUE 'F',
      l_type_name    TYPE sychar30,
      l_title(82)    TYPE c,
      l_textpool     TYPE TABLE OF textpool,
      l_textpool_wa  TYPE textpool,
      l_group        TYPE string.


    CONCATENATE '*' p_checksum INTO l_line.
    APPEND l_line TO  l_source.

    CONCATENATE 'FUNCTION-POOL' p_program '.' INTO l_line SEPARATED BY ' '.
    app_line.
    app 'type-pools SSCR.                                  '.
    app 'data DISPLAY type sychar01.                       '.

    app '  data L_OPT_LIST type SSCR_OPT_LIST.              '.
    app '  data L_ASS      type SSCR_ASS.                   '.
    app '  data L_RESTRICT type SSCR_RESTRICT.              '.

    app 'load-of-program.                                    '.
    app '  clear L_OPT_LIST.                                 '.
    app '  move ''JUST_EQ''  to L_OPT_LIST-NAME.             '.
    app '  move ''X'' to L_OPT_LIST-OPTIONS-EQ.              '.
    app '  append L_OPT_LIST to l_RESTRICT-OPT_LIST_TAB.     '.


    app 'selection-screen begin of screen 0100 title title ' &
                                              'as window.'.
    LOOP AT p_attributes ASSIGNING FIELD-SYMBOL(<l_attribute>).
      UNPACK sy-tabix TO l_name+1(*).


      IF <l_attribute>-obligatory = abap_true.
        l_obligatory = 'OBLIGATORY'.
      ELSE.
        CLEAR l_obligatory.
      ENDIF.

      IF <l_attribute>-kind = 'G'.         " group / selection screen block
        IF l_group IS NOT INITIAL.
          CONCATENATE 'selection-screen end of block' l_group '.' INTO l_line SEPARATED BY ' '. "#EC NOTEXT

          app_line.
        ENDIF.
        l_group = l_name.
        CONCATENATE 'selection-screen begin of block' l_group 'with frame' "#EC NOTEXT
                   'title' l_name '.'
                   INTO l_line SEPARATED BY ' '.
        app_line.

      ELSE.

        l_ref_type =
          cl_abap_typedescr=>describe_by_data_ref( <l_attribute>-ref ).
        IF l_ref_type->absolute_name(6) <> '\TYPE='.
          RAISE no_ddic_type.
        ENDIF.

        l_type_name = l_ref_type->absolute_name+6(*).
        CASE l_ref_type->type_kind.
          WHEN cl_abap_typedescr=>typekind_num OR
               cl_abap_typedescr=>typekind_date OR
               cl_abap_typedescr=>typekind_packed OR
               cl_abap_typedescr=>typekind_time OR
               cl_abap_typedescr=>typekind_char OR
               cl_abap_typedescr=>typekind_hex OR
               cl_abap_typedescr=>typekind_int   OR
               cl_abap_typedescr=>typekind_int1  OR
               cl_abap_typedescr=>typekind_int2  OR
               cl_abap_typedescr=>typekind_int8  OR
               cl_abap_typedescr=>typekind_struct1.
            CONCATENATE 'PARAMETER'
                        l_name
                        'TYPE'
                        l_type_name
                        l_obligatory
                    INTO l_line
                    SEPARATED BY ' '.
            app_line.
            CASE <l_attribute>-kind.
              WHEN 'C'. " checkbox
                app 'AS CHECKBOX modif id SO.'.

              WHEN 'R'.  " radiobutton
                IF <l_attribute>-button_group IS INITIAL.
                  l_line = 'radiobutton group G000 modif id SO.'. "#EC NOTEXT
                ELSE.
                  CONCATENATE
                    'radiobutton group' <l_attribute>-button_group 'modif id SO.' "#EC NOTEXT
                    INTO l_line
                    SEPARATED BY space.
                ENDIF.
                app l_line.

              WHEN 'L'.  " (Listbox).
                CONCATENATE
                  'as listbox visible length 20' 'modif id SO.' "#EC NOTEXT
                  INTO l_line
                  SEPARATED BY space.
                app l_line.

              WHEN OTHERS.
                app 'modif id SO.'.
            ENDCASE.

          WHEN cl_abap_typedescr=>typekind_table.

            IF <l_attribute>-kind = 'S'.              "Select-Option

              lcl_code_generator=>render_select_option(
                EXPORTING p_attribute =     <l_attribute>
                          p_table_handle =  CAST cl_abap_tabledescr( l_ref_type )
                          p_name_postfix =  l_name+1
                          p_screen_option = l_obligatory
                CHANGING  p_source =        l_source ).

            ELSE.
              l_for_name+1 = l_name+1.
              CONCATENATE 'DATA'
                          l_for_name
                          'TYPE LINE OF'
                          l_type_name
                          '.'
                      INTO l_line
                      SEPARATED BY ' '.
              app_line.
              CONCATENATE 'SELECT-OPTIONS'
                          l_name
                          'FOR'
                          l_for_name
                          l_obligatory
                          'NO INTERVALS MODIF ID SO.'
                      INTO l_line
                      SEPARATED BY ' '.
              app_line.




            ENDIF.
          WHEN OTHERS.
            RAISE unknown_kind.
        ENDCASE.
      ENDIF.
    ENDLOOP.
    IF l_group IS NOT INITIAL.
      CONCATENATE 'selection-screen end of block' l_group '.' INTO l_line SEPARATED BY ' '. "#EC NOTEXT

      app_line.
    ENDIF.

    app 'selection-screen end of screen 0100.                       '.


    app 'at selection-screen output.                                '.
    app '  loop at screen.                                          '.
    app '    if SCREEN-GROUP1 = ''SO''.                             '.
    app '      if SCREEN-NAME CS ''%-VALU_PUSH''.                   '.
    app '        SCREEN-INPUT = ''1''.                              '.
    app '      elseif SCREEN-NAME CS ''%-OPTI_PUSH''.               '.
    app '        SCREEN-INPUT = ''0''.                              '.
    app '      else.                                                '.
    app '        if DISPLAY = ''X''.                                '.
    app '          SCREEN-INPUT = ''0''.                            '.
    app '        else.                                              '.
    app '          SCREEN-INPUT = ''1''.                            '.
    app '        endif.                                             '.
    app '      endif.                                               '.
    app '    endif.                                                 '.
    app '    modify screen.                                         '.
    app '  endloop.                                                 '.
    app 'call function ''RS_EXTERNAL_SELSCREEN_STATUS''             '.
    app '   exporting                                               '.
    app '      P_FB          = ''SCI_QUERY_SET_GUI_STATUS''.        '.
    app 'call function ''SELECT_OPTIONS_RESTRICT''                  '.
    app '  exporting restriction = L_restrict                       '.
    app '            program     = SY-REPID                         '.
    app '  exceptions repeated   = 0                                '.
    app '             others     = 1.                               '.
    app 'assert SY-SUBRC = 0.                                       '.
*    app 'form SEL_SCREEN using P_ATTRIBUTES type SCI_ATTTAB '.
    app 'form SEL_SCREEN using P_ATTRIBUTES type ZCL_CI_QUERY_ATTRIBUTES=>tyt_sci_atttab'.
    app '                      P_MESSAGE    type C          '.
    app '                      P_DISPLAY    type C          '.
    app '                      P_BREAK      type SYCHAR01.  '.
    app '  data L_NAME(8)   type C value ''X''.             '.
    app '  data L_TAB(10)   type C.                         '.
    app '  data L_ATTRIBUTE like line of P_ATTRIBUTES.      '.
    app '  data L_TYPE(1)   type c.                         '.



    app '  field-symbols <L_FS_1> type ANY.                  '.
    app '  field-symbols <L_FS_2> type ANY.                  '.

    l_title = p_title.
    REPLACE '''' WITH '''''' INTO l_title.
    CONCATENATE '''' l_title ''''     INTO l_title.
    CONCATENATE 'TITLE =' l_title '.' INTO l_line
                 SEPARATED BY ' '.
    app_line.
    app '  p_break = '' ''.                                  '.
    app '  loop at P_ATTRIBUTES into L_ATTRIBUTE.            '.
    app '    unpack SY-TABIX           to L_NAME+1(*).       '.
    app '    if L_ATTRIBUTE-KIND = ''G''.                    '.
    app '       assign L_ATTRIBUTE-TEXT to <L_FS_2>.         '.
    app '    else.                                           '.
    app '      assign L_ATTRIBUTE-REF->* to <L_FS_2>.        '.
    app '    endif.                                          '.


    app '    describe field <l_fs_2> type l_type.            '.
    app '    if L_TYPE = ''h''.                              '.
    app '      concatenate l_name ''[]'' into l_tab.         '.
    app '      assign (L_TAB) to <L_FS_1>.                   '.
    app '      if L_ATTRIBUTE-KIND = ''S''.                  '.
    app '        <l_fs_1> = <l_fs_2>.                        '.
    app '      else.                                         '.
    app '        perform TAB_TO_SELOPT using <l_fs_2> <l_fs_1>.'.
    app '      clear L_ASS.                                    '.
    app '      move: ''S''            to L_ASS-KIND.           '.
    app '      move L_NAME            to l_ASS-NAME.           '.
    app '      move: ''I''            to L_ASS-SG_MAIN,        '.
    app '            '' ''            to L_ASS-SG_ADDY,        '.
    app '            ''JUST_EQ''      to L_ASS-OP_MAIN.        '.
    app '      append L_ASS to L_RESTRICT-ASS_TAB.             '.
    app '      endif.                                        '.
    app '    else.                                           '.
    app '      assign (L_NAME) to <L_FS_1>.                  '.
    app '      <L_FS_1> = <L_FS_2>.                          '.
    app '    endif.                                          '.


    app '  endloop.                                          '.
    app '  if p_message is not initial.                      '.
    app '    message s000(38) with p_message.                '.
    app '  endif.                                            '.
    app '  DISPLAY = P_DISPLAY.                              '.



    app '  call selection-screen 0100 starting at 1 1.       '.
    app '  if sy-subrc <> 0.                                 '.
    app '    p_break = ''X''.                                '.
    app '    return.                                         '.
    app '  endif.                                            '.
    app '  check P_DISPLAY = '' ''.                          '.
    app '  loop at P_ATTRIBUTES into L_ATTRIBUTE.            '.
    app '    unpack SY-TABIX           to L_NAME+1(*).       '.
    app '    assign L_ATTRIBUTE-REF->* to <L_FS_2>.          '.
    app '    describe field <l_fs_2> type l_type.            '.
    app '    if L_TYPE = ''h''.                              '.
    app '      concatenate l_name ''[]'' into l_tab.         '.
    app '      assign (L_TAB) to <L_FS_1>.                   '.
    app '      if L_ATTRIBUTE-KIND = ''S''.                  '.
    app '        <l_FS_2> = <l_FS_1>.                        '.
    app '      else.                                         '.
    app '        perform SELOPT_TO_TAB   using <l_FS_1> <l_FS_2>.'.
    app '      endif.                                        '.
    app '    elseif L_ATTRIBUTE-KIND <> ''G''.               '.
    app '      assign (L_NAME) to <L_FS_1>.                  '.
    app '      <L_FS_2> = <L_FS_1>.                          '.
    app '    endif.                                          '.
    app '  endloop.                                          '.
    app 'endform.                                            '.
    app '                                                    '.
    app 'form TAB_TO_SELOPT using p_tab    type TABLE        '.
    app '                         p_selopt type TABLE.       '.
    app '  field-symbols:                                    '.
    app '    <l_tab_wa>    TYPE any,                         '.
    app '    <l_selopt_wa> TYPE any,                         '.
    app '    <l_fs>        TYPE any.                         '.
    app '  data                                              '.
    app '    REF type ref to DATA.                           '.
    app '  create data ref like line of p_selopt.            '.
    app '  assign REF->* to <l_selopt_wa>.                   '.
    app '  assign component ''SIGN'' of structure <L_SELOPT_WA>'.
    app '         to <l_fs>.                                 '.
    app '  <l_fs> = ''I''.                                   '.
    app '  assign component ''OPTION'' of structure <L_SELOPT_WA>'.
    app '         to <l_fs>.                                 '.
    app '  <l_fs> = ''EQ''.                                  '.
    app '  assign component ''LOW'' of structure <L_SELOPT_WA>'.
    app '         to <l_fs>.                                 '.
    app '  clear p_selopt.                                   '.
    app '  loop at p_tab assigning <L_TAB_WA>.               '.
    app '    <l_fs> = <l_tab_wa>.                            '.
    app '    append <l_selopt_wa> to p_selopt.               '.
    app '  endloop.                                          '.
    app 'endform.                                            '.
    app 'form SELOPT_TO_TAB using p_selopt type TABLE        '.
    app '                         p_tab    type TABLE.       '.
    app '  field-symbols:                                    '.
    app '    <l_selopt_wa> TYPE any,                         '.
    app '    <l_fs>        TYPE any.                         '.
    app '  clear P_TAB.                                      '.
    app '  loop at p_SELOPT assigning <L_SELOPT_WA>.         '.
    app '    assign component ''LOW'' of structure <L_SELOPT_WA>'.
    app '         to <l_fs>.                                 '.
    app '    append <l_fs> to p_tab.                         '.
    app '  endloop.                                          '.
    app 'endform.                                            '.
    app '                                                    '.
    INSERT REPORT p_program FROM l_source.
    GENERATE REPORT p_program MESSAGE l_message LINE l_eline WORD l_word.
                                                       "#EC CI_GENERATE
    IF sy-subrc <> 0.
      RAISE syntax_error.
    ENDIF.
    LOOP AT p_attributes ASSIGNING FIELD-SYMBOL(<l_attribute_2>).
      UNPACK sy-tabix TO l_name+1(*).
      l_textpool_wa-id      = 'S'.
      l_textpool_wa-key     = l_name.
      l_textpool_wa-entry+8 = <l_attribute_2>-text.
      l_textpool_wa-length  = strlen( <l_attribute_2>-text ) + 8.
      APPEND l_textpool_wa TO l_textpool.
    ENDLOOP.
    INSERT TEXTPOOL p_program FROM l_textpool LANGUAGE sy-langu.
    COMMIT WORK.
  ENDMETHOD.


  METHOD GENERIC.
    CASE gui_state.
      WHEN gui_states-show_gui.
        DATA(l_program) = CONV program( |%__{ p_name+3(*) }| ).
        DATA(l_checksum) = generate_checksum( p_title      = p_title
                                              p_attributes = p_attributes ).
        DATA l_source TYPE string_table.
        READ REPORT l_program INTO l_source.
        IF sy-subrc = 0.
          DATA(l_first_line) = l_source[ 1 ].
          IF l_first_line+1 <> l_checksum.
            generate_program( p_program    = l_program
                              p_title      = p_title
                              p_checksum   = l_checksum
                              p_attributes = p_attributes ).
          ENDIF.
        ELSE.
          generate_program( p_program    = l_program
                            p_title      = p_title
                            p_checksum   = l_checksum
                            p_attributes = p_attributes ).
        ENDIF.
        PERFORM sel_screen IN PROGRAM (l_program)
                USING    p_attributes
                         p_message
                         p_display
                CHANGING p_break.

    ENDCASE.
  ENDMETHOD.


  METHOD MAP_TO_LEGACY.
*    NEW cl_ci_variant_configuration( )->map_to_legacy( new_attributes = new_attributes
*                                                    legacy_attributes = p_legacy_attributes
*                                                         mapping_type = mapping_type
*                                                           check_name = p_check_name ).
  ENDMETHOD.
ENDCLASS.
