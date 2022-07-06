*&---------------------------------------------------------------------*
*& Report ZDEMO_QUERY_ATTRIBUTES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_query_attributes.

DATA : BEGIN OF ls_fkart ,
         sign   TYPE  char1,
         option TYPE  char2,
         low    TYPE  fkart,
         high   TYPE  fkart,
       END OF ls_fkart,
       lt_fkart LIKE TABLE OF ls_fkart.

DATA: lv_bukrs      TYPE bukrs,
      " select-options: type range of.. (must be dictionary type)
      lrg_werks     TYPE ckf_werks_table,
      " select-options: table, separate values
      lt_werks      TYPE plants,
      " checkbox + radiobutton ( must be DDIC types )
      lv_simulate   TYPE xfeld,
      lv_mode_open  TYPE xfeld,
      lv_mode_close TYPE xfeld,
      lv_language   TYPE spras.

DATA : lv_fkart TYPE fkart.
lv_language = sy-langu.

zcl_ci_query_attributes=>generic(
  EXPORTING
    p_name       = CONV #( sy-repid )   " Selection Name
    p_title      = 'Generic screen for input'   " Title of Selection
    p_attributes = VALUE #(
    " parameter field
*  ( kind = 'S'  obligatory = abap_true  text = 'Company code'(001)  ref = REF #( lv_bukrs ) )
    " select-option
    ( kind = 'S'  text = 'Plant'(002)  ref = REF #( lrg_werks ) )
    ( kind = 'S'  text = 'Fkart10'(002)  ref = REF #( lt_fkart ) )
    ( kind = 'S'  text = 'Fkart1'(002)  ref = REF #( lt_fkart ) search_help = 'H_TVFK')
    ( kind = 'S'  text = 'Fkart2'(002)  ref = REF #( lt_fkart ) ref_field = 'TVFK-FKART')
    " selec-option no intervals
*  ( kind = 'T'  text = 'Additional plants'(008)  ref = REF #( lt_werks ) )
    " Screen groupd
*  ( kind = 'G'  text = 'Mode'(006)  ref = REF #( sy-index ) )
    " radiobuttons
*  ( kind = 'R'  text = 'Open'(004)  button_group = 'MOD'  ref = REF #( lv_mode_open ) )
*  ( kind = 'R'  text = 'Close'(005)  button_group = 'MOD'  ref = REF #( lv_mode_close ) )
    " checkbox field
*  ( kind = 'C'  text = 'Simulate'(003)  ref = REF #( lv_simulate ) )
    " listbox field
*  ( kind = 'L'  text = 'Language'(007)  ref = REF #( lv_language ) )
    )
    p_display    = abap_false    " General Flag
*  RECEIVING
*   p_break      =     " Terminate Yes/No
).
