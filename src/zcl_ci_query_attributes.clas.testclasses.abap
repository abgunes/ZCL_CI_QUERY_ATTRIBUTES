class LCL_TEST_CODE_GENERATOR definition final for testing risk level harmless.

  private section.
    methods:
      PASS_RANGE_WITH_DDIC_TYPE  for testing raising CX_DYNAMIC_CHECK,
      FAIL_RANGE_WITH_LOCAL_TYPE for testing raising CX_DYNAMIC_CHECK,
      FAIL_ITAB_WITHOUT_STRUCTURE for testing raising CX_DYNAMIC_CHECK,
      EXERCISE
        importing P_DATA type standard table
                  P_PASS type ABAP_BOOL.


endclass.

class LCL_TEST_CODE_GENERATOR implementation.

  method PASS_RANGE_WITH_DDIC_TYPE.
    data: L_DDIC_RANGE type range of SCI_SLINE.
    EXERCISE( P_DATA = L_DDIC_RANGE P_PASS = ABAP_TRUE ).
  endmethod.


  method FAIL_RANGE_WITH_LOCAL_TYPE.
    data: L_LOCAL_RANGE type range of I.
    EXERCISE( P_DATA = L_LOCAL_RANGE P_PASS = ABAP_FALSE ).
  endmethod.


  method FAIL_ITAB_WITHOUT_STRUCTURE.
    data: L_TEXTS type STRING_TABLE.
    EXERCISE( P_DATA = L_TEXTS P_PASS = ABAP_FALSE ).
  endmethod.


  method EXERCISE.
    data: L_TABLE_TYPE      type ref to CL_ABAP_TABLEDESCR.
    DATA: L_ATTRIBUTE       TYPE ZCL_CI_QUERY_ATTRIBUTES=>ty_sci_attent.
    data: L_SOURCE          type SCI_INCLUDE.

    L_TABLE_TYPE ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( P_DATA ).
    get reference of P_DATA into L_ATTRIBUTE-REF.

    if P_PASS = ABAP_TRUE.
      LCL_CODE_GENERATOR=>RENDER_SELECT_OPTION(
        exporting P_ATTRIBUTE = L_ATTRIBUTE
                  P_NAME_POSTFIX =          '1'
                  P_SCREEN_OPTION =   ''
                  P_TABLE_HANDLE = L_TABLE_TYPE
        changing  P_SOURCE = L_SOURCE ).
      CL_ABAP_UNIT_ASSERT=>ASSERT_NOT_INITIAL( L_SOURCE ).
    else.
      try.
          LCL_CODE_GENERATOR=>RENDER_SELECT_OPTION(
            exporting P_ATTRIBUTE = L_ATTRIBUTE
                      P_NAME_POSTFIX =          '1'
                      P_SCREEN_OPTION =   ''
                      P_TABLE_HANDLE = L_TABLE_TYPE ).
          CL_ABAP_UNIT_ASSERT=>FAIL(  ).
        catch LCX_BAD_TYPE ##no_Handler.
      endtry.
    endif.
  endmethod.

endclass.
