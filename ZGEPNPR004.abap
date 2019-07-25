REPORT zgepnpr004 NO STANDARD PAGE HEADING
                        LINE-SIZE 132
                        LINE-COUNT 65.
*                       MESSAGE-ID zhpa.
" Message id according to the module
* Change the line size line-count according to the report layout
*======================================================================*
*  Functional spec ID     :
*  Program ID             :  ZGEPNPR004
*  Program Description    :  Pensioners Exit Entry Processing- Update IT19
*
*  Functional Consultant  :
*  Created By             :
*  Start Date             :
*  End Date               :
*======================================================================*
*                     Modify Log History.
*----------------------------------------------------------------------*
*No.    Modified by    Date         Description
*---    -----------    ----         -----------
*M000    NCS_xxxx      DD.MM.YYYY   Change Description
*======================================================================*

* Global declarations for program specific ----------------------------*
*_ Copy out the below include -  The name would be <Your program name>_TOP.
INCLUDE zgepnpr004_top.
*INCLUDE zgecapi_shell_top.

* Global declarations for common objects ------------------------------*
INCLUDE zgecapi001.

* Reserved Include ----------------------------------------------------*
INCLUDE zgecapi002.

* ALV Subroutines -----------------------------------------------------*
*_ Remove INCLUDE zgecapi003 for classical reports
INCLUDE zgecapi003.

* Selection Screen ----------------------------------------------------*
SELECTION-SCREEN:  BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s00.
*
*_ Comment the below paramater and create as per the requirement
PARAMETERS: "p_mode TYPE flag USER-COMMAND ucm1,
  p_nric TYPE psg_idnum MODIF ID md1,
  p_date TYPE begda MODIF ID md1,
*            p_test TYPE flag,
  p_rad1 RADIOBUTTON GROUP rb1 MODIF ID md1,
  p_rad2 RADIOBUTTON GROUP rb1 MODIF ID md1.
*  p_crct TYPE flag MODIF ID md1.
*SELECTION-SCREEN SKIP.
*
*SELECT-OPTIONS:
*
*SELECTION-SCREEN SKIP.
*
SELECTION-SCREEN:  END OF BLOCK b1.

*======================================================================*
*                          INITIALIZATION
*======================================================================*
INITIALIZATION.

*_ Reserved
  PERFORM f_reserved_init.

*_ Initialization
  PERFORM f_initialization.

*======================================================================*
*                          AT SELECTION-SCREEN
*======================================================================*
AT SELECTION-SCREEN.

*_ Reserved
  PERFORM f_reserved_atss.

*_ Selection screen validations
  PERFORM f_validate_parameters.

*======================================================================*
*                          AT SELECTION-SCREEN OUTPUT
*======================================================================*
AT SELECTION-SCREEN OUTPUT.

*_ Reserved
  PERFORM f_reserved_aso.

*  PERFORM f_modify_sso.


*_ Remove this Event and related logic for ALV reports
*======================================================================*
*                          TOP-OF-PAGE
*======================================================================*
*TOP-OF-PAGE.
**_ Reserved
*  PERFORM f_reserved_top.

*======================================================================*
*                          START-OF-SELECTION
*======================================================================*
START-OF-SELECTION.

*_ To Print Selection screen
*_ Remove this subroutine for ALV reports
*  PERFORM f_print_selection_screen.

*_ Reserved
  PERFORM f_reserved_sos.


* Un comment this flag for all reports using PNPCE logical database
* pnp_sw_skip_pernr = 'N'.

* •	For HR reports avoid PNPPERNR use PNPINDEX.
* Good to populate PNPINDEX for better performance. Note - to be done before GET PERAS.
* GET PERAS " FOR PNPCE logical database

*_ Process Main logic
  PERFORM f_process_data.

*======================================================================*
*                          END-OF-SELECTION
*======================================================================*
END-OF-SELECTION.

*_ Reserved
  PERFORM f_reserved_eos.

*_ Un comment this subroutine for ALV reports
*_ Remove this subroutine and relevant logic for classical reports
*_ To Print ALV
  PERFORM f_print_alv.


*_ Print output
*_ Remove this subroutine for ALV reports
*  PERFORM f_print_report.

*======================================================================*
*                          End of Report Banner
*======================================================================*

*&---------------------------------------------------------------------*
*&      Form  F_INITIALIZATION
*&---------------------------------------------------------------------*
*       For Initialization
*----------------------------------------------------------------------*
FORM f_initialization .

*Types ----------------------------------------------------------------*
*TYPES: BEGIN OF lty_s_xxx,
*        xx TYPE xx,
*       END OF lty_s_xxx,
*       lty_t_xxx type table of lty_s_xxx.

*Variables ------------------------------------------------------------*
  DATA lv_hdr TYPE rs38m-repti.

*Internal Tables ------------------------------------------------------*
*DATA lt_xx TYPE STANDARD TABLE OF xx.

*Work area ------------------------------------------------------------*
*DATA ls_xx TYPE xx.

*Constans -------------------------------------------------------------*
*CONSTANTS gc_xx TYPE xx.

*Field Symbols --------------------------------------------------------*
*FIELD-SYMBOLS <lt_> TYPE STANDARD TABLE xx. " Internal Table
*FIELD-SYMBOLS <lv_> TYPE xx.                " Variavle

*Ranges ---------------------------------------------------------------*
*DATA lt_xx TYPE RANGE OF xx.

*References -----------------------------------------------------------*
*DATA lr_xx TYPE REF TO data.

*Object References ----------------------------------------------------*
*DATA: lo_xx TYPE REF TO cl_xx.

*------------------------Logic-----------------------------------------*

  lv_hdr = sy-title.          " Title first Line
  APPEND lv_hdr TO gt_hdr.

*  lv_hdr = 'Second Line'.     " Title second Line
*  APPEND lv_hdr TO gt_hdr.
*
*  lv_hdr = 'Third Line'.      " Title third Line
*  APPEND lv_hdr TO gt_hdr.
*
*  lv_hdr = 'Fourth Line'.     " Title fourth Line
*  APPEND lv_hdr TO gt_hdr.

ENDFORM.                    " F_INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  F_PRINT_HEADING
*&---------------------------------------------------------------------*
*       To Print page heading
*----------------------------------------------------------------------*
FORM f_print_heading .                                      "#EC NEEDED
*Types ----------------------------------------------------------------*
*TYPES: BEGIN OF lty_s_xxx,
*        xx TYPE xx,
*       END OF lty_s_xxx,
*       lty_t_xxx type table of lty_s_xxx.

*Variables ------------------------------------------------------------*
*  DATA lv_xxx TYPE xx.

*Internal Tables ------------------------------------------------------*
*DATA lt_xx TYPE STANDARD TABLE OF xx.

*Work area ------------------------------------------------------------*
*DATA ls_xx TYPE xx.

*Constans -------------------------------------------------------------*
*CONSTANTS gc_xx TYPE xx.

*Field Symbols --------------------------------------------------------*
*FIELD-SYMBOLS <lt_> TYPE STANDARD TABLE xx. " Internal Table
*FIELD-SYMBOLS <lv_> TYPE xx.                " Variavle

*Ranges ---------------------------------------------------------------*
*DATA lt_xx TYPE RANGE OF xx.

*References -----------------------------------------------------------*
*DATA lr_xx TYPE REF TO data.

*Object References ----------------------------------------------------*
*DATA: lo_xx TYPE REF TO cl_xx.

*------------------------Logic-----------------------------------------*


ENDFORM.                    " F_PRINT_HEADING

*&---------------------------------------------------------------------*
*&      Form  F_PRINT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       To Print Selection Screen
*----------------------------------------------------------------------*
FORM f_print_selection_screen .

*Types ----------------------------------------------------------------*
*TYPES: BEGIN OF lty_s_xxx,
*        xx TYPE xx,
*       END OF lty_s_xxx,
*       lty_t_xxx type table of lty_s_xxx.

*Variables ------------------------------------------------------------*
*  DATA lv_xxx TYPE xx.

*Internal Tables ------------------------------------------------------*
*DATA lt_xx TYPE STANDARD TABLE OF xx.

*Work area ------------------------------------------------------------*
*DATA ls_xx TYPE xx.

*Constans -------------------------------------------------------------*
*CONSTANTS gc_xx TYPE xx.

*Field Symbols --------------------------------------------------------*
*FIELD-SYMBOLS <lt_> TYPE STANDARD TABLE xx. " Internal Table
*FIELD-SYMBOLS <lv_> TYPE xx.                " Variavle

*Ranges ---------------------------------------------------------------*
*DATA lt_xx TYPE RANGE OF xx.

*References -----------------------------------------------------------*
*DATA lr_xx TYPE REF TO data.

*Object References ----------------------------------------------------*
*DATA: lo_xx TYPE REF TO cl_xx.

*------------------------Logic-----------------------------------------*

* To Print Selection screen
  CALL FUNCTION 'ZGECAFM_SL_PRT_SEL_SCR'
    EXPORTING
      iv_report_name = sy-cprog
*     ORGUNIT_IND    =
    .

  NEW-PAGE .
ENDFORM.                    " F_PRINT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_DATA
*&---------------------------------------------------------------------*
*       For Main Processing logic
*----------------------------------------------------------------------*
FORM f_process_data .                                       "#EC NEEDED
**Types ----------------------------------------------------------------*
**TYPES: BEGIN OF lty_s_xxx,
**        xx TYPE xx,
**       END OF lty_s_xxx,
**       lty_t_xxx type table of lty_s_xxx.
*
**Variables ------------------------------------------------------------*
  DATA:
    lv_endda     TYPE endda,
    lv_months    TYPE vtbbewe-atage,
    lv_operation TYPE pspar-actio.

*
**Internal Tables ------------------------------------------------------*
  DATA:
    lt_pa0185     TYPE STANDARD TABLE OF pa0185,
    lt_pa0185_tmp TYPE STANDARD TABLE OF pa0185,
    lt_p0019      TYPE STANDARD TABLE OF p0019,
    lt_pa0000     TYPE STANDARD TABLE OF p0000,
    lt_pa0001     TYPE STANDARD TABLE OF p0001,
    lt_pa0014     TYPE STANDARD TABLE OF p0014,
    lt_pa0019     TYPE STANDARD TABLE OF pa0019,
    lt_ytest      TYPE STANDARD TABLE OF zsrir_test,
    lt_ytest_2    TYPE STANDARD TABLE OF zsrir_test,
    lt_ytest_3    TYPE STANDARD TABLE OF zsrir_test,
    lt_ytest_4    TYPE STANDARD TABLE OF zsrir_test.
*
**Work area ------------------------------------------------------------*
  DATA:
    ls_pa0019    TYPE pa0019,
    ls_p0019     TYPE p0019,
    ls_p0019_2   TYPE p0019,
    ls_pa0185    TYPE pa0185,
    lv_process   TYPE abap_bool,
    ls_return    TYPE bapireturn1,
    ls_ytest     TYPE zsrir_test,
    ls_ytest_tmp TYPE zsrir_test.
**------------------------Logic-----------------------------------------*

  IF sy-batch EQ abap_true.
    SELECT * FROM zsrir_test
             INTO TABLE lt_ytest.
    IF sy-subrc EQ 0.
      lt_ytest_4 = lt_ytest.
      SORT lt_ytest BY nric begda DESCENDING.
      DELETE lt_ytest WHERE endda IS NOT INITIAL.

      SORT lt_ytest_4 BY nric endda DESCENDING.
      DELETE lt_ytest_4 WHERE begda IS NOT INITIAL.

      LOOP AT lt_ytest INTO ls_ytest.
        READ TABLE lt_ytest_4 INTO ls_ytest_tmp
          WITH KEY nric = ls_ytest-nric
                   endda = ls_ytest-begda
          BINARY SEARCH.
        IF sy-subrc NE 0.
          APPEND ls_ytest TO lt_ytest_2.
        ENDIF.
      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM lt_ytest_2 COMPARING nric.

      LOOP AT lt_ytest_4 INTO ls_ytest_tmp.
        READ TABLE lt_ytest INTO ls_ytest
          WITH KEY nric = ls_ytest_tmp-nric
                   begda = ls_ytest_tmp-endda
          BINARY SEARCH.
        IF sy-subrc NE 0.
          APPEND ls_ytest_tmp TO lt_ytest_3.
        ENDIF.
      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM lt_ytest_3 COMPARING nric.

      CLEAR lt_ytest.
      APPEND LINES OF lt_ytest_2 TO lt_ytest.
      APPEND LINES OF lt_ytest_3 TO lt_ytest.
      SORT lt_ytest BY nric begda.

    ENDIF.
  ELSE.
    IF p_nric IS INITIAL OR p_date IS INITIAL.
      MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    ls_ytest-nric = p_nric.

    CASE abap_true.
      WHEN p_rad1.
        ls_ytest-begda = p_date.
      WHEN p_rad2.
        ls_ytest-endda = p_date.
      WHEN OTHERS.
    ENDCASE.

    APPEND ls_ytest TO lt_ytest.
  ENDIF.

  IF lt_ytest IS NOT INITIAL.
    SELECT
      *
      FROM pa0185 INTO TABLE lt_pa0185
      FOR ALL ENTRIES IN lt_ytest
      WHERE icnum EQ lt_ytest-nric.
    IF sy-subrc EQ 0.
      SORT lt_pa0185 BY icnum.

      lt_pa0185_tmp = lt_pa0185.
      SORT lt_pa0185_tmp BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pa0185_tmp COMPARING pernr.

      SELECT
        *
        FROM pa0019
        INTO TABLE lt_pa0019
        FOR ALL ENTRIES IN lt_pa0185_tmp
        WHERE pernr EQ lt_pa0185_tmp-pernr
          AND subty EQ gc_subty_p1.
      IF sy-subrc EQ 0.
        SORT lt_pa0019 BY pernr ASCENDING
                          begda DESCENDING.
      ENDIF.
    ENDIF.

    LOOP AT lt_ytest INTO ls_ytest.
      READ TABLE lt_pa0185 INTO ls_pa0185
        WITH KEY icnum = ls_ytest-nric
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        PERFORM validate_pensioner USING ls_pa0185.

        IF gv_flag EQ abap_true.
          READ TABLE lt_pa0019 INTO ls_pa0019
            WITH KEY pernr = ls_pa0185-pernr
            BINARY SEARCH.
          IF sy-subrc NE 0.
            CLEAR ls_pa0019.
          ENDIF.

          CLEAR ls_p0019.

          IF ls_pa0019 IS NOT INITIAL.
            IF ls_pa0019-bvmrk EQ gc_bvmrk_tip.
              IF ls_ytest-begda IS NOT INITIAL.
                "Double Exit from Singapore (without Entry)
                ls_p0019-pernr = ls_pa0185-pernr.
                ls_p0019-subty = gc_subty_p1.
                ls_p0019-tmart = gc_tmart_p1.
                ls_p0019-bvmrk = gc_bvmrk_tip.
                ls_p0019-begda = ls_ytest-begda.
                ls_p0019-endda = gc_eod_9999. "ls_ytest-begda.
                ls_p0019-mndat = ls_ytest-begda.
                ls_p0019-termn = ls_ytest-begda.
                lv_operation   = gc_opr_ins.
                lv_process = abap_true.
                lv_endda = gc_eod_9999.
              ELSE.
                "Normal Entry to Singapore (Update existing IT0019)
                ls_p0019-pernr = ls_pa0185-pernr.
                ls_p0019-subty = gc_subty_p1.
                ls_p0019-tmart = gc_tmart_p1.
                ls_p0019-bvmrk = gc_bvmrk_tc.
                ls_p0019-begda = ls_pa0019-begda.
                ls_p0019-endda = ls_ytest-endda. "ls_pa0019-endda.
                ls_p0019-mndat = ls_ytest-endda.
                ls_p0019-termn = ls_pa0019-termn.
                lv_operation   = gc_opr_upd.
                lv_process = abap_true.
                lv_endda = ls_pa0019-endda.
              ENDIF.
            ELSE.
              IF ls_ytest-begda IS NOT INITIAL.
                "Normal Exit from Singapore (Create new IT0019)
                ls_p0019-pernr = ls_pa0185-pernr.
                ls_p0019-subty = gc_subty_p1.
                ls_p0019-tmart = gc_tmart_p1.
                ls_p0019-bvmrk = gc_bvmrk_tip.
                ls_p0019-begda = ls_ytest-begda.
                ls_p0019-endda = gc_eod_9999. "ls_ytest-begda.
                ls_p0019-mndat = ls_ytest-begda.
                ls_p0019-termn = ls_ytest-begda.
                lv_operation   = gc_opr_ins.
                lv_process = abap_true.
                lv_endda = gc_eod_9999.
              ELSE.
                "Double entry without exit happened
                "Do nothing, this error will show in another discrepancy program
                lv_process = abap_false.
              ENDIF.
            ENDIF.
          ELSE.
            IF ls_ytest-begda IS NOT INITIAL.
              "Newly created Exit from Singapore
              ls_p0019-pernr = ls_pa0185-pernr.
              ls_p0019-subty = gc_subty_p1.
              ls_p0019-tmart = gc_tmart_p1.
              ls_p0019-bvmrk = gc_bvmrk_tip.
              ls_p0019-begda = ls_ytest-begda.
              ls_p0019-endda = gc_eod_9999. "ls_ytest-begda.
              ls_p0019-mndat = ls_ytest-begda.
              ls_p0019-termn = ls_ytest-begda.
              lv_operation   = gc_opr_ins.
              lv_process = abap_true.
              lv_endda = gc_eod_9999.
            ELSE.
              lv_process = abap_false.
              "newly created Entry to Singapore
              "Do nothing, this error will show in another discrepancy program
            ENDIF.
          ENDIF.

          IF lv_process EQ abap_true.
            ls_p0019-infty = '0019'.

            CALL METHOD zgecacl_hr_util=>mt_infotype_operation_single
              EXPORTING
                iv_pernr         = ls_p0019-pernr
                iv_infty         = '0019'
                iv_subtype       = ls_p0019-subty
                iv_validityend   = lv_endda "ls_p0019-endda
                iv_validitybegin = ls_p0019-begda
                iv_object_id     = ls_p0019-objps
                iv_operation     = lv_operation
                iv_nocommit      = abap_false
                is_record        = ls_p0019
              IMPORTING
                es_return        = ls_return.
            IF ls_return IS NOT INITIAL.

*          ELSEIF lv_operation EQ gc_opr_upd.
            ELSEIF ls_p0019-bvmrk EQ gc_bvmrk_tc.
              CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
                EXPORTING
                  i_date_from = ls_p0019-termn
                  i_date_to   = ls_p0019-mndat
                IMPORTING
                  e_months    = lv_months.

              IF lv_months GE '3'.
                PERFORM modify_entrance USING ls_p0019 lv_months.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
*      ENDIF.
  ENDIF.
*  ENDIF.

  PERFORM modify_exit USING sy-datum.

ENDFORM.                    " F_PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_PARAMETERS
*&---------------------------------------------------------------------*
*       For validating Selection screen parameters
*----------------------------------------------------------------------*
FORM f_validate_parameters .                                "#EC NEEDED
*Types ----------------------------------------------------------------*
*TYPES: BEGIN OF lty_s_xxx,
*        xx TYPE xx,
*       END OF lty_s_xxx,
*       lty_t_xxx type table of lty_s_xxx.

*Variables ------------------------------------------------------------*
*  DATA lv_xxx TYPE xx.

*Internal Tables ------------------------------------------------------*
*DATA lt_xx TYPE STANDARD TABLE OF xx.

*Work area ------------------------------------------------------------*
*DATA ls_xx TYPE xx.

*Constans -------------------------------------------------------------*
*CONSTANTS gc_xx TYPE xx.

*Field Symbols --------------------------------------------------------*
*FIELD-SYMBOLS <lt_> TYPE STANDARD TABLE xx. " Internal Table
*FIELD-SYMBOLS <lv_> TYPE xx.                " Variavle

*Ranges ---------------------------------------------------------------*
*DATA lt_xx TYPE RANGE OF xx.

*References -----------------------------------------------------------*
*DATA lr_xx TYPE REF TO data.

*Object References ----------------------------------------------------*
*DATA: lo_xx TYPE REF TO cl_xx.

*------------------------Logic-----------------------------------------*


ENDFORM.                    " F_VALIDATE_PARAMETERS

*&---------------------------------------------------------------------*
*&      Form  F_PRINT_ALV
*&---------------------------------------------------------------------*
*       Print ALV Report
*----------------------------------------------------------------------*
FORM f_print_alv .
*Types ----------------------------------------------------------------*
*TYPES: BEGIN OF lty_s_xxx,
*        xx TYPE xx,
*       END OF lty_s_xxx,
*       lty_t_xxx type table of lty_s_xxx.

*Variables ------------------------------------------------------------*
*  DATA lv_xxx TYPE xx.

*Internal Tables ------------------------------------------------------*
*DATA lt_xx TYPE STANDARD TABLE OF xx.

*Work area ------------------------------------------------------------*
*DATA ls_xx TYPE xx.

*Constans -------------------------------------------------------------*
*CONSTANTS gc_xx TYPE xx.

*Field Symbols --------------------------------------------------------*
*FIELD-SYMBOLS <lt_> TYPE STANDARD TABLE xx. " Internal Table
*FIELD-SYMBOLS <lv_> TYPE xx.                " Variavle

*Ranges ---------------------------------------------------------------*
*DATA lt_xx TYPE RANGE OF xx.

*References -----------------------------------------------------------*
*DATA lr_xx TYPE REF TO data.

*Object References ----------------------------------------------------*
*DATA: lo_xx TYPE REF TO cl_xx.

*------------------------Logic-----------------------------------------*

*"Assign report parameters for output
  PERFORM f_report_parameters.

*"Build field catalog, layout and sort structures
  PERFORM f_build_output_layout.

*"Report display
  PERFORM f_show_alv_rep TABLES gt_print.

ENDFORM.                    " F_PRINT_ALV
*&---------------------------------------------------------------------*
*&      Form  F_REPORT_PARAMETERS
*&---------------------------------------------------------------------*
*       To Pass Report Parameters
*----------------------------------------------------------------------*
FORM f_report_parameters .                                  "#EC NEEDED
*{
*For header information populate
*internal tables gt_header and gt_header_add

**"For LVC mode
*  gv_lvc_mode = 'X'.

* For interactive actions uncomment below code and
* write a code in form USER_COMMAND to handle user actions
*  gv_ucomm = gc_ucomm.
*}
ENDFORM.                    " F_REPORT_PARAMETERS

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_OUTPUT_LAYOUT
*&---------------------------------------------------------------------*
*       Build field catalog, layout and sort structures
*----------------------------------------------------------------------*
FORM f_build_output_layout.
*{
*"Prepare layout
  PERFORM f_layout.

*"Prepare sort table
  PERFORM f_sort_table.

*"Build events for top of page
*  PERFORM event_build.

*"Prepare field catalog
  PERFORM f_field_catalog.

*}
ENDFORM.                    " F_BUILD_OUTPUT_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
*&---------------------------------------------------------------------*
*      Set layout features
*----------------------------------------------------------------------*
FORM f_layout .
*{
* "Uncomment below code based on requirement
*  for additional features check for other parametes in gs_layout

  IF gv_lvc_mode IS NOT INITIAL.
*    gs_lvc_layout-cwidth_opt = 'X'.
  ELSE.
    gs_layout-colwidth_optimize = 'X'.
  ENDIF.
*}
ENDFORM.                    " F_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  F_SORT_TABLE
*&---------------------------------------------------------------------*
*       prepare sort table for sort and subtotals
*----------------------------------------------------------------------*
FORM f_sort_table .
*{
*Based on requirement build sort table
*Below is the sample code for reference
*Uncomment below code and pass the values

  IF gv_lvc_mode IS NOT INITIAL.
*    gs_sort_lvc-spos = '1'.
*    gs_sort_lvc-fieldname = 'PERNR'.
*    gs_sort_lvc-subtot    = ''.
*    APPEND gs_sort_lvc TO gt_sort_lvc.
*    CLEAR gs_sort_lvc.
*
*    gs_sort_lvc-spos = '1'.
*    gs_sort_lvc-fieldname = 'AEDTM'.
*    gs_sort_lvc-subtot    = ''.
*    APPEND gs_sort_lvc TO gt_sort_lvc.
*    CLEAR gs_sort_lvc.
  ELSE.
*  gs_sort-spos = '1'.
*  gs_sort-fieldname = 'PERNR'.
*  gs_sort-tabname   = 'GT_OUTPUT'.
*  gs_sort-subtot    = ''.
*  APPEND gs_sort TO gt_sort.
*  CLEAR gs_sort.
*
*  gs_sort-spos = '2'.
*  gs_sort-fieldname = 'AEDTM'.
*  gs_sort-tabname   = 'GT_OUTPUT'.
*  gs_sort-subtot    = ''.
*  APPEND gs_sort TO gt_sort.
*  CLEAR gs_sort.
  ENDIF.
*}
ENDFORM.                    " F_SORT_TABLE

*&---------------------------------------------------------------------*
*&      Form  F_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       Build field catalog
*----------------------------------------------------------------------*
FORM f_field_catalog .
*{
*** Call this subroutine for automatic building of fieldcatalog
  PERFORM auto_get_fieldcat TABLES gt_print
                             USING gc_alv_tab_name.
*}
ENDFORM.                    " F_FIELD_CATALOG

**&---------------------------------------------------------------------*
**&      Form  USER_COMMAND
**&---------------------------------------------------------------------*
**       Handle user actions
**----------------------------------------------------------------------*
*FORM user_command USING pv_ucomm     TYPE sy-ucomm
*                        pv_selfld    TYPE slis_selfield.
**{
*
**}
*ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  F_PRINT_REPORT
*&---------------------------------------------------------------------*
*       To print output
*----------------------------------------------------------------------*
FORM f_print_report .                                       "#EC NEEDED
*Types ----------------------------------------------------------------*
*TYPES: BEGIN OF lty_s_xxx,
*        xx TYPE xx,
*       END OF lty_s_xxx,
*       lty_t_xxx type table of lty_s_xxx.

*Variables ------------------------------------------------------------*
*  DATA lv_xxx TYPE xx.

*Internal Tables ------------------------------------------------------*
*DATA lt_xx TYPE STANDARD TABLE OF xx.

*Work area ------------------------------------------------------------*
*DATA ls_xx TYPE xx.

*Constans -------------------------------------------------------------*
*CONSTANTS gc_xx TYPE xx.

*Field Symbols --------------------------------------------------------*
*FIELD-SYMBOLS <lt_> TYPE STANDARD TABLE xx. " Internal Table
*FIELD-SYMBOLS <lv_> TYPE xx.                " Variavle

*Ranges ---------------------------------------------------------------*
*DATA lt_xx TYPE RANGE OF xx.

*References -----------------------------------------------------------*
*DATA lr_xx TYPE REF TO data.

*Object References ----------------------------------------------------*
*DATA: lo_xx TYPE REF TO cl_xx.

*------------------------Logic-----------------------------------------*


ENDFORM.                    " F_PRINT_REPORT

*&---------------------------------------------------------------------*
*& Form MODIFY_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM modify_exit USING us_datum TYPE datum.
  DATA:
    lo_cl_merge TYPE REF TO cl_rspo_pdf_merge,
    lo_pen_util TYPE REF TO zgepncl_pensioner_util.

  DATA:
    lt_attachment TYPE y88tt_email_attach,
    lt_binary_tab TYPE clm_t_bindata,
    lt_binary     TYPE clm_t_bindata,
    lt_content    TYPE y88tt_email_content,
    lt_pa0019     TYPE STANDARD TABLE OF pa0019,
    lt_pa0019_2   TYPE STANDARD TABLE OF pa0019,
    lt_pa0019_3   TYPE STANDARD TABLE OF pa0019,
    lt_pa0000     TYPE STANDARD TABLE OF p0000,
    lt_pa0001     TYPE STANDARD TABLE OF p0001,
    lt_pa0014     TYPE STANDARD TABLE OF p0014,
    lt_pa0105     TYPE STANDARD TABLE OF p0105,
    lt_recipient  TYPE y88tt_email_recipients,
    lt_return     TYPE bapiret2_t,
    lt_solix      TYPE solix_tab.

  DATA:
    ls_all_cst_agd TYPE zges_pnlt_0002,
    ls_all_cst_spf TYPE zges_pnlt_0003,
    ls_attachment  TYPE y88s_email_attach,
    ls_pa0001      TYPE p0001,
    ls_pa0014      TYPE p0014,
    ls_pa0019      TYPE pa0019,
    ls_pa0019_3    TYPE pa0019,
    ls_pa0105      TYPE p0105,
    ls_recipient   TYPE y88s_email_recipients,
    ls_return      TYPE bapireturn1,
    ls_solix       TYPE solix.

  DATA:
    lv_content        TYPE string,
    lv_date           TYPE sy-datum,
    lv_file_len       TYPE i,
    lv_tmpid_lt       TYPE ze_tmpid,
    lv_tmpid_email    TYPE ze_tmpid,
    lv_pa0000         LIKE LINE OF lt_pa0000,
    lv_processid      TYPE ze_prcid,
    lv_pdf_merged     TYPE xstring,
    lv_pdf_merged_len TYPE i,
    lv_raw_line       TYPE so_raw255,
    lv_xstring        TYPE xstring.

  FIELD-SYMBOLS:
    <lfs_raw> TYPE x255.

  CREATE OBJECT lo_pen_util.
  CREATE OBJECT lo_cl_merge.

  CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
    EXPORTING
      months  = '-3'
      olddate = us_datum
    IMPORTING
      newdate = lv_date.

  SELECT *
    FROM pa0019
    INTO TABLE lt_pa0019
    WHERE subty = 'P1'
    AND   termn =  lv_date
    AND   bvmrk = '1'.
  IF sy-subrc EQ 0.
    SORT lt_pa0019 BY pernr.

    lt_pa0019_2 = lt_pa0019.
    DELETE ADJACENT DUPLICATES FROM lt_pa0019_2 COMPARING pernr.

    SELECT
      *
      FROM pa0019
      INTO TABLE lt_pa0019_3
      FOR ALL ENTRIES IN lt_pa0019_2
      WHERE pernr EQ lt_pa0019_2-pernr
        AND subty EQ 'P1'.
    IF sy-subrc EQ 0.
      SORT lt_pa0019_3 BY pernr endda DESCENDING.
    ENDIF.
  ENDIF.

  LOOP AT lt_pa0019 INTO ls_pa0019.
    READ TABLE lt_pa0019_3 INTO ls_pa0019_3
      WITH KEY pernr = ls_pa0019-pernr
      BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF ls_pa0019-termn NE ls_pa0019_3-termn OR
         ls_pa0019-bvmrk NE ls_pa0019_3-bvmrk.
        EXIT.
      ENDIF.
    ENDIF.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        tclas     = 'A'
        pernr     = ls_pa0019-pernr
        infty     = '0014'
        begda     = us_datum
        endda     = us_datum
      TABLES
        infty_tab = lt_pa0014.

    READ TABLE lt_pa0014 INTO ls_pa0014
      WITH KEY subty = '6501'.
    IF ls_pa0014 IS NOT INITIAL.
      lv_date = ls_pa0014-endda.
      ls_pa0014-endda = us_datum. "p_date.

      CALL METHOD zgecacl_hr_util=>mt_infotype_operation_single
        EXPORTING
          iv_pernr         = ls_pa0014-pernr
          iv_infty         = '0014'
          iv_subtype       = ls_pa0014-subty
          iv_validityend   = lv_date
          iv_validitybegin = ls_pa0014-begda
          iv_object_id     = ls_pa0014-objps
          iv_operation     = gc_opr_upd
          iv_nocommit      = abap_false
          is_record        = ls_pa0014
        IMPORTING
          es_return        = ls_return.
    ENDIF.

    "Generation of cessation letter
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        tclas     = 'A'
        pernr     = ls_pa0019-pernr
        infty     = '0001'
        begda     = us_datum
        endda     = us_datum
      TABLES
        infty_tab = lt_pa0001.
    READ TABLE lt_pa0001 INTO ls_pa0001 INDEX 1.
    CASE ls_pa0001-werks.
      WHEN '5080'. "'Allowance Cessation AGD'.
        CALL METHOD lo_pen_util->mt_allowan_cessat_agd_letter
          EXPORTING
            im_pernr       = ls_pa0019-pernr       " Personnel number
            im_date        = us_datum        " Start Date
          IMPORTING
            ex_placeholder = ls_all_cst_agd
            ex_tmpid       = lv_tmpid_lt
            ex_processid   = lv_processid.

        CALL FUNCTION 'Y_88GECAFM_OPEN_DOCUMENT'
          EXPORTING
            iv_conv_pdf                 = abap_true
            iv_temp_id                  = lv_tmpid_lt
            iv_process_id               = lv_processid
            is_structure                = ls_all_cst_agd
            iv_date                     = sy-datum
          IMPORTING
            lv_content                  = lv_content
          TABLES
            it_bin_data                 = lt_binary
          EXCEPTIONS
            template_not_found          = 1
            strcuture_is_empty          = 2
            process_id_is_empty         = 3
            template_contents_not_found = 4
            structure_mismatch          = 5
            OTHERS                      = 6.

      WHEN '5086'. "'Allowance Cessation SPF'.
        CALL METHOD lo_pen_util->mt_allowan_cessat_spf_letter
          EXPORTING
            im_pernr       = ls_pa0019-pernr      " Personnel number
            im_date        = us_datum        " Start Date
          IMPORTING
            ex_placeholder = ls_all_cst_spf
            ex_tmpid       = lv_tmpid_lt
            ex_processid   = lv_processid.

        CALL FUNCTION 'Y_88GECAFM_OPEN_DOCUMENT'
          EXPORTING
            iv_conv_pdf                 = abap_true
            iv_temp_id                  = lv_tmpid_lt
            iv_process_id               = lv_processid
            is_structure                = ls_all_cst_spf
            iv_date                     = sy-datum
          IMPORTING
            lv_content                  = lv_content
          TABLES
            it_bin_data                 = lt_binary
          EXCEPTIONS
            template_not_found          = 1
            strcuture_is_empty          = 2
            process_id_is_empty         = 3
            template_contents_not_found = 4
            structure_mismatch          = 5
            OTHERS                      = 6.
      WHEN OTHERS.
    ENDCASE.

    IF sy-subrc EQ 0.
      LOOP AT lt_binary ASSIGNING <lfs_raw>.
        lv_raw_line = <lfs_raw>.
        lv_file_len = lv_file_len + xstrlen( lv_raw_line ) + 2. "File Size
      ENDLOOP.

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_file_len
*         FIRST_LINE   = 0
*         LAST_LINE    = 0
        IMPORTING
          buffer       = lv_xstring
        TABLES
          binary_tab   = lt_binary
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.
      IF sy-subrc EQ 0.
        lo_cl_merge->add_document( lv_xstring ).
      ENDIF.

    ENDIF.


************ below only after all merged for each pension admin,
*** there has to be a condition to check for that,
*** for now it's processed every single record
    lo_cl_merge->merge_documents(
           IMPORTING merged_document = lv_pdf_merged ).


    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_pdf_merged
      IMPORTING
        output_length = lv_pdf_merged_len
      TABLES
        binary_tab    = lt_binary_tab.


    ls_attachment-type = 'BIN'. "'BIN'.   "binary
    CONCATENATE  'Cessation Letter-' sy-uzeit
                 '-' sy-datum '.PDF'
      INTO ls_attachment-subjt.
    CONDENSE ls_attachment-subjt.

    LOOP AT lt_binary_tab ASSIGNING <lfs_raw>.
      ls_solix-line = <lfs_raw>.
      APPEND ls_solix TO lt_solix.
    ENDLOOP.

    ls_attachment-cohex = lt_solix[].
    APPEND ls_attachment TO lt_attachment.


****Trigger Email.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        tclas     = 'A'
        pernr     = ls_pa0019-pernr
        infty     = '0105'
        begda     = us_datum
        endda     = us_datum
      TABLES
        infty_tab = lt_pa0105.

    READ TABLE lt_pa0105 INTO ls_pa0105
      WITH KEY subty = '10'.
    IF sy-subrc EQ 0.
      ls_recipient-email =  ls_pa0105-usrid_long.
      ls_recipient-pernr =  ls_pa0105-pernr.
      APPEND ls_recipient TO lt_recipient.
    ENDIF.

    CALL FUNCTION 'Y_88GECAFM_GET_TEMPLID'
      EXPORTING
        iv_module          = gc_module
        iv_process_id      = gc_process_id
      IMPORTING
        ev_tmpid           = lv_tmpid_email
      EXCEPTIONS
        template_not_found = 1
        empty_process_id   = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      lv_tmpid_email           = gc_tmpid. "'GEPNEM000000010'.
    ENDIF.

*      lt_content = VALUE #( ( field = gc_plchld_penid value = us_p0185-icnum )
*                            ( field = gc_plchld_idnum value = us_p0185-icnum )
*                            ( field = gc_plchld_pernr value = ls_pa0105-pernr )
*                            ( field = gc_plchld_cname value = ls_pa0001-ename )
*                            ( field = gc_plchld_dthdt value = p_date )
*                            ( field = gc_plchld_cstdt value = us_datum )
*                            ( field = gc_plchld_rmrks value = lv_remark )
*                            ).

    ls_recipient-email =  'chrissandy.fernando@ncs.com.sg'.
    APPEND ls_recipient TO lt_recipient.

    ls_recipient-email =  'saxena.shivangi@ncs.com.sg'.
    APPEND ls_recipient TO lt_recipient.

    CALL FUNCTION 'Y_88GECAFM_SEND_EMAIL'
      EXPORTING
        iv_tmpid     = lv_tmpid_email
        it_documents = lt_attachment
        it_recipient = lt_recipient
        it_content   = lt_content
      TABLES
        ct_return    = lt_return.
  ENDLOOP.

  CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
    EXPORTING
      months  = '-12'
      olddate = us_datum "p_date
    IMPORTING
      newdate = lv_date.

  SELECT * FROM pa0019 INTO TABLE lt_pa0019
                      WHERE subty = 'P1'
                      AND   termn = lv_date
                      AND   bvmrk = '1'.
  IF sy-subrc EQ 0.
    SORT lt_pa0019 BY pernr.

    lt_pa0019_2 = lt_pa0019.
    DELETE ADJACENT DUPLICATES FROM lt_pa0019_2 COMPARING pernr.

    SELECT
      *
      FROM pa0019
      INTO TABLE lt_pa0019_3
      FOR ALL ENTRIES IN lt_pa0019_2
      WHERE pernr EQ lt_pa0019_2-pernr
        AND subty EQ 'P1'.
    IF sy-subrc EQ 0.
      SORT lt_pa0019_3 BY pernr endda DESCENDING.
    ENDIF.
  ELSE.
    CLEAR lt_pa0019.
  ENDIF.


  LOOP AT lt_pa0019 INTO ls_pa0019.
    READ TABLE lt_pa0019_3 INTO ls_pa0019_3
      WITH KEY pernr = ls_pa0019-pernr
      BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF ls_pa0019-termn NE ls_pa0019_3-termn OR
         ls_pa0019-bvmrk NE ls_pa0019_3-bvmrk.
        EXIT.
      ENDIF.
    ENDIF.

    ls_pa0014-pernr = ls_pa0019-pernr.
    ls_pa0014-subty = ls_pa0014-lgart = '6005'.
    ls_pa0014-infty = '0014'.
    ls_pa0014-betrg = '0'.
    ls_pa0014-anzhl = '1'.
    ls_pa0014-begda = us_datum + 1.
    ls_pa0014-endda = gc_eod_9999.

    CALL METHOD zgecacl_hr_util=>mt_infotype_operation_single
      EXPORTING
        iv_pernr         = ls_pa0014-pernr
        iv_infty         = '0014'
        iv_subtype       = ls_pa0014-subty
        iv_validityend   = ls_pa0014-endda
        iv_validitybegin = ls_pa0014-begda
        iv_object_id     = ls_pa0014-objps
        iv_operation     = gc_opr_ins
        iv_nocommit      = abap_false
        is_record        = ls_pa0014
      IMPORTING
        es_return        = ls_return.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        tclas     = 'A'
        pernr     = ls_pa0019-pernr
        infty     = '0000'
        begda     = us_datum
        endda     = us_datum
      TABLES
        infty_tab = lt_pa0000.

    IF lt_pa0000 IS NOT INITIAL.

      READ TABLE lt_pa0000 INTO DATA(ls_pa0000) INDEX 1.

      lv_date = ls_pa0000-begda.
*        lv_pa0000-subty = 'D2'.
      lv_pa0000-massn = 'D2'.
      lv_pa0000-massg = '01'.
      lv_pa0000-stat2 = '3'.
      lv_pa0000-begda =  us_datum. "p_date.
      lv_pa0000-endda = gc_eod_9999.
      lv_pa0000-pernr = ls_pa0000-pernr.
      lv_pa0000-infty = '0000'.

      CALL METHOD zgecacl_hr_util=>mt_infotype_operation_single
        EXPORTING
          iv_pernr         = ls_pa0000-pernr
          iv_infty         = '0000'
          iv_subtype       = lv_pa0000-subty
          iv_validityend   = lv_pa0000-endda
          iv_validitybegin = lv_pa0000-begda
          iv_object_id     = lv_pa0000-objps
          iv_operation     = gc_opr_ins
          iv_nocommit      = abap_false
          is_record        = lv_pa0000
        IMPORTING
          es_return        = ls_return.

      IF ls_return IS NOT INITIAL.

      ENDIF.

    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        tclas     = 'A'
        pernr     = ls_pa0019-pernr
        infty     = '0001'
        begda     = us_datum
        endda     = us_datum
      TABLES
        infty_tab = lt_pa0001.

    IF lt_pa0001 IS NOT INITIAL.

      READ TABLE lt_pa0001 INTO ls_pa0001 INDEX 1.
      lv_date         = ls_pa0001-begda.
      ls_pa0001-begda = us_datum. "p_date.
      ls_pa0001-btrtl = '1780'.

      CALL METHOD zgecacl_hr_util=>mt_infotype_operation_single
        EXPORTING
          iv_pernr         = ls_pa0001-pernr
          iv_infty         = '0001'
          iv_subtype       = ls_pa0001-subty
          iv_validityend   = ls_pa0001-endda
          iv_validitybegin = lv_date
          iv_object_id     = ls_pa0001-objps
          iv_operation     = gc_opr_ins
          iv_nocommit      = abap_false
          is_record        = ls_pa0001
        IMPORTING
          es_return        = ls_return.
      IF ls_return IS NOT INITIAL.

      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_ENTRANCE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_PA0019
*&---------------------------------------------------------------------*
FORM modify_entrance USING  us_p0019 TYPE p0019
                            uv_months TYPE vtbbewe-atage.
  DATA:
    lo_cl_merge TYPE REF TO cl_rspo_pdf_merge,
    lo_pen_util TYPE REF TO zgepncl_pensioner_util.

  DATA :
    lt_attachment TYPE y88tt_email_attach,
    lt_binary_tab TYPE clm_t_bindata,
    lt_binary     TYPE clm_t_bindata,
    lt_content    TYPE y88tt_email_content,
    lt_pa0014     TYPE STANDARD TABLE OF p0014,
    lt_pa0000     TYPE STANDARD TABLE OF p0000,
    lt_pa0001     TYPE STANDARD TABLE OF p0001,
    lt_recipient  TYPE y88tt_email_recipients,
    lt_return     TYPE bapiret2_t,
    lt_solix      TYPE solix_tab.

  DATA :
    ls_all_rst_agd TYPE zges_pnlt_0004,
    ls_all_rst_spf TYPE zges_pnlt_0005,
    ls_attachment  TYPE y88s_email_attach,
    ls_pa0001      TYPE p0001,
    ls_pa0014      TYPE p0014,
    ls_pa0014_2    TYPE p0014,
    ls_recipient   TYPE y88s_email_recipients,
    ls_return      TYPE bapireturn1,
    ls_solix       TYPE solix.

  DATA :
    lv_content        TYPE string,
    lv_date           TYPE begda,
    lv_file_len       TYPE i,
    lv_tmpid_lt       TYPE ze_tmpid,
    lv_tmpid_email    TYPE ze_tmpid,
    lv_months         TYPE vtbbewe-atage,
    lv_processid      TYPE ze_prcid,
    lv_pdf_merged     TYPE xstring,
    lv_pdf_merged_len TYPE i,
    lv_raw_line       TYPE so_raw255,
    lv_xstring        TYPE xstring.

  FIELD-SYMBOLS:
    <lfs_raw> TYPE x255.

  IF uv_months GE '3'.
    CREATE OBJECT lo_cl_merge.
    CREATE OBJECT lo_pen_util.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        tclas     = 'A'
        pernr     = us_p0019-pernr
        infty     = '0014'
*       begda     = sy-datum
*       endda     = sy-datum
      TABLES
        infty_tab = lt_pa0014.

    SORT lt_pa0014 BY pernr ASCENDING
                      subty ASCENDING
                      endda DESCENDING.

    READ TABLE lt_pa0014 INTO ls_pa0014_2
      WITH KEY pernr = us_p0019-pernr
               subty = '6501'
               BINARY SEARCH.
    IF sy-subrc EQ 0.
      "This is to get previous record values
    ENDIF.

    ls_pa0014-pernr = us_p0019-pernr.
    ls_pa0014-subty = '6501'.
    ls_pa0014-begda = us_p0019-mndat. "p_date.
    ls_pa0014-endda = gc_eod_9999.
    ls_pa0014-betrg = ls_pa0014_2-betrg.
    ls_pa0014-waers = ls_pa0014_2-waers.

    CALL METHOD zgecacl_hr_util=>mt_infotype_operation_single
      EXPORTING
        iv_pernr         = ls_pa0014-pernr
        iv_infty         = '0014'
        iv_subtype       = ls_pa0014-subty
        iv_validityend   = ls_pa0014-endda
        iv_validitybegin = ls_pa0014-begda
        iv_object_id     = ls_pa0014-objps
        iv_operation     = gc_opr_ins
        iv_nocommit      = abap_false
        is_record        = ls_pa0014
      IMPORTING
        es_return        = ls_return.
    IF ls_return IS NOT INITIAL.

    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        tclas     = 'A'
        pernr     = us_p0019-pernr
        infty     = '0001'
        begda     = sy-datum
        endda     = sy-datum
      TABLES
        infty_tab = lt_pa0001.
    READ TABLE lt_pa0001 INTO ls_pa0001 INDEX 1.
    CASE ls_pa0001-werks.
      WHEN '5080'. "'Allowance Restoration AGD'.
        CALL METHOD lo_pen_util->mt_allowan_restore_agd_letter
          EXPORTING
            im_pernr       = us_p0019-pernr       " Personnel number
            im_date        = sy-datum        " Start Date
          IMPORTING
            ex_placeholder = ls_all_rst_agd
            ex_tmpid       = lv_tmpid_lt
            ex_processid   = lv_processid.

        CALL FUNCTION 'Y_88GECAFM_OPEN_DOCUMENT'
          EXPORTING
            iv_conv_pdf                 = abap_true
            iv_temp_id                  = lv_tmpid_lt
            iv_process_id               = lv_processid
            is_structure                = ls_all_rst_agd
            iv_date                     = sy-datum
          IMPORTING
            lv_content                  = lv_content
          TABLES
            it_bin_data                 = lt_binary
          EXCEPTIONS
            template_not_found          = 1
            strcuture_is_empty          = 2
            process_id_is_empty         = 3
            template_contents_not_found = 4
            structure_mismatch          = 5
            OTHERS                      = 6.

      WHEN '5086'. "'Allowance Cessation SPF'.
        CALL METHOD lo_pen_util->mt_allowan_restore_spf_letter
          EXPORTING
            im_pernr       = us_p0019-pernr      " Personnel number
            im_date        = sy-datum        " Start Date
          IMPORTING
            ex_placeholder = ls_all_rst_spf
            ex_tmpid       = lv_tmpid_lt
            ex_processid   = lv_processid.

        CALL FUNCTION 'Y_88GECAFM_OPEN_DOCUMENT'
          EXPORTING
            iv_conv_pdf                 = abap_true
            iv_temp_id                  = lv_tmpid_lt
            iv_process_id               = lv_processid
            is_structure                = ls_all_rst_spf
            iv_date                     = sy-datum
          IMPORTING
            lv_content                  = lv_content
          TABLES
            it_bin_data                 = lt_binary
          EXCEPTIONS
            template_not_found          = 1
            strcuture_is_empty          = 2
            process_id_is_empty         = 3
            template_contents_not_found = 4
            structure_mismatch          = 5
            OTHERS                      = 6.
      WHEN OTHERS.
    ENDCASE.

    IF sy-subrc EQ 0.
      LOOP AT lt_binary ASSIGNING <lfs_raw>.
        lv_raw_line = <lfs_raw>.
        lv_file_len = lv_file_len + xstrlen( lv_raw_line ) + 2. "File Size
      ENDLOOP.

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_file_len
*         FIRST_LINE   = 0
*         LAST_LINE    = 0
        IMPORTING
          buffer       = lv_xstring
        TABLES
          binary_tab   = lt_binary
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.
      IF sy-subrc EQ 0.
        lo_cl_merge->add_document( lv_xstring ).
      ENDIF.

    ENDIF.


************ below only after all merged for each pension admin,
*** there has to be a condition to check for that,
*** for now it's processed every single record
    lo_cl_merge->merge_documents(
           IMPORTING merged_document = lv_pdf_merged ).


    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_pdf_merged
      IMPORTING
        output_length = lv_pdf_merged_len
      TABLES
        binary_tab    = lt_binary_tab.


    ls_attachment-type = 'BIN'. "'BIN'.   "binary
    CONCATENATE  'Restoration Allowance-' sy-uzeit
                 '-' sy-datum '.PDF'
      INTO ls_attachment-subjt.
    CONDENSE ls_attachment-subjt.

    LOOP AT lt_binary_tab ASSIGNING <lfs_raw>.
      ls_solix-line = <lfs_raw>.
      APPEND ls_solix TO lt_solix.
    ENDLOOP.

    ls_attachment-cohex = lt_solix[].
    APPEND ls_attachment TO lt_attachment.


****Trigger Email.
*
*    CALL FUNCTION 'HR_READ_INFOTYPE'
*      EXPORTING
*        tclas     = 'A'
*        pernr     = ls_pa0019-pernr
*        infty     = '0105'
*        begda     = us_datum
*        endda     = us_datum
*      TABLES
*        infty_tab = lt_pa0105.
*
*    READ TABLE lt_pa0105 INTO ls_pa0105
*      WITH KEY subty = '10'.
    IF sy-subrc EQ 0.
*      ls_recipient-email =  ls_pa0105-usrid_long.
*      ls_recipient-pernr =  ls_pa0105-pernr.
      APPEND ls_recipient TO lt_recipient.
    ENDIF.

    CALL FUNCTION 'Y_88GECAFM_GET_TEMPLID'
      EXPORTING
        iv_module          = gc_module
        iv_process_id      = gc_process_id_rst
      IMPORTING
        ev_tmpid           = lv_tmpid_email
      EXCEPTIONS
        template_not_found = 1
        empty_process_id   = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      lv_tmpid_email           = gc_tmpid_rst. "'GEPNEM000000010'.
    ENDIF.

*      lt_content = VALUE #( ( field = gc_plchld_penid value = us_p0185-icnum )
*                            ( field = gc_plchld_idnum value = us_p0185-icnum )
*                            ( field = gc_plchld_pernr value = ls_pa0105-pernr )
*                            ( field = gc_plchld_cname value = ls_pa0001-ename )
*                            ( field = gc_plchld_dthdt value = p_date )
*                            ( field = gc_plchld_cstdt value = us_datum )
*                            ( field = gc_plchld_rmrks value = lv_remark )
*                            ).

    ls_recipient-email =  'chrissandy.fernando@ncs.com.sg'.
    APPEND ls_recipient TO lt_recipient.

    ls_recipient-email =  'saxena.shivangi@ncs.com.sg'.
    APPEND ls_recipient TO lt_recipient.

    CALL FUNCTION 'Y_88GECAFM_SEND_EMAIL'
      EXPORTING
        iv_tmpid     = lv_tmpid_email
        it_documents = lt_attachment
        it_recipient = lt_recipient
        it_content   = lt_content
      TABLES
        ct_return    = lt_return.
  ENDIF.

  IF uv_months GE 12.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        tclas     = 'A'
        pernr     = us_p0019-pernr
        infty     = '0000'
        begda     = sy-datum
        endda     = sy-datum
      TABLES
        infty_tab = lt_pa0000.

    IF lt_pa0000 IS NOT INITIAL.

      READ TABLE lt_pa0000 INTO DATA(ls_pa0000) INDEX 1.

      ls_pa0000-massn = 'D2'.
      ls_pa0000-massg = '02'.

      ls_pa0000-begda = us_p0019-mndat.

      CALL METHOD zgecacl_hr_util=>mt_infotype_operation_single
        EXPORTING
          iv_pernr         = ls_pa0000-pernr
          iv_infty         = '0000'
          iv_subtype       = ls_pa0000-subty
          iv_validityend   = ls_pa0000-endda
          iv_validitybegin = ls_pa0000-begda
          iv_object_id     = ls_pa0000-objps
          iv_operation     = gc_opr_ins
          iv_nocommit      = abap_false
          is_record        = ls_pa0000
        IMPORTING
          es_return        = ls_return.

      IF ls_return IS NOT INITIAL.

      ENDIF.
    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        tclas     = 'A'
        pernr     = us_p0019-pernr
        infty     = '0001'
*       begda     = sy-datum
*       endda     = sy-datum
      TABLES
        infty_tab = lt_pa0001.

    IF lt_pa0001 IS NOT INITIAL.
      SORT lt_pa0001 BY pernr endda DESCENDING.
      READ TABLE lt_pa0001 INTO ls_pa0001 INDEX 1.
      IF sy-subrc EQ 0.
*        lv_date = ls_pa0001-begda .
        ls_pa0001-begda = us_p0019-mndat.
      ENDIF.

      READ TABLE lt_pa0001 INTO DATA(ls_pa0001_2) INDEX 2.
      IF sy-subrc EQ 0.
        ls_pa0001-btrtl = ls_pa0001_2-btrtl.
      ENDIF.

      CALL METHOD zgecacl_hr_util=>mt_infotype_operation_single
        EXPORTING
          iv_pernr         = ls_pa0001-pernr
          iv_infty         = '0001'
          iv_subtype       = ls_pa0001-subty
          iv_validityend   = ls_pa0001-endda
          iv_validitybegin = ls_pa0001-begda
          iv_object_id     = ls_pa0001-objps
          iv_operation     = gc_opr_ins
          iv_nocommit      = abap_false
          is_record        = ls_pa0001
        IMPORTING
          es_return        = ls_return.
      IF ls_return IS NOT INITIAL.

      ENDIF.
    ENDIF.

    READ TABLE lt_pa0014 INTO ls_pa0014
      WITH KEY pernr = us_p0019-pernr
               subty = '6005'
               BINARY SEARCH.
    IF ls_pa0014 IS NOT  INITIAL.
      lv_date = ls_pa0014-endda.
      ls_pa0014-endda = us_p0019-mndat - 1.

      CALL METHOD zgecacl_hr_util=>mt_infotype_operation_single
        EXPORTING
          iv_pernr         = ls_pa0014-pernr
          iv_infty         = '0014'
          iv_subtype       = ls_pa0014-subty
          iv_validityend   = lv_date
          iv_validitybegin = ls_pa0014-begda
          iv_object_id     = ls_pa0014-objps
          iv_operation     = gc_opr_upd
          iv_nocommit      = abap_false
          is_record        = ls_pa0014
        IMPORTING
          es_return        = ls_return.

    ENDIF.
  ENDIF.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VALIDATE_PENSIONER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ls_pa0185
*&---------------------------------------------------------------------*
FORM validate_pensioner  USING    us_p0185 TYPE pa0185.
  DATA lt_pa0001 TYPE STANDARD TABLE OF p0001.
  DATA lt_pa0000 TYPE STANDARD TABLE OF p0000.

  CALL FUNCTION 'HR_READ_INFOTYPE'
    EXPORTING
      tclas     = 'A'
      pernr     = us_p0185-pernr
      infty     = '0000'
      begda     = sy-datum
      endda     = sy-datum
    TABLES
      infty_tab = lt_pa0000.


  CALL FUNCTION 'HR_READ_INFOTYPE'
    EXPORTING
      tclas     = 'A'
      pernr     = us_p0185-pernr
      infty     = '0001'
      begda     = sy-datum
      endda     = sy-datum
    TABLES
      infty_tab = lt_pa0001.

  READ TABLE lt_pa0001 INTO DATA(ls_pa0001) INDEX 1.
  READ TABLE lt_pa0000 INTO DATA(ls_pa0000) INDEX 1.


  IF ls_pa0001-persg = 'R'
    AND ls_pa0000-stat2 = '3'.
    gv_flag = abap_true.
  ENDIF.

ENDFORM.

**&---------------------------------------------------------------------*
**& Form F_MODIFY_SSO
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_modify_sso .
*
*  IF p_mode = abap_true .
*    LOOP AT SCREEN.
*      CASE screen-group1.
*        WHEN 'MD1'.
*          screen-input = 1.
*          screen-invisible = 0.
*          MODIFY SCREEN.
*      ENDCASE.
*    ENDLOOP.
*  ELSE.
*    LOOP AT SCREEN.
*      CASE screen-group1.
*        WHEN 'MD1'.
*          screen-input = 0.
*          screen-invisible = 1.
*          MODIFY SCREEN.
*      ENDCASE.
*    ENDLOOP.
*  ENDIF.
*
*ENDFORM.