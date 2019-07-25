FUNCTION y_88gecafm_send_email .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_TMPID) TYPE  ZE_TMPID
*"     REFERENCE(IV_PRCID) TYPE  ZE_PRCID OPTIONAL
*"     REFERENCE(IT_DOCUMENTS) TYPE  Y88TT_EMAIL_ATTACH OPTIONAL
*"     REFERENCE(IT_RECIPIENT) TYPE  Y88TT_EMAIL_RECIPIENTS OPTIONAL
*"     REFERENCE(IV_SENDER_BATCH) TYPE  CHAR1 DEFAULT 'X'
*"     REFERENCE(IT_SOOS1TAB) TYPE  SOOS1TAB OPTIONAL
*"     REFERENCE(IV_REQUESTED_STATUS) TYPE  BCS_RQST DEFAULT 'E'
*"     REFERENCE(IV_BUS_LOGIC) TYPE  BOOLEAN OPTIONAL
*"     REFERENCE(IS_SELECTION) TYPE  Y88S_SELECT_VARIABLES OPTIONAL
*"     REFERENCE(IM_COMMIT) TYPE  BOOLEAN DEFAULT 'X'
*"     REFERENCE(IT_CONTENT) TYPE  Y88TT_EMAIL_CONTENT OPTIONAL
*"     REFERENCE(IS_BUTTON) TYPE  Y88S_EMAIL_BTN OPTIONAL
*"  TABLES
*"      CT_RETURN TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
*======================================================================*
*  Functional spec ID     :
*  Program ID             :  Y88GECAFM_SEND_EMAIL
*  Program Description    :  Send Email with HTML Content
*
*  Functional Consultant  :
*  Created By             : Chitritha Eswar Kambi
*  Start Date             : 24.05.2018
*  End Date               : 24.05.2018
*======================================================================*
*                     Modify Log History.
*----------------------------------------------------------------------*
*No.    Modified by    Date         Description
*---    -----------    ----         -----------
*M000    NCS_xxxx      DD.MM.YYYY   Change Description
*======================================================================*

*======================================================================*
*                          Variables
*======================================================================*
  DATA : lv_subject        TYPE so_obj_des,
         lv_subject_string TYPE string,
         lv_text           TYPE soli-line,
         lv_status_mail    TYPE bcs_stml,
         lv_image_content  TYPE xstring,
         lv_obj_len        TYPE so_obj_len,
         lv_error          TYPE sy-msgv1.
*======================================================================*
*                          Internal Tables
*======================================================================*
  DATA : lt_message     TYPE soli_tab,
         lt_solix       TYPE solix_tab,
         lt_body        TYPE soli_tab,
         lt_werks       TYPE /iwbep/t_cod_select_options WITH HEADER LINE,
         lt_btrtl       TYPE /iwbep/t_cod_select_options WITH HEADER LINE,
         lt_persg       TYPE /iwbep/t_cod_select_options WITH HEADER LINE,
         lt_persk       TYPE /iwbep/t_cod_select_options WITH HEADER LINE,
         lt_prcid       TYPE /iwbep/t_cod_select_options WITH HEADER LINE,
         lt_email_group TYPE y88tt_email_group.
*======================================================================*
*                          Work area
*======================================================================*
  DATA : ls_documents_line LIKE LINE OF it_documents,
         ls_recipient      LIKE LINE OF it_recipient,
         ls_body           LIKE LINE OF lt_body,
         ls_return         TYPE bapiret2,
         ls_soos           TYPE soos1,
         ls_spool_reci     TYPE swotobjid,
         ls_template       TYPE y88t_email_temp,
         ls_notif_fm       TYPE y88t_process,
         ls_email_group    TYPE y88s_email_group.

*======================================================================*
*                          Object References
*======================================================================*
  DATA : lo_send_request TYPE REF TO cl_bcs,
         lo_document     TYPE REF TO cl_document_bcs,
         lo_mime_helper  TYPE REF TO cl_gbt_multirelated_service,
         li_recipient    TYPE REF TO if_recipient_bcs,
         li_sender       TYPE REF TO if_sender_bcs,
         li_mr_api       TYPE REF TO if_mr_api.

  DATA : lx_bcs_exception TYPE REF TO cx_bcs.

*=========================Main Logic====================================*
  IF is_selection-werks IS NOT INITIAL.
    lt_werks-sign = 'I'.
    lt_werks-option = 'EQ'.
    lt_werks-low = is_selection-werks.
    APPEND lt_werks.
  ENDIF.
  IF is_selection-btrtl IS NOT INITIAL.
    lt_btrtl-sign = 'I'.
    lt_btrtl-option = 'EQ'.
    lt_btrtl-low = is_selection-btrtl.
    APPEND lt_btrtl.
  ENDIF.
  IF is_selection-persg IS NOT INITIAL.
    lt_persg-sign = 'I'.
    lt_persg-option = 'EQ'.
    lt_persg-low = is_selection-persg.
    APPEND lt_persg.
  ENDIF.
  IF is_selection-persk IS NOT INITIAL.
    lt_persk-sign = 'I'.
    lt_persk-option = 'EQ'.
    lt_persk-low = is_selection-persk.
    APPEND lt_persk.
  ENDIF.
  IF iv_prcid IS NOT INITIAL.
    lt_prcid-sign = 'I'.
    lt_prcid-option = 'EQ'.
    lt_prcid-low = iv_prcid.
    APPEND lt_prcid.
  ENDIF.

  " Select email template
  SELECT SINGLE *
    FROM y88t_email_temp
    INTO ls_template
    WHERE werks IN lt_werks[]
    AND   btrtl IN lt_btrtl[]
    AND   persg IN lt_persg[]
    AND   persk IN lt_persk[]
    AND   prcid IN lt_prcid[]
    AND   tmpid = iv_tmpid
    AND   begda LE sy-datum
    AND   endda GE sy-datum.

  BREAK ncs_chie.
  IF ls_template IS NOT INITIAL.

    IF iv_bus_logic IS INITIAL.

      CLEAR lv_subject_string .
      lv_subject_string  = ls_template-subjt.
      " Replace variables in Email Content
      LOOP AT it_content INTO DATA(ls_content).
        REPLACE ALL OCCURRENCES OF ls_content-field IN lv_subject_string  WITH ls_content-value IGNORING CASE.
        REPLACE ALL OCCURRENCES OF ls_content-field IN ls_template-cntnt WITH ls_content-value IGNORING CASE.
      ENDLOOP.

      CLEAR ls_email_group.
      ls_email_group-subjt = lv_subject_string.
      ls_email_group-cntnt = ls_template-cntnt.
      ls_email_group-attch = it_documents.
      ls_email_group-recip = it_recipient.
      APPEND ls_email_group TO lt_email_group.

    ELSE.

*_ Get FM Name
      SELECT SINGLE *
        FROM y88t_process
        INTO ls_notif_fm
        WHERE prcid = ls_template-prcid.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname           = ls_notif_fm-datfm
          EXCEPTIONS
            function_not_exist = 1
            OTHERS             = 2.
        IF sy-subrc = 0.
          CALL FUNCTION ls_notif_fm-datfm
            EXPORTING
              iv_prcid       = iv_prcid
              it_recipient   = it_recipient
              it_content     = it_content
            IMPORTING
              et_email_group = lt_email_group.
        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT lt_email_group INTO ls_email_group.

      TRY.
          "Create persistent send request
          lo_send_request = cl_bcs=>create_persistent( ).

          "Set Subject
          lv_subject = ls_email_group-subjt.

          IF li_mr_api IS INITIAL.
            li_mr_api = cl_mime_repository_api=>if_mr_api~get_api( ).

            CALL METHOD li_mr_api->get
              EXPORTING
                i_url              = '/SAP/PUBLIC/Logo.jpg'
              IMPORTING
                e_content          = lv_image_content
              EXCEPTIONS
                parameter_missing  = 1
                error_occured      = 2
                not_found          = 3
                permission_failure = 4
                OTHERS             = 5.

            " Converting xstring to solix table type format
            CALL METHOD cl_bcs_convert=>xstring_to_solix
              EXPORTING
                iv_xstring = lv_image_content
              RECEIVING
                et_solix   = lt_solix.

            lv_obj_len = xstrlen( lv_image_content ).

          ENDIF.

          CREATE OBJECT lo_mime_helper.
          " Adding image to the mail
          CALL METHOD lo_mime_helper->add_binary_part
            EXPORTING
              content      = lt_solix
              filename     = 'Logo.jpg'
              extension    = 'JPG'
              content_type = 'image/jpeg'
              length       = lv_obj_len
              content_id   = 'Logo.jpg'.


          CLEAR: lt_body, lt_message.

          " Convert Email body content to Soli table type format.
          CALL METHOD cl_bcs_convert=>string_to_soli
            EXPORTING
              iv_string = ls_email_group-cntnt
            RECEIVING
              et_soli   = lt_body.

**********************************************************************
          "Set body of email
          APPEND '<!DOCTYPE html>' TO lt_message.
          APPEND '<html>' TO lt_message.
          APPEND '<head>' TO lt_message.
          APPEND '<style>' TO lt_message.
          APPEND '.img-responsive { width: 7rem; height: 4rem;}' TO lt_message.
          APPEND 'body { font-family: Calibri; font-size: 14px; background-color: aliceblue; }' TO lt_message.
          APPEND '.container {background: white; width: 45rem; height: auto;}' TO lt_message.
          APPEND 'button {background: #FF6633; margin-right: 0.25rem;  margin-left: 0.25rem;  height: 2.5rem;  min-width: 2rem;  flex-shrink: 1;  border-width: 0; color: white; width: 10rem}' TO lt_message.
          APPEND '.bgColor {background: white;}' TO lt_message.
          APPEND '.padding {padding-left: 1rem}' TO lt_message.
          APPEND '</style>' TO lt_message.
          APPEND '</head>' TO lt_message.

          APPEND '<body background-image="">' TO lt_message.
          APPEND '<table class="body" align="center">' TO lt_message.
          APPEND '<td class="container">' TO lt_message.
          APPEND '<div class="bgColor"> <table> <tr> <td>' TO lt_message.
          APPEND '<img src="Logo.jpg" alt="Logo.jpg" class="img-responsive"/>' TO lt_message.
          APPEND '</td> </tr> <tr> <td class="padding">' TO lt_message.

          LOOP AT lt_body INTO ls_body.
            APPEND ls_body TO lt_message.
          ENDLOOP.

          IF is_button IS SUPPLIED.
            DATA(lv_btn_text) = '<center><button onclick="window.location.href = ''' && is_button-btn_url && ''';">' && is_button-btn_text && '</button></center>'.
            APPEND lv_btn_text TO lt_message.
          ENDIF.

          APPEND '</td> </tr> <tr> <td>' TO lt_message.

          APPEND '</td> </tr> </table> </div>' TO lt_message.
          APPEND '</td> </table>' TO lt_message.

          APPEND '<center><div style="padding-top: 1rem; font-size: 10px;"><p>' TO lt_message.
          APPEND 'WARNING: This email may contain privileged and confidential information<br>' TO lt_message.
          APPEND 'If you recieve this email by mistake, you should immediately notify the sender and delete the email<br>' TO lt_message.
          APPEND 'Unauthorised communication and disclosure of any information in the email is an offence under the Official Secrets Act(CAP 213).</p>' TO lt_message.
          APPEND '</div></center></div>' TO lt_message.

          APPEND '</body>' TO lt_message.
          APPEND '</html>' TO lt_message.
**********************************************************************

          CALL METHOD lo_mime_helper->set_main_html
            EXPORTING
              content     = lt_message
              description = 'Email'.

          "Create Document, use mutlirelated because we are using image in body
          lo_document = cl_document_bcs=>create_from_multirelated(
                                         i_multirel_service = lo_mime_helper
                                         i_subject = lv_subject ).
*
*          CALL METHOD lo_document->set_subject_long
*            EXPORTING
*              i_subject_long = lv_subject.

          " Add Attachment
          LOOP AT ls_email_group-attch INTO ls_documents_line.
            IF ls_documents_line-cohex[] IS INITIAL.

              IF ls_documents_line-lengt IS INITIAL.
                CALL METHOD lo_document->add_attachment
                  EXPORTING
                    i_attachment_type    = ls_documents_line-type
                    i_attachment_subject = ls_documents_line-subjt
                    i_att_content_text   = ls_documents_line-cotxt.
              ELSE.

                " if xlsx or docx is being attached, length should be passed.
                CALL METHOD lo_document->add_attachment
                  EXPORTING
                    i_attachment_type    = ls_documents_line-type
                    i_attachment_subject = ls_documents_line-subjt
                    i_att_content_text   = ls_documents_line-cotxt
                    i_attachment_size    = ls_documents_line-lengt.
              ENDIF.

            ELSE.
              IF ls_documents_line-lengt IS INITIAL.
                CALL METHOD lo_document->add_attachment
                  EXPORTING
                    i_attachment_type    = ls_documents_line-type
                    i_attachment_subject = ls_documents_line-subjt
                    i_att_content_hex    = ls_documents_line-cohex.

              ELSE.
                CALL METHOD lo_document->add_attachment
                  EXPORTING
                    i_attachment_type    = ls_documents_line-type
                    i_attachment_subject = ls_documents_line-subjt
                    i_att_content_hex    = ls_documents_line-cohex
                    i_attachment_size    = ls_documents_line-lengt.
              ENDIF.

            ENDIF.
          ENDLOOP.

          " Add document to send request
          lo_send_request->set_document( lo_document ).

          " Get sender object
          IF iv_sender_batch IS NOT INITIAL.
            li_sender = cl_sapuser_bcs=>create( sy-uname ).
          ELSE.
            li_sender = cl_sapuser_bcs=>create( 'EMAIL_NOTIF' ).
          ENDIF.

          " Add sender
          CALL METHOD lo_send_request->set_sender
            EXPORTING
              i_sender = li_sender.

          " Now create Recipients
          LOOP AT  ls_email_group-recip INTO ls_recipient.
            CLEAR: ls_soos, li_recipient.

*            " Note for User name, RECNAM is filled, and RECESC = "B".
*            " For SAP Objects, recextnam should be filled by type swotobjid and recesc = 'J'
*            IF ls_recipient-uname IS NOT INITIAL.
            " Means send to user ID
*              ls_soos-recnam = ls_recipient-uname.
*              ls_soos-recesc = 'B'.
*              ls_soos-sndex = 'X'.

*            ELSE.
            IF ls_recipient-email IS NOT INITIAL.
              " Send to external email address
              ls_soos-recextnam = ls_recipient-email.
              ls_soos-recesc = 'U'.

            ELSEIF ls_recipient-objkey IS NOT INITIAL.
              " Send to spool recipient
              ls_soos-recesc = 'J'.
              MOVE-CORRESPONDING ls_recipient TO ls_spool_reci.
              ls_soos-recextnam = ls_spool_reci.

            ELSE.
              " No recipient, add Error -> No recipient
              IF ls_recipient-pernr IS INITIAL.
                lv_error = TEXT-e03.
              ELSE.
                lv_error = TEXT-e04.
                REPLACE '&1' IN lv_error WITH ls_recipient-pernr.
              ENDIF.

              CALL FUNCTION 'BALW_BAPIRETURN_GET2'
                EXPORTING
                  type   = 'E'
                  cl     = 'ZGECAMC001'
                  number = '000'
                  par1   = lv_error
                  par2   = space
                  par3   = space
                  par4   = space
                IMPORTING
                  return = ls_return.
              APPEND ls_return TO ct_return.
              CONTINUE.
            ENDIF.

            CALL METHOD cl_send_request_bcs=>create_recipient_from_soos1
              EXPORTING
                i_soos1 = ls_soos
              RECEIVING
                result  = li_recipient.

            CALL METHOD lo_send_request->add_recipient
              EXPORTING
                i_recipient  = li_recipient
                i_express    = ls_recipient-exprs
                i_copy       = ls_recipient-copy
                i_blind_copy = ls_recipient-bcopy
                i_no_forward = ls_recipient-nofwd.

          ENDLOOP.

          " Set that you don't need a Return Status E-mail
          lv_status_mail = 'A'.
          CALL METHOD lo_send_request->set_status_attributes
            EXPORTING
              i_requested_status = iv_requested_status
              i_status_mail      = lv_status_mail.

          " Set send immediately flag
          lo_send_request->set_send_immediately( 'X' ).
          BREAK ncs_chie.
          lv_subject_string = ls_email_group-subjt.

          lo_send_request->set_message_subject( lv_subject_string ).

          " Send document
          CALL METHOD lo_send_request->send( ).
          IF im_commit = abap_true.
            COMMIT WORK.
          ENDIF.

        CATCH cx_bcs INTO lx_bcs_exception.
          CLEAR: lv_text.
          CALL METHOD lx_bcs_exception->if_message~get_text
            RECEIVING
              result = lv_text.
          ls_return-type = 'E'.
          ls_return-message = TEXT-e01.
          APPEND ls_return TO ct_return.
      ENDTRY.

    ENDLOOP.
  ELSE.
    lv_error = TEXT-e02.
    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = 'E'
        cl     = 'ZGECAMC001'
        number = '000'
        par1   = lv_error
        par2   = space
        par3   = space
        par4   = space
      IMPORTING
        return = ls_return.
    APPEND ls_return TO ct_return.
    RETURN.
  ENDIF.

ENDFUNCTION.