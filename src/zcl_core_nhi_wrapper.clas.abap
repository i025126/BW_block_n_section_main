CLASS zcl_core_nhi_wrapper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS ce_nhi_activation_mode DEFINITION LOAD .

    TYPES:
      BEGIN OF gtys_xref,
        package    TYPE string,
        name       TYPE string,
        type_descr TYPE string,
      END OF gtys_xref .
    TYPES:
      gtyts_xrefs TYPE SORTED TABLE OF gtys_xref WITH UNIQUE KEY package name .

    CLASS-METHODS class_constructor .

    DATA nv_package TYPE string READ-ONLY .

    METHODS constructor
      IMPORTING
        iv_package TYPE string.

    METHODS check_nhi_obj_exist
      IMPORTING
        ir_nhi_obj_id   TYPE REF TO cl_nhi_object_id OPTIONAL
      RETURNING
        VALUE(r_exists) TYPE rs_bool
      RAISING
        cx_rs2hana_view_nhi .

    METHODS read_nhi_obj
      IMPORTING
        ir_nhi_obj_id   TYPE REF TO cl_nhi_object_id OPTIONAL
      RETURNING
        VALUE(rt_content) TYPE CO2_STRING_TB
      RAISING
        cx_rs2hana_view_nhi .

    METHODS delete_nhi_obj
      IMPORTING
        iv_force           TYPE rs_bool DEFAULT rs_c_false
        ir_nhi_obj_id      TYPE REF TO cl_nhi_object_id
      EXPORTING
        ets_affected_xrefs TYPE gtyts_xrefs
      RAISING
        cx_rs2hana_view_nhi .

    METHODS get_nhi_obj_id
      IMPORTING
        iv_id                   TYPE string
        iv_active_version       TYPE rs_bool DEFAULT rs_c_true
        ir_type                 TYPE REF TO ce_nhi_object_type
      RETURNING
        VALUE(rr_nhi_object_id) TYPE REF TO cl_nhi_object_id .

    METHODS deploy_nhi_obj
      IMPORTING
        iv_content         TYPE string
        ir_nhi_obj_id      TYPE REF TO cl_nhi_object_id
        iv_detlevel        TYPE ballevel OPTIONAL
        iv_force           TYPE rs_bool DEFAULT rs_c_false
      EXPORTING
        et_rsmsg           TYPE rs_t_msg
        et_msg             TYPE rs2hana_view_t_msg
        ets_affected_xrefs TYPE gtyts_xrefs
      RAISING
        cx_rs2hana_view_nhi .
*        !i_active_version TYPE rs_bool DEFAULT rs_c_false   "false means newest available version

    METHODS get_repository_info
      IMPORTING
        ir_nhi_obj_id TYPE REF TO cl_nhi_object_id
      EXPORTING
        ets_outgoing  TYPE gtyts_xrefs
        ets_incoming  TYPE gtyts_xrefs
        ev_xml_string TYPE string
      RAISING
        cx_rs2hana_view_nhi .

    METHODS get_package_types
      RETURNING
        VALUE(rt_object_type_list) TYPE cl_nhi_object_type_description=>ty_objecttypes
      RAISING
        cx_rs2hana_view_nhi .

    METHODS delete_package
      IMPORTING
        iv_package TYPE rs2hana_view_package
      RAISING
        cx_rs2hana_view_nhi .

    METHODS check_package
      RETURNING
        VALUE(rv_exists) type rs_bool
      RAISING
        cx_rs2hana_view_nhi .

    METHODS create_package
      RAISING
        cx_rs2hana_view_nhi .

    CLASS-METHODS get_package_info
      IMPORTING
        iv_package              TYPE rs2hana_view_package
      RETURNING
        VALUE(rr_read_response) TYPE REF TO cl_nhi_read_package_res
      RAISING
        cx_rs2hana_view_nhi .

    METHODS get_subpackages
      IMPORTING
        iv_package            TYPE string OPTIONAL
      RETURNING
        VALUE(rt_subpackages) TYPE cl_nhi_package_info=>ty_packages
      RAISING
        cx_nhi_hana_repository .

    CLASS-METHODS __nhi_check_result_2_rs_msg
      IMPORTING
        ir_nhi_check_result TYPE REF TO cl_nhi_check_result
        iv_detlevel         TYPE ballevel OPTIONAL
      EXPORTING
        et_rsmsg            TYPE rs_t_msg
        et_msg              TYPE rs2hana_view_t_msg .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF gs_error_codes,
        empty(1)              TYPE c VALUE abap_false,
        ok(1)                 TYPE c VALUE '0',
        info(5)               TYPE c VALUE '40137',
        del_notfound(5)       TYPE c VALUE '40112',
        rollback(8)           TYPE c VALUE '8000037',
        o_rollback(8)         TYPE c VALUE '8000039',
        inconsistent_model(5) TYPE c VALUE '40117',
        invalid_table_name(8) TYPE c VALUE '71000259',
      END   OF gs_error_codes .
    CONSTANTS cp_sys_bic TYPE string VALUE '_SYS_BIC' ##NO_TEXT.
    CONSTANTS pv_workspace TYPE string VALUE '' ##NO_TEXT.
    CONSTANTS pv_tenant TYPE string VALUE '' ##NO_TEXT.

    CLASS-DATA gr_nhi_object TYPE REF TO if_nhi_object .
    CLASS-DATA gv_db_user(30)   TYPE c .
    CLASS-DATA gv_package TYPE string .
    CLASS-DATA gr_nhi_package TYPE REF TO if_nhi_package .

    DATA pr_inactive_session TYPE REF TO cl_nhi_inactive_session .
    DATA pv_owner TYPE string .

    CLASS-METHODS __nhi_error_2_anv_error
      IMPORTING
        ir_nhi_error            TYPE REF TO cl_nhi_error
      RETURNING
        VALUE(rr_anv_nhi_error) TYPE REF TO cx_rs2hana_view_nhi .
    METHODS __get_inactive_session
      RETURNING
        VALUE(rr_inactive_session) TYPE REF TO cl_nhi_inactive_session .
    CLASS-METHODS __is_error_code_exception
      IMPORTING
        iv_error_code      TYPE string
      RETURNING
        VALUE(rv_is_error) TYPE rs_bool .
    METHODS __check_package
      RAISING
        cx_rs2hana_view_nhi .
    CLASS-METHODS __is_error_code_invalidation
      IMPORTING
        iv_error_code      TYPE string
      RETURNING
        VALUE(rv_is_error) TYPE rs_bool .
    METHODS __get_xrefs
      IMPORTING
        it_r_refs  TYPE cl_nhi_reference_info=>ty_references
      EXPORTING
        !ets_xrefs TYPE gtyts_xrefs .
    METHODS __activate
      IMPORTING
        ir_nhi_obj_id      TYPE REF TO cl_nhi_object_id
        its_xrefs          TYPE gtyts_xrefs
        ir_metadata        TYPE REF TO cl_nhi_metadata OPTIONAL
        iv_force           TYPE abap_bool DEFAULT abap_false
        iv_detlevel        TYPE ballevel OPTIONAL
      EXPORTING
        ets_affected_xrefs TYPE gtyts_xrefs
        et_rsmsg           TYPE rs_t_msg
        et_msg             TYPE rs2hana_view_t_msg
      RAISING
        cx_rs2hana_view_nhi
        cx_nhi_hana_repository .
    CLASS-METHODS __check_nhi_response
      IMPORTING
        ir_nhi_error TYPE REF TO cl_nhi_error
      RAISING
        cx_rs2hana_view_nhi .
    CLASS-METHODS __parse_checkresults_xrefs
      IMPORTING
        its_checkresults TYPE cl_nhi_check_result=>ty_checkresults
        its_xrefs        TYPE gtyts_xrefs
      RETURNING
        VALUE(rts_xrefs) TYPE gtyts_xrefs .
ENDCLASS.



CLASS zcl_core_nhi_wrapper IMPLEMENTATION.

  METHOD check_nhi_obj_exist.

    DATA: l_r_exists_response TYPE REF TO cl_nhi_object_exists_res,
          l_r_exc             TYPE REF TO cx_root.

    TRY.
        l_r_exists_response = gr_nhi_object->exists( gr_nhi_object->create_object_exists_req(
                                                    session  = cl_nhi_inactive_session=>create_inactive_session(
                                                                 owner     = pv_owner
                                                                 workspace = pv_workspace )
                                                    object   = ir_nhi_obj_id
                                                )  ).
        IF l_r_exists_response->exists = abap_true.
          r_exists = rs_c_true.
        ELSE.
          r_exists = rs_c_false.
        ENDIF.
      CATCH cx_nhi_hana_repository INTO l_r_exc.
        RAISE EXCEPTION TYPE cx_rs2hana_view_nhi
          EXPORTING
            previous = l_r_exc.
    ENDTRY.

  ENDMETHOD.


  METHOD class_constructor.

    CALL FUNCTION 'DB_DBUSER'
      IMPORTING
        dbuser = gv_db_user.
    gr_nhi_package = cl_nhi_api=>create_instance( )->get_package( ).
    gr_nhi_object = cl_nhi_api=>create_instance( )->get_object( ).

  ENDMETHOD.

  METHOD read_nhi_obj.

    " So the hierarchy of the role in HANA is given from AUTH and above
    " this makes it easyer for the security to get access across all
    " clsuter just having access to AUTH

    TRY.
        DATA(lr_nhi_object_read) = gr_nhi_object->read(  request = gr_nhi_object->create_read_object_req(
                   lang    = |{ sy-langu }|
                   object  = ir_nhi_obj_id
                   session = cl_nhi_session=>create_session( EXPORTING sessiontype = ce_nhi_session_type=>st_active_session )
                   version = cl_nhi_version=>create_version( EXPORTING versiontype = ce_nhi_version_type=>vt_active_version ) ) ).
        __check_nhi_response( lr_nhi_object_read ).

        data lv_string type string.
        lv_string = lr_nhi_object_read->cdata.
        split lv_string AT cl_abap_char_utilities=>newline into table rt_content.

      CATCH cx_nhi_hana_repository INTO DATA(lr_exc).
        RAISE EXCEPTION TYPE cx_rs2hana_view_nhi
          EXPORTING
            previous = lr_exc.
    ENDTRY.
  endmethod.

  METHOD constructor.
    nv_package = iv_package.
    pv_owner   = gv_db_user.
  ENDMETHOD.


  METHOD delete_nhi_obj.

    DATA: lr_exc       TYPE REF TO cx_root,
          ltr_obj_list TYPE cl_nhi_object_id=>ty_objlist,
          lts_incoming TYPE gtyts_xrefs.

    TRY.
        IF rs_c_true = check_nhi_obj_exist(  ir_nhi_obj_id = ir_nhi_obj_id ).
          "get dependent objects which will be affected by deletion
          get_repository_info( EXPORTING ir_nhi_obj_id = ir_nhi_obj_id
                                IMPORTING ets_incoming  = lts_incoming ).

        ENDIF.
        __check_nhi_response( gr_nhi_object->delete( gr_nhi_object->create_delete_object_req(
                                                    action   = cl_nhi_delete_object_req=>co_action
                                                    what     = cl_nhi_delete_object_req=>co_object
                                                    session  = __get_inactive_session( )
                                                    object   = ir_nhi_obj_id
                                                )  )
         ).

        __activate(
          EXPORTING
            iv_force              = iv_force
            ir_nhi_obj_id         = ir_nhi_obj_id
            its_xrefs             = lts_incoming
          IMPORTING
            ets_affected_xrefs    = ets_affected_xrefs ).

      CATCH cx_nhi_hana_repository
            cx_rs2hana_view_nhi INTO lr_exc.
        IF rs_c_false = check_nhi_obj_exist( ir_nhi_obj_id = get_nhi_obj_id(
                         iv_id            = ir_nhi_obj_id->name
                         ir_type          = ir_nhi_obj_id->type
                     )  ).
          "because only inactive version existed
          "or the dependent HANA objects have a problem without this view
          "the activation part failed.
          RETURN.
        ELSE.
          RAISE EXCEPTION TYPE cx_rs2hana_view_nhi
            EXPORTING
              previous = lr_exc.
        ENDIF.
    ENDTRY.

  ENDMETHOD.

  method create_package.

    call METHOD __check_package.

  ENDMETHOD.

  METHOD check_package.
    TRY .
        rv_exists = gr_nhi_package->exists( gr_nhi_package->create_exists_package_req(
                                      tenant  = pv_tenant
                                      package = nv_package ) )->exists.
      CATCH cx_nhi_hana_repository INTO data(lr_exc).
        RAISE EXCEPTION TYPE cx_rs2hana_view_nhi
          EXPORTING
            previous = lr_exc.
    ENDTRY.

  ENDMETHOD.

  METHOD delete_package.

    DATA: lr_nhi_package TYPE REF TO if_nhi_package,
          lv_package     TYPE string,
          lr_exc         TYPE REF TO cx_root.

    IF iv_package IS INITIAL.
      lv_package = nv_package.
    ELSE.
      lv_package = iv_package.
    ENDIF.
    lr_nhi_package = cl_nhi_api=>create_instance( )->get_package( ).
    TRY.
        __check_nhi_response( lr_nhi_package->delete( lr_nhi_package->create_delete_package_req(
                                                 tenant  = pv_tenant
                                                 package = lv_package )
                                              )
                            ).
      CATCH cx_nhi_hana_repository INTO lr_exc.
        RAISE EXCEPTION TYPE cx_rs2hana_view_nhi
          EXPORTING
            previous = lr_exc.
    ENDTRY.

  ENDMETHOD.


  METHOD deploy_nhi_obj.

    DATA: lv_xcontent          TYPE xstring,
          lr_exc               TYPE REF TO cx_root,
          ltr_xref             TYPE cl_nhi_reference_info=>ty_references,
          ltr_obj_list         TYPE cl_nhi_object_id=>ty_objlist,
          lr_metadata          TYPE REF TO cl_nhi_metadata, "cl_nhi_metadata_active_ver,
          lr_metadata_response TYPE REF TO cl_nhi_read_obj_metadata_res,
          lts_incoming         TYPE gtyts_xrefs.


    CLEAR: et_rsmsg, ets_affected_xrefs, et_msg.

    TRY.
        __check_package( ).

        IF rs_c_true = check_nhi_obj_exist( ir_nhi_obj_id = ir_nhi_obj_id ).
          lr_metadata_response = gr_nhi_object->read_metadata( gr_nhi_object->create_read_obj_metadata_req(
                     object        = ir_nhi_obj_id
                     session       = __get_inactive_session( ) )
          ).
          __check_nhi_response( lr_metadata_response ).
          lr_metadata ?= lr_metadata_response->metadata.
          "get dependent objects which will be affected by activation
          get_repository_info( EXPORTING ir_nhi_obj_id =  ir_nhi_obj_id
                                IMPORTING ets_incoming  = lts_incoming ).

        ELSE.
          lr_metadata = cl_nhi_metadata_active_ver=>create_metadata( version_id   = ''
                                                                     activated_at = ''
                                                                     activated_by = ''
                                                                     edit         = '' ).
        ENDIF.

        __check_nhi_response( gr_nhi_object->write(
          gr_nhi_object->create_write_object_req(
            EXPORTING
              determinereferences = abap_true
              xreflist            = ltr_xref
              object              = ir_nhi_obj_id
              session             = __get_inactive_session( )
              metadata            = lr_metadata
              cdata               = iv_content
              bdata               = lv_xcontent
            )
          )
        ).

        __activate(
          EXPORTING
            ir_nhi_obj_id         = ir_nhi_obj_id
            its_xrefs             = lts_incoming
            ir_metadata           = lr_metadata
            iv_detlevel             = iv_detlevel
            "if this is the first version we force the activation
            "to avoid problems with possible leftover HANA objects which
            "used this view before it was deleted
            iv_force                = boolc( lr_metadata->version_id = '' OR iv_force = rs_c_true )
          IMPORTING
            ets_affected_xrefs    = ets_affected_xrefs
            et_rsmsg              = et_rsmsg
            et_msg                = et_msg ).

      CATCH cx_nhi_hana_repository cx_nhi_not_supported INTO lr_exc.
        RAISE EXCEPTION TYPE cx_rs2hana_view_nhi
          EXPORTING
            previous = lr_exc.
    ENDTRY.


  ENDMETHOD.


  METHOD get_nhi_obj_id.

    DATA: lr_version           TYPE REF TO cl_nhi_version,
          lv_xcontent          TYPE xstring,
          lr_metadata          TYPE REF TO cl_nhi_metadata,
          lr_metadata_response TYPE REF TO cl_nhi_read_obj_metadata_res.

    IF iv_active_version = rs_c_true.
      lr_version = cl_nhi_active_version=>create_active_version( ).
    ELSE.
      lr_version = cl_nhi_inactive_version=>create_inactive_version(
                    owner     = pv_owner
                    workspace = pv_workspace ).
    ENDIF.

    rr_nhi_object_id = cl_nhi_object_id=>create_object_id(
                          tenant   = pv_tenant
                          package  = nv_package
                          name     = iv_id
                          type     = ir_type
                          version  = lr_version
                          metadata = cl_nhi_metadata_active_ver=>create_metadata(
                                                              version_id   = ''
                                                              activated_at = ''
                                                              activated_by = ''
                                                              edit         = '' ) ).
    TRY.
        lr_metadata_response = gr_nhi_object->read_metadata( gr_nhi_object->create_read_obj_metadata_req(
                   object        = rr_nhi_object_id
                   session       = __get_inactive_session( )
                   version       = lr_version )
        ).
        __check_nhi_response( lr_metadata_response ).
        IF lr_metadata_response->error_code NE gs_error_codes-del_notfound.
          lr_metadata ?= lr_metadata_response->metadata.
          rr_nhi_object_id = cl_nhi_object_id=>create_object_id(
                            tenant   = pv_tenant
                            package  = nv_package
                            name     = iv_id
                            type     = ir_type
                            version  = lr_version
                            metadata = lr_metadata ).
        ENDIF.
      CATCH cx_rs2hana_view_nhi
            cx_nhi_hana_repository.

    ENDTRY.

  ENDMETHOD.


  METHOD get_package_info.

    DATA(lr_nhi_package) = cl_nhi_api=>create_instance( )->get_package( ).
    TRY.
        rr_read_response = lr_nhi_package->read( request = lr_nhi_package->create_read_package_req(
                                           tenant        = pv_tenant
                                           package       = |{ iv_package }|
                                       ) ).
      CATCH cx_nhi_hana_repository INTO DATA(lr_exc).
        RAISE EXCEPTION TYPE cx_rs2hana_view_nhi
          EXPORTING
            previous = lr_exc.
    ENDTRY.

  ENDMETHOD.


  METHOD get_package_types.

    DATA: lr_list_types_res TYPE REF TO cl_nhi_list_object_types_res,
          lr_exc            TYPE REF TO cx_root.

    TRY.
        lr_list_types_res =
          gr_nhi_object->list_types( gr_nhi_object->create_list_object_types_req(
                                      tenant                  = pv_tenant
                                      package                 = nv_package
                                      restrict2designtimeonly = abap_true
                                      session                 = __get_inactive_session( ) )
                                   ).
        __check_nhi_response( lr_list_types_res ).
        rt_object_type_list = lr_list_types_res->objecttypes.
      CATCH cx_nhi_hana_repository INTO lr_exc.
        RAISE EXCEPTION TYPE cx_rs2hana_view_nhi
          EXPORTING
            previous = lr_exc.
    ENDTRY.

  ENDMETHOD.


  METHOD get_repository_info.

    DATA: lr_exc               TYPE REF TO cx_root,
          lr_metadata_response TYPE REF TO cl_nhi_read_obj_metadata_res,
          lr_read_obj_response TYPE REF TO cl_nhi_read_object_res,
          ls_xref              TYPE gtys_xref,
          lv_get_outgoing      TYPE abap_bool,
          lv_get_incoming      TYPE abap_bool,
          lr_version           TYPE REF TO cl_nhi_version.

    CLEAR: ets_incoming, ets_outgoing, ev_xml_string.
    TRY.
*        IF i_active_version = rs_c_true.
*          cl_nhi_version=>create_version( ce_nhi_version_type=>vt_active_version ).
*        ENDIF.
        lr_metadata_response = gr_nhi_object->read_metadata( gr_nhi_object->create_read_obj_metadata_req(
                     object        = ir_nhi_obj_id
                     session       = __get_inactive_session( )
                     version       = lr_version )
                     ).
        IF lr_metadata_response->metadata IS BOUND.
          IF ets_incoming IS REQUESTED.
            lv_get_incoming = abap_true.
          ENDIF.
          IF ets_outgoing IS REQUESTED.
            lv_get_outgoing = abap_true.
          ENDIF.
          lr_read_obj_response = gr_nhi_object->read( gr_nhi_object->create_read_object_req(
                            lang                 = ''
                            getoutgoingrefs      = lv_get_outgoing
                            getincomingrefs      = lv_get_incoming
*                          getreferencedobjects = ABAP_FALSE  "this is only supported with protocol version repoV2 or higher
                            object               = ir_nhi_obj_id
                            session              = __get_inactive_session( )
                            version              = lr_metadata_response->version
                        )  ).
          __get_xrefs(
            EXPORTING
              it_r_refs = lr_read_obj_response->incomingrefs
            IMPORTING
              ets_xrefs = ets_incoming ).
          __get_xrefs(
            EXPORTING
              it_r_refs = lr_read_obj_response->outgoingrefs
            IMPORTING
              ets_xrefs = ets_outgoing ).
          ev_xml_string = lr_read_obj_response->cdata.
        ENDIF.
      CATCH cx_nhi_hana_repository cx_nhi_not_supported INTO lr_exc.
        RAISE EXCEPTION TYPE cx_rs2hana_view_nhi
          EXPORTING
            previous = lr_exc.
    ENDTRY.
  ENDMETHOD.


  METHOD get_subpackages.


    IF iv_package IS INITIAL.
      DATA(lr_subpack) = gr_nhi_package->get_sub_packages( request = gr_nhi_package->create_get_sub_packages_req( package = nv_package ) ).
    ELSE.
      lr_subpack = gr_nhi_package->get_sub_packages( request = gr_nhi_package->create_get_sub_packages_req( package = iv_package ) ).
    ENDIF.
    rt_subpackages = lr_subpack->subpackages.

  ENDMETHOD.

  METHOD __activate.

    DATA: lr_activate_response TYPE REF TO cl_nhi_activate_objects_res,
          ltr_obj_list         TYPE cl_nhi_object_id=>ty_objlist,
          lt_rsmsg             TYPE rs_t_msg,
          lt_msg               TYPE rs2hana_view_t_msg,
          lr_exc               TYPE REF TO cx_rs2hana_view_nhi,
          lr_metadata_response TYPE REF TO cl_nhi_read_obj_metadata_res,
          lv_old_version       TYPE i,
          lv_new_version       TYPE i,
          lr_activation_mode   TYPE REF TO ce_nhi_activation_mode.

    CLEAR: ets_affected_xrefs, et_rsmsg, et_msg.
    APPEND ir_nhi_obj_id TO ltr_obj_list.
    IF iv_force = abap_true.
      lr_activation_mode = ce_nhi_activation_mode=>activation_optimistic.
    ELSE.
      lr_activation_mode = ce_nhi_activation_mode=>activation_casc_2_phase.
    ENDIF.
    lr_activate_response = gr_nhi_object->activate( gr_nhi_object->create_activate_objects_req(
                                                       activationmode = lr_activation_mode
                                                       objlist        = ltr_obj_list
                                                       session        = __get_inactive_session( )
                                                   )
    ).
    LOOP AT lr_activate_response->checkresults ASSIGNING FIELD-SYMBOL(<l_r_checkresult>).
      __nhi_check_result_2_rs_msg( EXPORTING ir_nhi_check_result = <l_r_checkresult>
                                             iv_detlevel = iv_detlevel
                                   IMPORTING et_rsmsg = lt_rsmsg
                                             et_msg   = lt_msg ) .
      APPEND LINES OF lt_rsmsg TO et_rsmsg.
      APPEND LINES OF lt_msg TO et_msg.
    ENDLOOP.
    TRY.
        __check_nhi_response( lr_activate_response ).
      CATCH cx_rs2hana_view_nhi INTO lr_exc.
        "N2633632 Remove the xrefs that don't actually have a validation error for more accurate logging
        ets_affected_xrefs = __parse_checkresults_xrefs(
            EXPORTING   its_xrefs          = its_xrefs
                        its_checkresults   = lr_activate_response->checkresults ).
        IF its_xrefs IS NOT INITIAL AND ir_metadata IS BOUND.
          "check if the error is because the view could not be activated or it`s
          "one of the depending HANA Objects
          lr_metadata_response = gr_nhi_object->read_metadata( gr_nhi_object->create_read_obj_metadata_req(
                     object        = ir_nhi_obj_id
                     session       = __get_inactive_session( ) ) ).
          lv_new_version = lr_metadata_response->metadata->version_id.
          lv_old_version = ir_metadata->version_id.
          IF NOT ( lv_new_version >= lv_old_version
           AND lr_metadata_response->version->versiontype EQ ce_nhi_version_type=>vt_active_version ).
            "after writing new model and activating it we need to have a new active model.
            "if so, the problem should be one of the depending HANA Objects
            "if not, the problem is the activation of the current view
            RAISE EXCEPTION lr_exc.
          ENDIF.
        ELSE.
          RAISE EXCEPTION lr_exc.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD __check_nhi_response.

    DATA: lr_anv_error TYPE REF TO cx_rs2hana_view_nhi.

    IF rs_c_true = __is_error_code_exception( ir_nhi_error->error_code ).
      lr_anv_error = __nhi_error_2_anv_error( ir_nhi_error ).
      RAISE EXCEPTION lr_anv_error.
    ENDIF.

  ENDMETHOD.


  METHOD __check_package.

    DATA: lr_exc         TYPE REF TO cx_root,
          lv_description TYPE string,
          lt_texts       TYPE cl_nhi_text=>ty_texts,
          lv_package     TYPE rs2hana_view_package.

    TRY .
        IF gr_nhi_package->exists( gr_nhi_package->create_exists_package_req(
                                      tenant  = pv_tenant
                                      package = nv_package ) )->exists = abap_false.
*         package does not exist => create package
          MESSAGE s011(rs2hana_view) INTO lv_description.
          __check_nhi_response( gr_nhi_package->create(
            gr_nhi_package->create_create_package_req(
              tenant                  = pv_tenant
              package                 = nv_package
              description             = lv_description
              responsible             = pv_owner
              orig_lang               = ''
              structural              = abap_false
              delivery_unit           = ''
              du_vendor               = ''
              text_collection         = ''
              text_status             = ''
              text_terminology_domain = ''
              hints_for_translation   = ''
              texts                   = lt_texts ) )
          ).
        ENDIF.
      CATCH cx_nhi_hana_repository INTO lr_exc.
        RAISE EXCEPTION TYPE cx_rs2hana_view_nhi
          EXPORTING
            previous = lr_exc.
    ENDTRY.

  ENDMETHOD.


  METHOD __get_inactive_session.

    IF pr_inactive_session IS NOT BOUND.
      rr_inactive_session = pr_inactive_session = cl_nhi_inactive_session=>create_inactive_session(
                                                      owner     = pv_owner
                                                      workspace = pv_workspace ).
    ELSE.
      rr_inactive_session = pr_inactive_session.
    ENDIF.

  ENDMETHOD.


  METHOD __get_xrefs.

    DATA: ls_xref LIKE LINE OF ets_xrefs.

    CLEAR ets_xrefs.
    LOOP AT it_r_refs ASSIGNING FIELD-SYMBOL(<lr_ref>).
      CHECK <lr_ref>->object->package NE cp_sys_bic.
      ls_xref-package    = <lr_ref>->object->package.
      ls_xref-name       = <lr_ref>->object->name.
      ls_xref-type_descr = <lr_ref>->object->type->description.
      INSERT ls_xref INTO TABLE ets_xrefs.
    ENDLOOP.

  ENDMETHOD.


  METHOD __is_error_code_exception.

    IF   iv_error_code EQ gs_error_codes-empty
      OR iv_error_code EQ gs_error_codes-info
      OR iv_error_code EQ gs_error_codes-ok
      OR iv_error_code EQ gs_error_codes-del_notfound.
      rv_is_error = rs_c_false.
    ELSE.
      rv_is_error = rs_c_true.
    ENDIF.

  ENDMETHOD.


  METHOD __is_error_code_invalidation.
    IF     iv_error_code EQ gs_error_codes-inconsistent_model
        OR iv_error_code EQ gs_error_codes-invalid_table_name.
      rv_is_error = rs_c_true.
    ELSE.
      rv_is_error = rs_c_false.
    ENDIF.
  ENDMETHOD.


  METHOD __nhi_check_result_2_rs_msg.

    DATA: lv_msg(200) TYPE c,
          ls_msg      TYPE rs_s_msg,
          lv_pos      TYPE i,
          lv_off      TYPE i VALUE 150,
          lv_strlen   TYPE i.

    CLEAR: et_rsmsg, et_msg.


    lv_strlen = strlen( ir_nhi_check_result->error_msg ).

    ls_msg-msgid = 'RS2HANA_VIEW'.
    ls_msg-msgno = 004.
    ls_msg-detlevel = iv_detlevel.
    IF  ir_nhi_check_result->severity NE 'INFO' AND rs_c_true = __is_error_code_exception( ir_nhi_check_result->error_code ).
      ls_msg-msgty = rsz_c_message_type-e.
    ELSE.
      ls_msg-msgty = rsz_c_message_type-s.
      "note 2292431: reduce msg - remove hierarchy view messages
      IF lv_strlen > 30 AND ir_nhi_check_result->error_msg(22) = 'Create hierarchy view:'.
        RETURN.
      ENDIF.
    ENDIF.
    ls_msg-msgv1 = ir_nhi_check_result->error_code.


    "note 2292431: limit length of messages
    DATA(lv_error_msg) = ir_nhi_check_result->error_msg.
    IF lv_strlen > 1000.
      lv_error_msg = lv_error_msg(995) && '[...]'.
      lv_strlen = 1000.
    ENDIF.
    APPEND VALUE #( msgty = ls_msg-msgty detlevel = iv_detlevel msg = lv_error_msg ) TO et_msg.
    WHILE lv_strlen > 0.
      IF lv_strlen > lv_off.
        lv_msg = lv_error_msg+lv_pos(lv_off).
      ELSE.
        lv_msg = lv_error_msg+lv_pos(lv_strlen).
      ENDIF.

      ls_msg-msgv2 = lv_msg(50).
      ls_msg-msgv3 = lv_msg+50(50).
      ls_msg-msgv4 = lv_msg+100(50).
      APPEND ls_msg TO et_rsmsg.
      SUBTRACT lv_off FROM lv_strlen.
      ADD lv_off TO lv_pos.
    ENDWHILE.

  ENDMETHOD.


  METHOD __nhi_error_2_anv_error.

    DATA: ls_msg      LIKE cx_rs2hana_view_nhi=>if_t100_message~t100key,
          lv_v2       TYPE symsgv,
          lv_v3       TYPE symsgv,
          lv_v4       TYPE symsgv,
          lv_msg(200) TYPE c.

    ls_msg-msgid = 'RS2HANA_VIEW'.
    ls_msg-msgno = 004.
    ls_msg-attr1 = 'V1'.
    ls_msg-attr2 = 'V2'.
    ls_msg-attr3 = 'V3'.
    ls_msg-attr4 = 'V4'.
    lv_msg = ir_nhi_error->error_msg.

    CALL FUNCTION 'RSDG_WORD_WRAP'
      EXPORTING
        textline            = lv_msg
        outputlen           = 50
      IMPORTING
        out_line1           = lv_v2
        out_line2           = lv_v3
        out_line3           = lv_v4
      EXCEPTIONS
        outputlen_too_large = 1
        OTHERS              = 2.
    IF sy-subrc NE 0.
      lv_v2 = ir_nhi_error->error_msg.
    ENDIF.


    CREATE OBJECT rr_anv_nhi_error
      EXPORTING
        textid = ls_msg
        v1     = ir_nhi_error->error_code && '/' && ir_nhi_error->error_arg
        v2     = lv_v2
        v3     = lv_v3
        v4     = lv_v4.

  ENDMETHOD.


  METHOD __parse_checkresults_xrefs.

    DATA: lv_invalidated TYPE rs_bool.

    rts_xrefs = its_xrefs.
    LOOP AT rts_xrefs ASSIGNING FIELD-SYMBOL(<ls_xref>).
      lv_invalidated = rs_c_false.
      LOOP AT its_checkresults ASSIGNING FIELD-SYMBOL(<lv_checkresult>)
          WHERE table_line->object->name = <ls_xref>-name AND table_line->object->package = <ls_xref>-package AND table_line->error_code <> 0.
        IF __is_error_code_invalidation( <lv_checkresult>->error_code ).
          lv_invalidated = rs_c_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_invalidated = rs_c_false.
        DELETE rts_xrefs WHERE name = <ls_xref>-name AND package = <ls_xref>-package.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

