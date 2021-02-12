CLASS zcl_core_basis_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    constants:
      begin of gc_parameters,
        "! default value ROOT
        prefix_cluster type string value 'PREFIX_CLUSTER',
        "! Default value MDA
        mda_section    type string value 'MDA_SECTION',
        "! Default value EDW
        edw_section    type string value 'EDW_SECTION',
        "! Default value IAP
        iap_section    type string value 'IAP_SECTION',
        "! Default value AAV
        aav_section    type string value 'AAV_SECTION',
        "! Default value EXZ
        exz_section    type string value 'EXZ_SECTION',
        "! Default value SYS
        sys_section    type string value 'SYS_SECTION',
        "! Defailt value ALL
        all_postfix    type string value 'ALL_POSTFIX',
        "! Auth default block AUTH
        auth_block     type string value 'AUTH_BLOCK',
        "! Prefix for role default BW4
        prefix_role    type string value 'PREFIX_ROLE',
        "! P,lace to keep the cluster definitions SYSX_ALL
        infoareacluster type string value 'INFOAREA_CLUSTER',
        "! Block for authorization AUTH
        auth_root      type string value 'AUTH_ROOT',
      end of gc_parameters.

    CLASS-DATA:
      gs_setup TYPE zcore_setup.

    CLASS-METHODS:
      get_c
        IMPORTING
          iv_constant     TYPE string
        RETURNING
          VALUE(rv_value) TYPE char30,
      hana_role_create
        IMPORTING
          iv_rolename TYPE agr_name
        RAISING
          cx_nhi_adt_error,
      hana_role_read
        IMPORTING
          iv_rolename       TYPE agr_name
        RETURNING
          VALUE(rv_content) TYPE string
        RAISING
          cx_rs_not_found
          cx_rs_error,
      hana_package_create
        IMPORTING
          iv_parent         TYPE string OPTIONAL
          iv_package        TYPE string
          iv_structural     TYPE rs_bool DEFAULT rs_c_false
          iv_txtlg          TYPE clike
        RETURNING
          VALUE(rv_package) TYPE string
        RAISING
          cx_rs_msg,
      infoarea_create
        IMPORTING
          iv_parent   TYPE rsinfoarea
          iv_infoarea TYPE rsinfoarea
          iv_txtlg    TYPE rstxtlg
        RAISING
          cx_rs_msg.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS:
      hana_package_exists
        IMPORTING
          iv_package       TYPE string
        RETURNING
          VALUE(rv_exists) TYPE rs_bool,
      infoarea_exists
        IMPORTING
          iv_infoarea      TYPE rsinfoarea
        RETURNING
          VALUE(rv_exists) TYPE rs_bool.

    CLASS-METHODS:
      _hana_role_read
        IMPORTING
          iv_package             TYPE string
          iv_hdbrole             TYPE string
        RETURNING
          VALUE(rv_role_content) TYPE string
        RAISING
          cx_rs_not_found
          cx_rs_error,
      _cluster_section_block_as_area
        IMPORTING
          iv_cluster       TYPE zcore_cluster  OPTIONAL
          iv_section       TYPE zcore_section  OPTIONAL
          iv_block         TYPE zcore_block    OPTIONAL
        RETURNING
          VALUE(rv_exists) TYPE rs_bool.

ENDCLASS.


CLASS zcl_core_basis_tools IMPLEMENTATION.

  METHOD hana_role_create.


    DATA(lv_block)     = zcl_core_role_admin=>get_block_from_rolename( iv_rolename ).
    DATA(lv_roletype)  = zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ).
    DATA(lv_cluster)   = zcl_core_role_admin=>get_cluster_from_rolename( iv_rolename ).
    " Make sure the root package have been created
*    DATA(lv_txtlg)    = |Roles for Mixed models|.
*
*    "" This is to create the (make sure it's there) the AUTH structural package
*    data(lv_auth_package)  = hana_package_create(  iv_parent = || iv_package = |{ get_c( gc_parameters-auth_root ) }| iv_txtlg = lv_txtlg iv_structural = rs_c_true ).
*
*    "" create the AUTH.ROOTx as all role will be in this one... not a structural
*    lv_txtlg   = |Roles for Mixed models - Cluster { zcl_core_role_admin=>get_cluster_description( lv_cluster ) } |.
*    lv_auth_package = hana_package_create(  iv_parent = lv_auth_package iv_package = |{ get_c( gc_parameters-prefix_cluster ) }{ lv_cluster }| iv_txtlg = lv_txtlg iv_structural = rs_c_false ).
*
*    "" Prepare the role... there is only one role per block and this will span all blocks in across the sections
*    DATA lt_role_source     TYPE STANDARD TABLE OF string WITH NON-UNIQUE DEFAULT KEY.
*    APPEND |role { lv_auth_package }::{ iv_rolename } { '{' } | TO lt_role_source.
*    APPEND |// Privileges needed for anyone who wants to do data modeling in packages HILTI.03_Integration, HILTI.04_Analytics, etc. | TO lt_role_source.
*    APPEND |// This role contains everything required for modeling  except for: | TO lt_role_source.
*    APPEND |// - privileges on application data schemas | TO lt_role_source.
*    APPEND |// | TO lt_role_source.
*    APPEND |// This role must always be accompanied by the complementary roles including data schema access to enable data modeling. | TO lt_role_source.
*    APPEND |// | TO lt_role_source.
*    APPEND |  -- System Privileges:  | TO lt_role_source.
*    APPEND |  -- Allow the user to verify that the tables underlying the Information Models do exist.| TO lt_role_source.
*    APPEND |  -- We simply grant CATALOG READ, so the user has no SELECT Privilege on the underlying database tables.| TO lt_role_source.
*    APPEND |  -- This will allow reading _all_ metadata in the system, but no table contents !| TO lt_role_source.
*    APPEND |  system privilege: CATALOG READ; | TO lt_role_source.
*    APPEND |   | TO lt_role_source.
*    APPEND |  -- Here we allow the role to read the repository tree (expand the "content" tree). This does not allow seeing the content of packages:| TO lt_role_source.
*    APPEND |  catalog sql object "SYS"."REPOSITORY_REST": EXECUTE; | TO lt_role_source.
*    APPEND |   | TO lt_role_source.
*    APPEND |  -- We also enable activation of time-based Attribute Views as these are based on a standard table in schema _SYS_BI | TO lt_role_source.
*    APPEND |  -- for simplicity, we include the entire _SYS_BI schema which does not contain sensitive data | TO lt_role_source.
*    APPEND |  catalog schema "_SYS_BI": SELECT;| TO lt_role_source.
*    APPEND |  -- repository access:| TO lt_role_source.
*
*    "" Make sure the ROOTx package is created as a root
*    DATA(lv_root_package) = |{ get_c( 'PREFIX_CLUSTER' ) }{ lv_cluster }|.
*    lv_root_package = hana_package_create( iv_parent = '' iv_package = lv_root_package iv_txtlg = zcl_core_role_admin=>get_cluster_description( lv_cluster ) ).
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE zcl_core_role_admin=>gc_section TO FIELD-SYMBOL(<lv_section>).
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*      IF _cluster_section_block_as_area( iv_section = <lv_section> iv_block = zcl_core_role_admin=>get_block_from_rolename( iv_rolename ) ) = rs_c_true.
*        " If the infoarea is created the section.block the corresponding HANA is also created
*        " Create the package ROOTx.<Section>.<Block> and add the
*        DATA(lv_section_package) = hana_package_create( iv_parent = lv_root_package iv_package = |{ <lv_section> }| iv_txtlg = zcl_core_role_admin=>get_cluster_description( lv_cluster ) iv_structural = rs_c_true ).
*        " create the ROOTx.xxx.YYYY as an none structural block
*        DATA(lv_block_package)   = hana_package_create( iv_parent = lv_section_package iv_package = |{ lv_block }| iv_txtlg = zcl_core_role_admin=>get_cluster_description( lv_cluster ) iv_structural = rs_c_false ).
*
*        APPEND |  package : { lv_block_package }: | &&
*               |REPO.READ, REPO.EDIT_NATIVE_OBJECTS, REPO.ACTIVATE_NATIVE_OBJECTS, REPO.MAINTAIN_NATIVE_PACKAGES, REPO.ACTIVATE_IMPORTED_OBJECTS, REPO.MAINTAIN_IMPORTED_PACKAGES;| TO lt_role_source.
*      ENDIF.
*    ENDDO.
*    APPEND |  -- Read access to the generate external HANA View| TO lt_role_source.
*    APPEND |  package system-local: REPO.READ;  | TO lt_role_source.
*    APPEND |  -- Strictly speaking, no SELECT or EXECUTE privileges are required in | TO lt_role_source.
*    APPEND |  -- order to build and activate data models, as long as the modeler has access to the catalog metadata (system privilege CATALOG READ).| TO lt_role_source.
*    APPEND |  -- It is, however, easier to perform data modeling tasks if the modelers have SELECT access to the database schemas underlying the data models.| TO lt_role_source.
*    APPEND |  -- Change to whatever schema you need | TO lt_role_source.
*    APPEND |  catalog schema "_SYS_BIC": SELECT;| TO lt_role_source.
*    APPEND |  -- It is possible to add any schema created for the general use| TO lt_role_source.
*    APPEND '}' TO lt_role_source.
*
*    DATA lv_role_source TYPE string.
*    CONCATENATE LINES OF lt_role_source INTO lv_role_source RESPECTING BLANKS IN CHARACTER MODE SEPARATED BY cl_abap_char_utilities=>cr_lf.
*
*    DATA:
*      lr_nhi_api    TYPE REF TO if_nhi_api,
*      lr_nhi_object TYPE REF TO if_nhi_object.
*
*    DATA:
*
*      lt_types    TYPE ce_nhi_object_type=>ty_types,
*      lt_xreflist TYPE cl_nhi_reference_info=>ty_references,
*      lt_text     TYPE cl_nhi_text=>ty_texts.
*
*    lr_nhi_object = cl_nhi_object=>create_instance( ).
*
*    APPEND ce_nhi_object_type=>type_hdb_role TO lt_types.
*
*    TRY.
**    if lr_nhi_object->exists( lr_nhi_object->create_object_exists_req(
**                           object = lr_object
**                           session = cl_nhi_session=>create_session( ce_nhi_session_type=>st_active_session ) ) )->exists = abap_true.
*        " Role exists... we need to find the meta data object
*        READ TABLE lr_nhi_object->find( lr_nhi_object->create_find_objects_req(
*                                    package = lv_auth_package
*                                    name    = |{ iv_rolename }|
*                                    types   = lt_types
*                                    session = cl_nhi_session=>create_session( ce_nhi_session_type=>st_active_session ) ) )->objects INTO DATA(lr_object) INDEX 1.
*        " the above might throw an error and we need to deal with that later
*        IF sy-subrc <> 0.
*          DATA(lr_metadata) = cl_nhi_metadata_inactive_ver=>create_metadata( version_id = '' edit = abap_false is_deletion = abap_false last_changed_at = |{ sy-datum }| ).
*        ELSE.
*          lr_metadata ?= lr_object->metadata.
*        ENDIF.
*
*        DATA(lr_object_id) = cl_nhi_object_id=>create_object_id(
*                                name     = |{ iv_rolename }|
*                                package  = |{ lv_auth_package }|
*                                tenant   = ''
*                                type     = ce_nhi_object_type=>type_hdb_role
*                                metadata = lr_metadata
*                                version  = cl_nhi_inactive_version=>create_inactive_version( EXPORTING owner = cl_amdp_utils=>get_current_db_schema_name(  ) workspace = '' ) ).
*
*        APPEND cl_nhi_text=>create_text( content = 'Role create' max_length = |60| text_id = 'Description' text_type = 'ddd' ) TO lt_text.
*
*        DATA lvx_role_source TYPE xstring.
*
*        DATA(lr_session_inactive) = cl_nhi_inactive_session=>create_inactive_session( owner =  cl_amdp_utils=>get_current_db_schema_name(  )  workspace = '' ).
*        DATA(lr_nhi_object_write) = lr_nhi_object->write_inactive( request = lr_nhi_object->create_write_single_inact_req(
*                   cdata         = lv_role_source
*                   bdata         = lvx_role_source
*                   texts         = lt_text
*                   object        = lr_object_id
*                   xreflist      = lt_xreflist
*                   metadata      = lr_metadata
*                   content_texts = lt_text
*                   session       = lr_session_inactive ) ).
*        if lr_nhi_object_write->error_code = 40124.
*
*        endif.
*
*        IF lr_nhi_object_write->error_code <> 0.
*          MESSAGE lr_nhi_object_write->error_msg TYPE rs_c_error.
*        ENDIF.
*
*        data lt_objlist type cl_nhi_object_id=>ty_objlist.
*        append lr_object_id to lt_objlist.
*
*        data(lr_nhi_activate) = lr_nhi_object->activate( request = lr_nhi_object->create_activate_objects_req(
*                                                                        activationmode = ce_nhi_activation_mode=>activation_optimistic
*                                                                        objlist        = lt_objlist
*                                                                        session        = lr_session_inactive ) ).
*      CATCH cx_sy_assign_cast_error
*            cx_sy_move_cast_error INTO DATA(lrx_cast).
*        DATA(d) = lrx_cast->get_longtext(  ).
*        MESSAGE lrx_cast->get_text(  ) TYPE rs_c_error.
*      CATCH cx_nhi_hana_repository INTO DATA(lrx_nhi).
*        DATA(l) = lrx_nhi->get_longtext(  ).
*        MESSAGE lrx_nhi->get_text(  ) TYPE rs_c_error.
*    ENDTRY.

  ENDMETHOD.


  METHOD _cluster_section_block_as_area.
    " iv_cluster
    " iv_section
    " iv_block
    DATA lv_infoarea TYPE rsinfoarea.
    IF iv_block IS INITIAL.
      CONCATENATE iv_section iv_cluster INTO lv_infoarea.
      SELECT SINGLE infoarea
          FROM rsdarea
          WHERE objvers = @rs_c_objvers-active AND
                infoarea =  @lv_infoarea
          INTO @lv_infoarea.
    ELSEIF iv_cluster IS INITIAL.
      CONCATENATE iv_block '_' iv_section INTO lv_infoarea.
      SELECT SINGLE infoarea
          FROM rsdarea
          WHERE objvers = @rs_c_objvers-active AND
                infoarea = @lv_infoarea
          INTO @lv_infoarea.
    ENDIF.
    IF sy-subrc = 0.
      rv_exists = rs_c_true.
    ELSE.
      rv_exists = rs_c_false.
    ENDIF.

  ENDMETHOD.

  METHOD hana_role_read.

    DATA(lv_auth_package) = |{ get_c( 'AUTH_ROOT' ) }.{ get_c( 'PREFIX_CLUSTER' ) }{ zcl_core_role_admin=>get_cluster_from_rolename( iv_rolename ) }|.
    rv_content = _hana_role_read( iv_hdbrole = |{ iv_rolename }| iv_package = lv_auth_package ).

  ENDMETHOD.

  METHOD _hana_role_read.
    " So the hierarchy of the role in HANA is given from AUTH and above
    " this makes it easyer for the security to get access across all
    " clsuter just having access to AUTH

    DATA:
      lr_nhi_object TYPE REF TO if_nhi_object.
    lr_nhi_object = cl_nhi_object=>create_instance( ).

    TRY.
        DATA(lr_nhi_object_read) = lr_nhi_object->read(  request = lr_nhi_object->create_read_object_req(
                   lang    = |{ sy-langu }|
                   object  = cl_nhi_object_id=>create_object_id(
                        name    = iv_hdbrole
                        package = iv_package
                        tenant  = ''
                        type = ce_nhi_object_type=>type_hdb_role
                        version = cl_nhi_version=>create_version( EXPORTING versiontype = ce_nhi_version_type=>vt_active_version ) )
                   session = cl_nhi_session=>create_session( EXPORTING sessiontype = ce_nhi_session_type=>st_active_session )
                   version = cl_nhi_version=>create_version( EXPORTING versiontype = ce_nhi_version_type=>vt_active_version ) ) ).
        IF lr_nhi_object_read->error_code IS NOT INITIAL.
          RAISE EXCEPTION TYPE cx_rs_not_found
            EXPORTING
              key    = |{ iv_package }::{ iv_hdbrole }|
              object = |HANA role|.
        ELSE.
          rv_role_content = lr_nhi_object_read->cdata.
        ENDIF.

      CATCH cx_nhi_hana_repository INTO DATA(lrx_nhi).
        MESSAGE lrx_nhi->get_text(  ) TYPE rs_c_error.
        RAISE EXCEPTION TYPE cx_rs_error
          EXPORTING
            previous = lrx_nhi.
    ENDTRY.

  ENDMETHOD.

  METHOD get_c.
    IF gs_setup IS INITIAL.
      SELECT SINGLE *
          FROM zcore_setup
          WHERE customizid = 'BW'
          INTO @gs_setup.
    ENDIF.

    ASSIGN COMPONENT iv_constant OF STRUCTURE gs_setup TO FIELD-SYMBOL(<lv_constant>).
    IF sy-subrc = 0.
      rv_value = <lv_constant>.
    ELSE.
      CLEAR rv_value.
    ENDIF.

    IF rv_value IS INITIAL.
      CASE iv_constant.
        WHEN 'PREFIX_CLUSTER'.
          rv_value = 'ROOT'.
        WHEN 'MDA_SECTION'.
          rv_value = 'MDA'.
        WHEN 'EDW_SECTION'.
          rv_value = 'EDW'.
        WHEN 'IAP_SECTION'.
          rv_value = 'IAP'.
        WHEN 'AAV_SECTION'.
          rv_value = 'AAV'.
        WHEN 'EXZ_SECTION'.
          rv_value = 'EXZ'.
        WHEN 'SYS_SECTION'.
          rv_value = 'SYS'.
        WHEN 'ALL_POSTFIX'.
          rv_value = 'ALL'.
        WHEN 'AUTH_BLOCK'.
          rv_value = 'AUTH'.
        WHEN 'PREFIX_ROLE'.
          rv_value = 'BW4'.
        WHEN 'INFOAREA_CLUSTER'.
          rv_value = 'SYSX_ALL'.
        WHEN 'AUTH_ROOT'.
          rv_value = 'AUTH'.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD infoarea_exists.


    IF iv_infoarea IS INITIAL.
      rv_exists = rs_c_true.
      RETURN.
    ELSE.
      rv_exists = rs_c_false.
    ENDIF.

    SELECT SINGLE infoarea
        FROM rsdarea
        WHERE infoarea = @iv_infoarea AND
              objvers  = @rs_c_objvers-active
        INTO @DATA(lv_infoarea).
    IF sy-subrc = 0.
      rv_exists = rs_c_true.
    ENDIF.

  ENDMETHOD.

  METHOD hana_package_exists.

    DATA:
      lr_nhi_api     TYPE REF TO if_nhi_api,
      lr_nhi_package TYPE REF TO if_nhi_package.

    IF iv_package IS INITIAL.
      rv_exists = rs_c_true.
      RETURN.
    ELSE.
      rv_exists = rs_c_false.
    ENDIF.

    TRY.
        lr_nhi_api     = cl_nhi_api=>create_instance(  ).
        lr_nhi_package = lr_nhi_api->get_package(  ).

        DATA(lr_nhi_package_read) = lr_nhi_package->read(  request = lr_nhi_package->create_read_package_req( tenant = '' package = |{ iv_package }| ) ).

        IF lr_nhi_package_read->error_code IS INITIAL.
          rv_exists = rs_c_true.
        ENDIF.

      CATCH cx_nhi_hana_repository INTO DATA(lrx_nhi).
        MESSAGE lrx_nhi->get_text(  ) TYPE rs_c_error.
    ENDTRY.

  ENDMETHOD.

  METHOD hana_package_create.

    DATA:
      lt_texts       TYPE cl_nhi_text=>ty_texts,
      lr_nhi_api     TYPE REF TO if_nhi_api,
      lr_nhi_package TYPE REF TO if_nhi_package.

    lr_nhi_api = cl_nhi_api=>create_instance(  ).
    lr_nhi_package = lr_nhi_api->get_package(  ).

    IF hana_package_exists( iv_parent ) = rs_c_false.
      RAISE EXCEPTION TYPE cx_rs_msg.
    ENDIF.

    IF iv_parent IS INITIAL.
      DATA(lv_package) = iv_package.
    ELSE.
      lv_package = |{ iv_parent }.{ iv_package }|.
    ENDIF.

    IF hana_package_exists( lv_package ) = rs_c_false.
      TRY.
          DATA(lr_nhi_package_create) = lr_nhi_package->create(  request = lr_nhi_package->create_create_package_req(
                            tenant = ''
                            delivery_unit = ''
                            description = |{ iv_txtlg }|
                            du_vendor = ''
                            hints_for_translation = ''
                            orig_lang = |{ sy-langu }|
                            package = lv_package
                            responsible = 'SAPHANADB'
                            structural = iv_structural
                            texts = lt_texts
                            text_collection = ''
                            text_status = ''
                            text_terminology_domain = '' ) ).
          rv_package = lv_package.
        CATCH cx_nhi_hana_repository INTO DATA(lrx_nhi).
          RAISE EXCEPTION TYPE cx_rs_msg
            EXPORTING
              previous = lrx_nhi.
      ENDTRY.
    ENDIF.
    rv_package = lv_package.

  ENDMETHOD.

  METHOD infoarea_create.

    DATA:
      lr_infoarea TYPE REF TO if_rsawbn_folder_tree.

    CREATE OBJECT lr_infoarea TYPE cl_new_awb_area.

    IF infoarea_exists( iv_parent ) = rs_c_false.
      RAISE EXCEPTION TYPE cx_rs_msg.
    ENDIF.

    IF infoarea_exists(  iv_infoarea ) = rs_c_false.
      DATA:
        lv_nodename   TYPE rsawbnfoldernm,
        lv_parentnode TYPE rsawbnfoldernm.

      lv_nodename   = iv_infoarea.
      lv_parentnode = iv_parent.
      CALL METHOD lr_infoarea->create_node
        EXPORTING
          i_nodename   = lv_nodename
          i_parentname = lv_parentnode
          i_txtlg      = iv_txtlg
          i_txtsh      = |{ iv_txtlg }|
        EXCEPTIONS
          cancelled    = 8.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_rs_msg
          EXPORTING
            msgid = sy-msgid
            msgno = sy-msgno
            msgty = sy-msgty
            msgv1 = sy-msgv1
            msgv2 = sy-msgv2
            msgv3 = sy-msgv3
            msgv4 = sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
