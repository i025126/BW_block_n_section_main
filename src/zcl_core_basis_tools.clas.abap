CLASS zcl_core_basis_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF gc_parameters,
        "! default value ROOT
        prefix_cluster  TYPE string VALUE 'PREFIX_CLUSTER',
        "! Default value MDA
        mda_section     TYPE string VALUE 'MDA_SECTION',
        "! Default value EDW
        edw_section     TYPE string VALUE 'EDW_SECTION',
        "! Default value IAP
        iap_section     TYPE string VALUE 'IAP_SECTION',
        "! Default value AAV
        aav_section     TYPE string VALUE 'AAV_SECTION',
        "! Default value EXZ
        exz_section     TYPE string VALUE 'EXZ_SECTION',
        "! Default value SYS
        sys_section     TYPE string VALUE 'SYS_SECTION',
        "! Defailt value ALL
        all_postfix     TYPE string VALUE 'ALL_POSTFIX',
        "! Auth default block AUTH
        auth_block      TYPE string VALUE 'AUTH_BLOCK',
        "! Prefix for role default BW4
        prefix_role     TYPE string VALUE 'PREFIX_ROLE',
        "! P,lace to keep the cluster definitions SYSX_ALL
        infoareacluster TYPE string VALUE 'INFOAREA_CLUSTER',
        "! Block for authorization AUTH
        auth_root       TYPE string VALUE 'AUTH_ROOT',
      END OF gc_parameters.

    CLASS-DATA:
      gt_template type co2_string_tb,
      gs_setup TYPE zcore_setup.

    CLASS-METHODS:
      "! Returns the configuration value as per the given parameter
      "! The parameters can have the values as found in the constant
      "! gc_parameters
      "! @parameter iv_constant |What constant do you need
      "! @parameter rv_value    |value of constant
      get_c
        IMPORTING
          iv_constant     TYPE string
        RETURNING
          VALUE(rv_value) TYPE char30,
      "! The method will create a role that allows a bearer to create content in the package and
      "! below. The role is either the xALL or a block role - Forthe xALL the package is the root
      "! level ROOTx and for the block it's the ROOTx.[Section].[Block] the sections is created if
      "! they exists in BW as an InfoArea. So if you need to do only mixed models in f.x. IAP
      "! you need to create an block in BW in IAP
      "! @parameter iv_rolename | Rolename of the cluster or block
      hana_role_create
        IMPORTING
          iv_rolename TYPE agr_name
        RAISING
          cx_rs_msg
          cx_rs_not_found
          cx_nhi_adt_error,
      "! Delete a role in HANA
      "! @parameter iv_rolename | Rolename of the cluster or block
      hana_role_delete
        IMPORTING
          iv_rolename TYPE agr_name
        RAISING
          cx_rs_msg
          cx_nhi_adt_error,
      "! Return the content of the role as a table - if it exists
      "! @parameter iv_rolename | Rolename of the cluster or block
      hana_role_read
        IMPORTING
          iv_rolename       TYPE agr_name
        RETURNING
          VALUE(rt_content) TYPE co2_string_tb
        RAISING
          cx_rs2hana_view_nhi,
      "! Create a repository package as a sub package below the package given in parent. if the
      "! parent is blank or not provided the package is placed in the root.
      "! @parameter iv_parent      | Name of parent package
      "! @parameter iv_package     | Name of package to be placed below parent
      "! @parameter iv_structural  | If it's a structural package, no content can be placed in it
      "! @parameter iv_txtlg       | Text for description of the new package
      "! @parameter rv_package     | The fully qualified path of the new package
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
      "! Creates infoarea is BW, intended to create the basis structure for a new cluster
      "! @parameter iv_parent    | InfoArea as parent
      "! @parameter iv_infoarea  | Name of Infoarea to be placed below parent
      "! @parameter iv_txtlg     | Description of new InfoArea
      infoarea_create
        IMPORTING
          iv_parent   TYPE rsinfoarea
          iv_infoarea TYPE rsinfoarea
          iv_txtlg    TYPE rstxtlg
        RAISING
          cx_rs_msg,
      class_constructor.

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
      _cluster_section_block_as_area
        IMPORTING
          iv_cluster       TYPE zcore_cluster  OPTIONAL
          iv_section       TYPE zcore_section  OPTIONAL
          iv_block         TYPE zcore_block    OPTIONAL
        RETURNING
          VALUE(rv_exists) TYPE rs_bool.

ENDCLASS.


CLASS zcl_core_basis_tools IMPLEMENTATION.

  METHOD class_constructor.
   append:
     |role AUTH::BW4AUTHCTEMPLATE { '{' }| to gt_template,
     |-- Privileges needed for anyone who wants to do data modeling in packages of the block| to gt_template,
     |-- This role contains everything required for modeling| to gt_template,
     || to gt_template,
     |-- System Privileges:| to gt_template,
     |-- Allow the user to verify that the tables underlying the Information Models do exist.| to gt_template,
     |-- We simply grant CATALOG READ, so the user has no SELECT Privilege on the underlying database tables.| to gt_template,
     |-- This will allow reading _all_ metadata in the system, but no table contents !| to gt_template,
     |  system privilege: CATALOG READ; | to gt_template,
     || to gt_template,
     |-- Here we allow the role to read the repository tree (expand the "content" tree). This does not allow seeing the content of packages:| to gt_template,
     |  catalog sql object "SYS"."REPOSITORY_REST": EXECUTE; | to gt_template,
     || to gt_template,
     |-- We also enable activation of time-based Attribute Views as these are based on a standard table in schema _SYS_BI| to gt_template,
     |-- for simplicity, we include the entire _SYS_BI schema which does not contain sensitive data | to gt_template,
     |  catalog schema "_SYS_BI": SELECT;| to gt_template,
     |-- Strictly speaking, no SELECT or EXECUTE privileges are required in| to gt_template,
     |-- order to build and activate data models, as long as the modeler has access to the catalog metadata (system privilege CATALOG READ).| to gt_template,
     |-- It is, however, easier to perform data modeling tasks if the modelers have SELECT access to the database schemas underlying the data models.| to gt_template,
     |-- Change to whatever schema you need | to gt_template,
     |catalog schema "_SYS_BIC": SELECT;  | to gt_template,
     |-- Read access to the generate external HANA View| to gt_template,
     |  package system-local: REPO.READ;  | to gt_template,
     || to gt_template,
     |-- repository access --| to gt_template,
     |-- The below line will prevent the role from being activated, the program reading it will look for the // and replace the CLUSTER.SECTION.BLOCK| to gt_template,
     |//  package CLUSTER.SECTION.BLOCK: REPO.READ, REPO.EDIT_NATIVE_OBJECTS, REPO.ACTIVATE_NATIVE_OBJECTS, REPO.MAINTAIN_NATIVE_PACKAGES, REPO.ACTIVATE_IMPORTED_OBJECTS, REPO.MAINTAIN_IMPORTED_PACKAGES;| to gt_template,
     |{ '}' }| to gt_template.
  ENDMETHOD.

  METHOD hana_role_create.

    DATA(lv_block)     = zcl_core_role_admin=>get_block_from_rolename( iv_rolename ).
    DATA(lv_roletype)  = zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ).
    DATA(lv_cluster)   = zcl_core_role_admin=>get_cluster_from_rolename( iv_rolename ).

    CALL METHOD zcl_core_role_admin=>static_do_message( iv_message = |HANA Role { iv_rolename } must be created| iv_detlevel = 1 ).

    DATA(lv_root)      = |{ get_c( gc_parameters-prefix_cluster ) }{ lv_cluster }|.

    "" Make sure the root package have been created
    "" This is to create the (make sure it's there) the AUTH structural package
    DATA(lv_txtlg)    = |Roles for Mixed models|.
    DATA(lv_auth_package)  = hana_package_create(  iv_parent = || iv_package = |{ get_c( gc_parameters-auth_root ) }| iv_txtlg = lv_txtlg iv_structural = rs_c_false ).

    "" create the AUTH.ROOTx as all role will be in this one... not a structural
    "" AUTH.ROOTx
    lv_txtlg   = |Roles for Mixed models - Cluster { zcl_core_role_admin=>get_cluster_description( lv_cluster ) } |.
    lv_auth_package = hana_package_create(  iv_parent = lv_auth_package iv_package = |{ get_c( gc_parameters-prefix_cluster ) }{ lv_cluster }| iv_txtlg = lv_txtlg iv_structural = rs_c_false ).

    TRY.
        DATA(lt_template_content) = hana_role_read( iv_rolename = zcl_core_role_admin=>get_template_from_roletype( zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ) ) ).
        "" Technically this can also be a problem with the nhi - but i would expect that the cx_rs2hana is thrown
        if lt_template_content IS NOT INITIAL.
          " This is a default kind of template, that can be overwritten
          lt_template_content = gt_template.
        endif.
      CATCH cx_rs2hana_view_nhi INTO DATA(lrx_nhi_not_found).
        RAISE EXCEPTION TYPE cx_rs_not_found
          EXPORTING
            previous = lrx_nhi_not_found
            object   = |Template HANA Role not found|
            key      = |{ iv_rolename }|.
    ENDTRY.
    "" Prepare the role... there is only one role per block and this will span all blocks in across the sections
    "" Since we get a notification when a new block is created we can update the role as we go, given the bearer access to new sections of the block
    DATA lt_role_content TYPE co2_string_tb.

    SELECT doccluster, docsection, docblock
        FROM zi_core_contentview
        WHERE docblock = @lv_block
        ORDER BY doccluster, docsection, docblock
        INTO TABLE @DATA(lt_section).

    " Make sure that repository packages that is referenced in the role is also available in the repository
    " It looks worse then it is - We just need to make sure all the packages are in
    LOOP AT lt_section INTO DATA(ls_cluster) GROUP BY ls_cluster-doccluster.
      " Create the ROOTx if not available
      DATA(lv_root_package) = hana_package_create(  iv_parent = ''
                                               iv_package = |{ get_c( gc_parameters-prefix_cluster ) }{ ls_cluster-doccluster }|
                                               iv_structural = rs_c_true
                                               iv_txtlg = |Cluster { ls_cluster-doccluster }{ zcl_core_role_admin=>get_cluster_description( ls_cluster-doccluster ) } | ).
      LOOP AT GROUP ls_cluster INTO DATA(ls_section) GROUP BY ls_section-docsection.
        " Create the section package if not available
        data(lv_sec_package) = hana_package_create( iv_parent = lv_root_package
                                          iv_package = |{ ls_section-docsection }|
                                          iv_structural = rs_c_true
                                          iv_txtlg = |Cluster { ls_cluster-doccluster }{ zcl_core_role_admin=>get_cluster_description( ls_cluster-doccluster ) } Section { ls_section-docsection } | ).
        LOOP AT GROUP ls_section INTO DATA(ls_block).
          " Create the development package below the section for the block
          CALL METHOD hana_package_create
            EXPORTING
              iv_parent     = lv_sec_package
              iv_package    = |{ ls_block-docblock }|
              iv_structural = rs_c_false
              iv_txtlg      = ||.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    "" Create the role from template
    LOOP AT lt_template_content ASSIGNING FIELD-SYMBOL(<lv_role_content>).
      APPEND INITIAL LINE TO lt_role_content ASSIGNING FIELD-SYMBOL(<lv_new_content>).
      <lv_new_content> = <lv_role_content>.

      AT FIRST.
        "" role AUTH::BW4AUTHCTEMPLATE {
        <lv_new_content> = |role { lv_auth_package }::{ iv_rolename }{ ' { ' }|.
      ENDAT.
      if strlen( <lv_role_content> ) >= 2 and <lv_role_content>(2) = '//'.
        SHIFT <lv_new_content> LEFT BY 2 PLACES.
        IF lv_block = zcl_core_role_admin=>get_virtual_block( iv_blocktype = zcl_core_role_admin=>gc_prefix-cluster_fix iv_cluster = lv_cluster ).
          " This is a cluster role and we can grant access at cluster level
          REPLACE 'CLUSTER.SECTION.BLOCK' IN <lv_new_content> WITH lv_root IN CHARACTER MODE.
        ELSE.
          LOOP AT lt_section INTO ls_section.
            REPLACE 'CLUSTER' IN <lv_new_content> WITH lv_root               IN CHARACTER MODE.
            REPLACE 'SECTION' IN <lv_new_content> WITH ls_section-docsection IN CHARACTER MODE.
            REPLACE 'BLOCK'   IN <lv_new_content> WITH ls_section-docblock   IN CHARACTER MODE.

            at last.
              exit.
            endat.
            " Still more to come, let us add a line for this
            append INITIAL LINE TO lt_role_content ASSIGNING <lv_new_content>.
            " and fill it from the source
            <lv_new_content> = <lv_role_content>+2.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DATA lv_role_source TYPE string.
    CONCATENATE LINES OF lt_role_content INTO lv_role_source RESPECTING BLANKS IN CHARACTER MODE SEPARATED BY cl_abap_char_utilities=>cr_lf.


    DATA: lr_wrapper TYPE REF TO zcl_core_nhi_wrapper.

    " We deal with objects in one package AUTH.ROOTA
    CREATE OBJECT lr_wrapper
      EXPORTING
        iv_package = lv_auth_package.

    TRY.
        DATA:
          lt_rsmsg           TYPE rs_t_msg,
          lt_msg             TYPE rs2hana_view_t_msg,
          lts_affected_xrefs TYPE zcl_core_nhi_wrapper=>gtyts_xrefs.
        CALL METHOD lr_wrapper->deploy_nhi_obj
          EXPORTING
            iv_content         = lv_role_source
            ir_nhi_obj_id      = lr_wrapper->get_nhi_obj_id(
                                      iv_id             = CONV #( iv_rolename )
                                      iv_active_version = rs_c_true
                                      ir_type           = ce_nhi_object_type=>type_hdb_role )
            iv_detlevel        = '1'
            iv_force           = rs_c_false
          IMPORTING
            et_rsmsg           = lt_rsmsg
            et_msg             = lt_msg
            ets_affected_xrefs = lts_affected_xrefs.

        DATA _message TYPE string.
        LOOP AT lt_rsmsg INTO DATA(ls_msg).
          MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO _message.
          CALL METHOD zcl_core_role_admin=>static_do_message.
        ENDLOOP.
      CATCH cx_rs2hana_view_nhi INTO DATA(lrx_nhi).
        CALL METHOD zcl_core_role_admin=>static_do_message( lrx_nhi->get_text( ) ).
    ENDTRY.
    CALL METHOD zcl_core_role_admin=>static_set_detlevel( -1 ).

  ENDMETHOD.

  METHOD hana_role_delete.
    DATA: lr_wrapper TYPE REF TO zcl_core_nhi_wrapper.

    " We deal with objects in one package AUTH.ROOTA
    DATA(lv_block)     = zcl_core_role_admin=>get_block_from_rolename( iv_rolename ).
    DATA(lv_roletype)  = zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ).
    DATA(lv_cluster)   = zcl_core_role_admin=>get_cluster_from_rolename( iv_rolename ).

    " Make sure the root package have been created
    DATA(lv_txtlg)    = |Roles for Mixed models|.

    "" This is to create the (make sure it's there) the AUTH structural package
    DATA(lv_auth_package)  = hana_package_create(  iv_parent = || iv_package = |{ get_c( gc_parameters-auth_root ) }| iv_txtlg = lv_txtlg iv_structural = rs_c_true ).

    "" create the AUTH.ROOTx as all role will be in this one... not a structural
    lv_txtlg   = |Roles for Mixed models - Cluster { zcl_core_role_admin=>get_cluster_description( lv_cluster ) } |.
    lv_auth_package = hana_package_create(  iv_parent = lv_auth_package iv_package = |{ get_c( gc_parameters-prefix_cluster ) }{ lv_cluster }| iv_txtlg = lv_txtlg iv_structural = rs_c_false ).

    CREATE OBJECT lr_wrapper
      EXPORTING
        iv_package = lv_auth_package.

    DATA:
      lt_rsmsg           TYPE rs_t_msg,
      lt_msg             TYPE rs2hana_view_t_msg,
      lts_affected_xrefs TYPE zcl_core_nhi_wrapper=>gtyts_xrefs.
    TRY.
        CALL METHOD lr_wrapper->delete_nhi_obj
          EXPORTING
            iv_force           = rs_c_false
            ir_nhi_obj_id      = lr_wrapper->get_nhi_obj_id(
                                      iv_id             = CONV #( iv_rolename )
                                      iv_active_version = rs_c_false
                                      ir_type           = ce_nhi_object_type=>type_hdb_role )
          IMPORTING
            ets_affected_xrefs = lts_affected_xrefs.
        CALL METHOD lr_wrapper->delete_nhi_obj
          EXPORTING
            iv_force           = rs_c_false
            ir_nhi_obj_id      = lr_wrapper->get_nhi_obj_id(
                                      iv_id             = CONV #( iv_rolename )
                                      iv_active_version = rs_c_true
                                      ir_type           = ce_nhi_object_type=>type_hdb_role )
          IMPORTING
            ets_affected_xrefs = lts_affected_xrefs.
      CATCH cx_rs2hana_view_nhi INTO DATA(lrx_nhi).
    ENDTRY.
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

    DATA lr_wrapper TYPE REF TO zcl_core_nhi_wrapper.

    " did they ask for a template?
    IF zcl_core_role_admin=>get_template_from_roletype( zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ) ) = iv_rolename.
      " They did... the thing is that the template is not placed in the same package as the rest
      " Sitting directly in AUTH
      DATA(lv_package) = |{ zcl_core_basis_tools=>get_c( zcl_core_basis_tools=>gc_parameters-auth_root ) }|.
      CALL METHOD zcl_core_role_admin=>static_do_message( |Reading HANA template role { iv_rolename }| ).
    ELSE.
      " Sitting in AUTH.ROOTx
      lv_package = |{ zcl_core_basis_tools=>get_c( zcl_core_basis_tools=>gc_parameters-auth_root ) }.{ zcl_core_basis_tools=>get_c( zcl_core_basis_tools=>gc_parameters-prefix_cluster ) }{ zcl_core_role_admin=>get_cluster_from_rolename( iv_rolename ) }|.
      CALL METHOD zcl_core_role_admin=>static_do_message( |Reading HANA role { iv_rolename }| ).
    ENDIF.

    CREATE OBJECT lr_wrapper
      EXPORTING
        iv_package = lv_package.


    rt_content = lr_wrapper->read_nhi_obj( lr_wrapper->get_nhi_obj_id(
                                  iv_active_version = rs_c_true
                                  iv_id             = |{ iv_rolename }|
                                  ir_type           = ce_nhi_object_type=>type_hdb_role ) ).

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
      CALL METHOD zcl_core_role_admin=>static_do_message( |Repository package { lv_package } must be created| ).
      TRY.
          DATA(lr_nhi_package_create) = lr_nhi_package->create(  request = lr_nhi_package->create_create_package_req(
                            tenant = ''
                            delivery_unit = ''
                            description = |{ iv_txtlg }|
                            du_vendor = ''
                            hints_for_translation = ''
                            orig_lang = |en_US|
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
    ELSE.
      CALL METHOD zcl_core_role_admin=>static_do_message( |Repository package { lv_package } must be Updated| ).
      TRY.
          DATA(lr_nhi_package_update) = lr_nhi_package->update( request = lr_nhi_package->create_update_package_req(
                            tenant = ''
                            delivery_unit = ''
                            description = |{ iv_txtlg }|
                            du_vendor = ''
                            hints_for_translation = ''
                            orig_lang = |en_US|
                            package = lv_package
                            responsible = 'SAPHANADB'
                            structural = iv_structural
                            texts = lt_texts
                            text_collection = ''
                            text_status = ''
                            text_terminology_domain = '' ) ).
          rv_package = lv_package.
        CATCH cx_nhi_hana_repository INTO lrx_nhi.
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
      CALL METHOD zcl_core_role_admin=>static_do_message( |InfoArea { iv_infoarea } must be created below parent { iv_parent }| ).
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

