*&---------------------------------------------------------------------*
*& Report ywstw_cross_transport
*&---------------------------------------------------------------------*
*&!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*&  MASTER SYSTEM für den REPORT = S4D !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*&!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*& Cross-Transport für Transporte
*&   - die Transporte müssen schon freigegeben sein
*&   - die Transporte (Co-+Data-File) werden in ein ZIP-File auf den
*&     Client exportiert
*&   - im Zielsystem kann das ZIP-File für den Import verwendet werden
*&   - danach muss in der STMS der Transport in die Import-Queue
*&     gestellt werden
*&---------------------------------------------------------------------*
REPORT ywstw_cross_transport.

"-----------------------------------------------------------------------------------------
" SELECTION-SCREEN
"-----------------------------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.

  PARAMETERS: p_downl RADIOBUTTON GROUP rb01 DEFAULT 'X' ##needed.

  POSITION 12.
  PARAMETERS: p_trq   TYPE e070-trkorr MATCHCODE OBJECT fpm_shlp_trkorr.

  SELECTION-SCREEN ULINE.

  PARAMETERS: p_upl   RADIOBUTTON GROUP rb01.
  PARAMETERS: p_over AS CHECKBOX DEFAULT space.

SELECTION-SCREEN END OF BLOCK b01.

"-----------------------------------------------------------------------------------------
" CLASS LCX_MAIN
"-----------------------------------------------------------------------------------------

CLASS lcx_generic DEFINITION INHERITING FROM cx_static_check FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_t100_message.

    METHODS constructor
      IMPORTING i_msg TYPE string.

    DATA: msg      TYPE string READ-ONLY ##NEEDED.

ENDCLASS.

CLASS lcx_generic IMPLEMENTATION.

  METHOD constructor.

    TYPES: BEGIN OF ty_message,
             msgv1 TYPE sy-msgv1,
             msgv2 TYPE sy-msgv2,
             msgv3 TYPE sy-msgv3,
             msgv4 TYPE sy-msgv4,
           END OF ty_message.
    DATA: message   TYPE ty_message,
          callstack TYPE abap_callstack.

    super->constructor( ).

    me->msg = i_msg.

    "Default message type
    me->if_t100_message~t100key-msgid = 'SY'.
    me->if_t100_message~t100key-msgno = '499'.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level = 2
      IMPORTING
        callstack = callstack.

    TRY.
        DATA(line_of_code) = CONV string( callstack[ 2 ]-line ).
      CATCH cx_sy_itab_line_not_found.
        line_of_code = '??'.
    ENDTRY.


    message = CONV #( |{ i_msg } (Line: { line_of_code }).| ) ##operator[ty_message].
    me->if_t100_message~t100key-attr1 = message-msgv1.
    me->if_t100_message~t100key-attr2 = message-msgv2.
    me->if_t100_message~t100key-attr3 = message-msgv3.
    me->if_t100_message~t100key-attr4 = message-msgv4.

  ENDMETHOD.

ENDCLASS.

"-----------------------------------------------------------------------------------------
" CLASS LCL_MAIN
"-----------------------------------------------------------------------------------------

CLASS lcl_main DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.

    METHODS:
      constructor,
      run IMPORTING i_request_id      TYPE e070-trkorr
                    i_mode_upload     TYPE xfeld
                    i_force_overwrite TYPE xfeld.

    CLASS-METHODS:
      get_instance IMPORTING i_singleton       TYPE abap_bool DEFAULT abap_true
                   RETURNING VALUE(r_instance) TYPE REF TO lcl_main.

  PRIVATE SECTION.
    CONSTANTS: c_dir_trans     TYPE spfl_parameter_name VALUE 'DIR_TRANS',
               c_dir_separator TYPE spfl_parameter_name VALUE 'DIR_SEP'.

    DATA: profile_dir_trans          TYPE spfl_parameter_value,
          profile_dir_separator_char TYPE spfl_parameter_value.

    CLASS-DATA:
      static_instance TYPE REF TO lcl_main.

    METHODS download_data
      IMPORTING
                i_request_id TYPE e070-trkorr
      RAISING   lcx_generic.

    METHODS upload_data
      IMPORTING
                i_force_overwrite TYPE xfeld
      RAISING   lcx_generic.

    METHODS create_file_names_from_request
      IMPORTING
        i_request_id    TYPE e070-trkorr
      EXPORTING
        e_cofile_name   TYPE string
        e_datafile_name TYPE string.

    METHODS file_exists
      IMPORTING
        i_file_name_full     TYPE string
      RETURNING
        VALUE(r_file_exists) TYPE abap_bool.

    METHODS get_binary_file
      IMPORTING
        i_filename       TYPE string
      RETURNING
        VALUE(r_xstring) TYPE xstring
      RAISING
        lcx_generic.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD constructor.

    " read PROFILE_PARAMETERS

    cl_spfl_profile_parameter=>get_value(
      EXPORTING
        name  = c_dir_trans
      IMPORTING
        value = profile_dir_trans
      RECEIVING
        rc    = DATA(rc_spfl)
    ).

    IF rc_spfl <> 0.
      FREE profile_dir_trans.
    ENDIF.

    cl_spfl_profile_parameter=>get_value(
      EXPORTING
        name  = c_dir_separator
      IMPORTING
        value = profile_dir_separator_char
      RECEIVING
        rc    = rc_spfl
    ).

    IF rc_spfl <> 0.
      FREE profile_dir_separator_char.
    ENDIF.

  ENDMETHOD.

  METHOD get_instance.
    r_instance = COND #( WHEN i_singleton = abap_true AND static_instance IS BOUND THEN static_instance ELSE NEW lcl_main( ) ).
    static_instance = r_instance.
  ENDMETHOD.

  METHOD run.

    TRY.
        IF i_mode_upload = abap_true.
          upload_data( i_force_overwrite = i_force_overwrite ).
        ELSE.
          IF strlen( CONV string( i_request_id ) ) <> 10 OR i_request_id+3(1) <> 'K'.
            RAISE EXCEPTION TYPE lcx_generic
              EXPORTING
                i_msg = |Transport '{ i_request_id }' has no valid syntax.| ##no_text.
          ENDIF.

          download_data( i_request_id = i_request_id ).
        ENDIF.

      CATCH lcx_generic INTO DATA(generic_exception).
        MESSAGE ID generic_exception->if_t100_message~t100key-msgid
                TYPE 'S'
                NUMBER generic_exception->if_t100_message~t100key-msgno
                WITH generic_exception->if_t100_message~t100key-attr1
                     generic_exception->if_t100_message~t100key-attr2
                     generic_exception->if_t100_message~t100key-attr3
                     generic_exception->if_t100_message~t100key-attr4
                DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD download_data.

    DATA: cofile_name   TYPE string,
          datafile_name TYPE string,
          path          TYPE string,
          fullpath      TYPE string.

    TRY.
        DATA(transport_entity) = cl_cts_transport_factory=>get_transport_entity(
          EXPORTING
            id = i_request_id
        ).
        IF transport_entity->get_status( ) <> if_cts_transport_request=>co_status_released.
          RAISE EXCEPTION TYPE lcx_generic
            EXPORTING
              i_msg = |Transport { i_request_id } not released.| ##no_text.
        ENDIF.

        DATA(transport_request) = CAST if_cts_transport_request( transport_entity ).
        DATA(transport_type) = transport_request->get_type(  ).

        IF     transport_type <> if_cts_transport_request=>co_req_type_copy
           AND transport_type <> if_cts_transport_request=>co_req_type_customizing
           AND transport_type <> if_cts_transport_request=>co_req_type_workbench.
          RAISE EXCEPTION TYPE lcx_generic
            EXPORTING
              i_msg = |Transport { i_request_id } is not of valid type.| ##no_text.
        ENDIF.

        transport_request->get_header_data(
          IMPORTING
            header = DATA(transport_header)
        ).

      CATCH cx_cts_transport_entity.
        RAISE EXCEPTION TYPE lcx_generic
          EXPORTING
            i_msg = |Error identifying Transport { i_request_id }.| ##no_text.
    ENDTRY.

    create_file_names_from_request( EXPORTING i_request_id    = transport_header-trkorr
                                    IMPORTING e_cofile_name   = cofile_name
                                              e_datafile_name = datafile_name ).

    DATA(zip_handler) = NEW cl_abap_zip( ).

    zip_handler->add(
      EXPORTING
        name    = datafile_name
        content = get_binary_file( |{ profile_dir_trans }{ profile_dir_separator_char }data{ profile_dir_separator_char }{ datafile_name }| ) ##no_text
    ).

    zip_handler->add(
      EXPORTING
        name    = cofile_name
        content = get_binary_file( |{ profile_dir_trans }{ profile_dir_separator_char }cofiles{ profile_dir_separator_char }{ cofile_name }| ) ##no_text
    ).

    DATA(zip_xstring) = zip_handler->save( ).

    DATA(filename) = |{ i_request_id }.ZIP| ##no_text.

    "get target
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title              = CONV #( 'Zieldatei auswählen'(001) )
        default_file_name         = filename
      CHANGING
        filename                  = filename
        path                      = path
        fullpath                  = fullpath
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
    ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_generic
        EXPORTING
          i_msg = 'File save aborted.' ##no_text.
    ENDIF.

    DATA(zip_raw_data) = cl_bcs_convert=>xstring_to_solix( EXPORTING iv_xstring = zip_xstring ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize            = xstrlen( zip_xstring )
        filename                = fullpath
        filetype                = 'BIN' ##no_text
      IMPORTING
        filelength              = DATA(filelength)
      CHANGING
        data_tab                = zip_raw_data
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
    ).

    IF sy-subrc <> 0 OR filelength = 0.
      RAISE EXCEPTION TYPE lcx_generic
        EXPORTING
          i_msg = |Cannot download file RC = { sy-subrc }.| ##no_text.
    ENDIF.

  ENDMETHOD.

  METHOD get_binary_file.

    OPEN DATASET i_filename FOR INPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_generic
        EXPORTING
          i_msg = |Can't open { i_filename } for reading.| ##no_text.
    ENDIF.

    READ DATASET i_filename INTO r_xstring.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_generic
        EXPORTING
          i_msg = |Can't read { i_filename } properly.| ##no_text.
    ENDIF.
    CLOSE DATASET i_filename.

  ENDMETHOD.


  METHOD upload_data.

    DATA: filelength       TYPE i,
          zip_raw_data     TYPE solix_tab,
          file_table       TYPE filetable,
          rc               TYPE i,
          cofile_name      TYPE string,
          datafile_name    TYPE string,
          cofile_xstring   TYPE xstring,
          datafile_xstring TYPE xstring.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = CONV #( 'Upload Transport-ZIP-File'(002) )
        default_extension       = 'zip'
        default_filename        = '*.zip' ##no_text
        multiselection          = abap_false
      CHANGING
        file_table              = file_table
        rc                      = rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
    ).
    IF sy-subrc <> 0 OR file_table IS INITIAL.
      RAISE EXCEPTION TYPE lcx_generic
        EXPORTING
          i_msg = 'File empty or upload aborted.' ##no_text.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = CONV #( file_table[ 1 ]-filename )
        filetype                = 'BIN'
      IMPORTING
        filelength              = filelength
      CHANGING
        data_tab                = zip_raw_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_generic
        EXPORTING
          i_msg = |Error uploading file RC={ sy-subrc }.| ##no_text.
    ENDIF.

    DATA(zip_xstring) = cl_bcs_convert=>solix_to_xstring( it_solix = zip_raw_data[]
                                                          iv_size  = filelength ).

    DATA(zip_handler) = NEW cl_abap_zip( ).

    zip_handler->load(
      EXPORTING
        zip             = zip_xstring
        check_header    = abap_true
      EXCEPTIONS
        zip_parse_error = 1
    ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_generic
        EXPORTING
          i_msg = |Error handling ZIP RC={ sy-subrc }.| ##no_text.
    ENDIF.

    LOOP AT zip_handler->files REFERENCE INTO DATA(single_file).

      IF single_file->name(1) = 'K'.
        datafile_name = |R{ single_file->name+1 }|.
      ELSE.
        CONTINUE.
      ENDIF.

      zip_handler->get(
        EXPORTING
          name                    = single_file->name
          index                   = 0
        IMPORTING
          content                 = cofile_xstring
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
      ).

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_generic
          EXPORTING
            i_msg = |Error unzipping cofile { single_file->name } RC={ sy-subrc }.| ##no_text.
      ENDIF.

      cofile_name = single_file->name.

      zip_handler->get(
        EXPORTING
          name                    = datafile_name
          index                   = 0
        IMPORTING
          content                 = datafile_xstring
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
      ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_generic
          EXPORTING
            i_msg = |Error unzipping datafile { datafile_name } RC={ sy-subrc }.| ##no_text.
      ENDIF.

      IF cofile_xstring IS INITIAL OR datafile_xstring IS INITIAL OR cofile_name IS INITIAL OR datafile_name IS INITIAL.
        RAISE EXCEPTION TYPE lcx_generic
          EXPORTING
            i_msg = |Error handling datafile or cofile { datafile_name } { cofile_name } RC={ sy-subrc }.| ##no_text.
      ENDIF.

      DATA(cofile_name_full) = |{  profile_dir_trans }{ profile_dir_separator_char }cofiles{ profile_dir_separator_char }{ cofile_name }| ##no_text.
      DATA(datafile_name_full) = |{  profile_dir_trans }{ profile_dir_separator_char }data{ profile_dir_separator_char }{ datafile_name }| ##no_text.

      IF i_force_overwrite = abap_false AND ( file_exists( i_file_name_full = cofile_name_full ) = abap_true OR
                                              file_exists( i_file_name_full = datafile_name_full ) ).
        RAISE EXCEPTION TYPE lcx_generic
          EXPORTING
            i_msg = |Aborted, i wont overwrite neither datfile nor cofile { cofile_name },{ datafile_name }.| ##no_text.
      ENDIF.

      OPEN DATASET cofile_name_full FOR OUTPUT IN BINARY MODE.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_generic
          EXPORTING
            i_msg = |Error writing to { cofile_name_full }.| ##no_text.
      ENDIF.
      TRANSFER cofile_xstring TO cofile_name_full.
      CLOSE DATASET cofile_name_full.

      OPEN DATASET datafile_name_full FOR OUTPUT IN BINARY MODE.
      IF sy-subrc <> 0.
        "clear cofile for consistency
        DELETE DATASET cofile_name_full.
        "#todo error handling
        RAISE EXCEPTION TYPE lcx_generic
          EXPORTING
            i_msg = |Error writing to { datafile_name_full }.| ##no_text.
      ENDIF.
      TRANSFER datafile_xstring TO datafile_name_full.
      CLOSE DATASET datafile_name_full.

      WRITE: 'Cofile created: '(003), cofile_name_full ##no_text.
      WRITE: / 'Datafile created: '(004), datafile_name_full ##no_text.
      WRITE: / |Use TX STMS / STMS_IMPORT to add to buffer ... { cofile_name+8(3) }{ cofile_name(7) }|.
      ULINE.

    ENDLOOP.

  ENDMETHOD.

  METHOD create_file_names_from_request.

    FREE: e_cofile_name, e_datafile_name.

    DATA(transport_sid) = i_request_id(3).
    DATA(transport_number) = i_request_id+4.

    e_cofile_name = |K{ transport_number }.{ transport_sid }|.
    e_datafile_name = |R{ transport_number }.{ transport_sid }|.

  ENDMETHOD.

  METHOD file_exists.

    OPEN DATASET i_file_name_full FOR INPUT IN BINARY MODE.
    IF sy-subrc = 0.
      r_file_exists = abap_true.
      CLOSE DATASET i_file_name_full.
    ELSE.
      r_file_exists = abap_false.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*-----------------------------------------------------------------------------------------
* INITIALIZATION
*-----------------------------------------------------------------------------------------

INITIALIZATION.

  p_trq = |{ sy-sysid }K......| ##no_text.

*-----------------------------------------------------------------------------------------
* start-of-selection
*-----------------------------------------------------------------------------------------

START-OF-SELECTION.

  lcl_main=>get_instance(  )->run(
    EXPORTING
      i_request_id  = p_trq
      i_mode_upload = p_upl
      i_force_overwrite = p_over
  ).
