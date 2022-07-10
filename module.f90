module simple_errmsg_handler

    ! required MODULE(s)
    use , intrinsic :: iso_c_binding
    use , intrinsic :: iso_fortran_env



    ! require all variables to be explicitly declared
    implicit none



    ! default accessibility of the items in this MODULE
    private

    ! exported items in this MODULE
    public :: is_NG
    public :: is_OK
    public :: type_errmsg_handler



    ! constant(s) definition for this MODULE
    integer , parameter :: CODE_OK =   0
    integer , parameter :: LEN_MSG = 256



    ! constant(s) definition for this MODULE
    enum, bind(c)
        enumerator :: MODE_ERRMSG
        enumerator :: MODE_IOMSG
    end enum



    ! TYPE defined in this MODULE
    type , abstract :: type_msg_handler

        ! field(s) of this TYPE
        integer, public :: code

        ! field(s) of this TYPE
        character( len= LEN_MSG ), public :: msg

        ! bounded PROCEDURE declaration
        contains

        procedure , pass , private :: display_core
        procedure , pass , private :: validate

    end type type_msg_handler



    ! TYPE defined in this MODULE
    type , extends(type_msg_handler) :: type_errmsg_handler

        ! additional field(s) of this TYPE
        ! NONE

        ! additional bounded PROCEDURE declaration
        contains

        procedure , pass , private :: display_errmsg
        procedure , pass , private :: validate_errmsg
        procedure , pass , public  :: validate_allocate
        procedure , pass , public  :: validate_deallocate
        generic   ,        public  :: display_msg         => display_errmsg

    end type type_errmsg_handler



    ! TYPE defined in this MODULE
    type , extends(type_msg_handler) :: type_iomsg_handler

        ! additional field(s) of this TYPE
        ! NONE

        ! additional bounded PROCEDURE declaration
        contains

        procedure , pass , private :: display_iomsg
        procedure , pass , private :: validate_iomsg
        procedure , pass , public  :: validate_backspace
        procedure , pass , public  :: validate_close
        procedure , pass , public  :: validate_inquire
        procedure , pass , public  :: validate_open
        procedure , pass , public  :: validate_read
        procedure , pass , public  :: validate_rewind
        procedure , pass , public  :: validate_write
        generic   ,        public  :: display_msg     => display_iomsg

    end type type_iomsg_handler



    ! INTERFACE definition
    interface is_NG
        module procedure :: is_NG_handler
    end interface is_NG



    ! INTERFACE definition
    interface is_OK
        module procedure :: is_OK_handler
    end interface is_OK



    contains



    pure elemental function is_NG_handler (handler)

        ! bounded argument for this FUNCTION
        class(type_msg_handler) , intent(in) :: handler

        ! return value of this FUNCTION
        logical :: is_NG_handler

        is_NG_handler = .not. is_OK(handler)
        return

    end function is_NG_handler



    pure elemental function is_OK_handler (handler)

        ! bounded argument for this FUNCTION
        class(type_msg_handler) , intent(in) :: handler

        ! return value of this FUNCTION
        logical :: is_OK_handler

        is_OK_handler = (handler%code .eq. CODE_OK)
        return

    end function is_OK_handler



    subroutine display_core ( handler, mode, unit_display, statement )

        ! bounded argument for this SUBROUTINE
        class(type_msg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer              , intent(in) :: mode
        integer              , intent(in) :: unit_display
        character ( len= * ) , intent(in) :: statement

        write( unit= unit_display , fmt= '(A9,1X,":",1X,A)'  ) 'STATEMENT' , statement

        select case (mode)

            case(MODE_ERRMSG)

                write( unit= unit_display , fmt= '(A9,1X,":",1X,I0)' ) 'STAT'   , handler%code
                write( unit= unit_display , fmt= '(A9,1X,":",1X,A)'  ) 'ERRMSG' , trim(handler%msg)

            case(MODE_IOMSG)

                write( unit= unit_display , fmt= '(A9,1X,":",1X,I0)' ) 'IOSTAT' , handler%code
                write( unit= unit_display , fmt= '(A9,1X,":",1X,A)'  ) 'IOMSG'  , trim(handler%msg)

        end select

        return

    end subroutine display_core



    subroutine display_errmsg ( handler, unit_display, statement )

        ! bounded argument for this SUBROUTINE
        class(type_errmsg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer              , intent(in) :: unit_display
        character ( len= * ) , intent(in) :: statement

        call handler%display_core(&!
            mode         = MODE_ERRMSG  , &!
            unit_display = unit_display , &!
            statement    = statement      &!
        )

        return

    end subroutine display_errmsg



    subroutine display_iomsg ( handler, unit_display, statement )

        ! bounded argument for this SUBROUTINE
        class(type_iomsg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer              , intent(in) :: unit_display
        character ( len= * ) , intent(in) :: statement

        call handler%display_core(&!
            mode         = MODE_IOMSG   , &!
            unit_display = unit_display , &!
            statement    = statement      &!
        )

        return

    end subroutine display_iomsg



    subroutine validate ( handler, mode, unit_display, statement, allowance_stop )

        ! bounded argument for this SUBROUTINE
        class(type_msg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer              , intent(in) :: mode
        integer              , intent(in) :: unit_display
        character ( len= * ) , intent(in) :: statement
        logical              , intent(in) :: allowance_stop

        ! STEP.01
        ! validate the stored STAT/IOSTAT code
        if ( is_OK(handler) ) return

        ! STEP.02
        ! display the ERRMSG/IOMSG
        call handler%display_core( mode, unit_display, statement )

        ! STEP.03
        ! stop the execution if it was allowed
        if ( .not. allowance_stop ) return

        select case (mode)
            case ( MODE_ERRMSG ) ; stop 'by ERRMSG handler'
            case ( MODE_IOMSG  ) ; stop 'by IOMSG handler'
        end select

    end subroutine validate



    subroutine validate_allocate ( handler, unit_display, allowance_stop )

        ! bounded argument for this SUBROUTINE
        class(type_errmsg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer , intent(in) :: unit_display
        logical , intent(in) :: allowance_stop

        call handler%validate_errmsg ( &!
            unit_display   , &!
            'ALLOCATE'     , &!
            allowance_stop   &!
        )

        return

    end subroutine validate_allocate



    subroutine validate_backspace ( handler, unit_display, allowance_stop )

        ! bounded argument for this SUBROUTINE
        class(type_iomsg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer , intent(in) :: unit_display
        logical , intent(in) :: allowance_stop

        call handler%validate_iomsg ( &!
            unit_display   , &!
            'BACKSPACE'    , &!
            allowance_stop   &!
        )

        return

    end subroutine validate_backspace



    subroutine validate_close ( handler, unit_display, allowance_stop )

        ! bounded argument for this SUBROUTINE
        class(type_iomsg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer , intent(in) :: unit_display
        logical , intent(in) :: allowance_stop

        call handler%validate_iomsg ( &!
            unit_display   , &!
            'CLOSE'        , &!
            allowance_stop   &!
        )

        return

    end subroutine validate_close



    subroutine validate_deallocate ( handler, unit_display, allowance_stop )

        ! bounded argument for this SUBROUTINE
        class(type_errmsg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer , intent(in) :: unit_display
        logical , intent(in) :: allowance_stop

        call handler%validate_errmsg ( &!
            unit_display   , &!
            'DEALLOCATE'   , &!
            allowance_stop   &!
        )

        return

    end subroutine validate_deallocate



    subroutine validate_errmsg ( handler, unit_display, statement, allowance_stop )

        ! bounded argument for this SUBROUTINE
        class(type_errmsg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer              , intent(in) :: unit_display
        character ( len= * ) , intent(in) :: statement
        logical              , intent(in) :: allowance_stop

        call handler%validate( &!
            mode           = MODE_ERRMSG    , &!
            unit_display   = unit_display   , &!
            statement      = statement      , &!
            allowance_stop = allowance_stop   &!
        )

        return

    end subroutine validate_errmsg



    subroutine validate_inquire ( handler, unit_display, allowance_stop )

        ! bounded argument for this SUBROUTINE
        class(type_iomsg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer , intent(in) :: unit_display
        logical , intent(in) :: allowance_stop

        call handler%validate_iomsg ( &!
            unit_display   , &!
            'INQUIRE'      , &!
            allowance_stop   &!
        )

        return

    end subroutine validate_inquire



    subroutine validate_iomsg ( handler, unit_display, statement, allowance_stop )

        ! bounded argument for this SUBROUTINE
        class(type_iomsg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer              , intent(in) :: unit_display
        character ( len= * ) , intent(in) :: statement
        logical              , intent(in) :: allowance_stop

        call handler%validate( &!
            mode           = MODE_IOMSG     , &!
            unit_display   = unit_display   , &!
            statement      = statement      , &!
            allowance_stop = allowance_stop   &!
        )

        return

    end subroutine validate_iomsg



    subroutine validate_open ( handler, unit_display, allowance_stop )

        ! bounded argument for this SUBROUTINE
        class(type_iomsg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer , intent(in) :: unit_display
        logical , intent(in) :: allowance_stop

        call handler%validate_iomsg ( &!
            unit_display   , &!
            'OPEN'         , &!
            allowance_stop   &!
        )

        return

    end subroutine validate_open



    subroutine validate_read ( handler, unit_display, allowance_stop )

        ! bounded argument for this SUBROUTINE
        class(type_iomsg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer , intent(in) :: unit_display
        logical , intent(in) :: allowance_stop

        call handler%validate_iomsg ( &!
            unit_display   , &!
            'READ'         , &!
            allowance_stop   &!
        )

        return

    end subroutine validate_read



    subroutine validate_rewind ( handler, unit_display, allowance_stop )

        ! bounded argument for this SUBROUTINE
        class(type_iomsg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer , intent(in) :: unit_display
        logical , intent(in) :: allowance_stop

        call handler%validate_iomsg ( &!
            unit_display   , &!
            'REWIND'       , &!
            allowance_stop   &!
        )

        return

    end subroutine validate_rewind



    subroutine validate_write ( handler, unit_display, allowance_stop )

        ! bounded argument for this SUBROUTINE
        class(type_iomsg_handler) , intent(in) :: handler

        ! unbounded argument(s) for this SUBROUTINE
        integer , intent(in) :: unit_display
        logical , intent(in) :: allowance_stop

        call handler%validate_iomsg ( &!
            unit_display   , &!
            'WRITE'        , &!
            allowance_stop   &!
        )

        return

    end subroutine validate_write

end module simple_errmsg_handler
