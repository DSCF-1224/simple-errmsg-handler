# simple errmsg/iomsg handler

## objective

To handle the `ERRMSG` / `IOMSG` from the following statement

- `ERRMSG`
  - `ALLOCATE`
  - `DEALLOCATE`
- `IOMSG`
  - `BACKSPACE`
  - `CLOSE`
  - `INQUIRE`
  - `OPEN`
  - `READ`
  - `REWIND`
  - `WRITE`

## How to use

### `ERRMSG`

```fortran
program sample

    use , intrinsic :: iso_fortran_env
    implicit none

    integer, parameter :: len_array = 10

    integer, allocatable :: val_array (:)

    type(type_errmsg_handler) :: errmsg_handler

    allocate(&!
        val_array(len_len_array)     , &!
        stat   = errmsg_handler%code , &!
        errmsg = errmsg_handler%msg    &!
    )

    call errmsg_handler%validate_allocate(&!
        unit_display   = ERROR_UNIT , &!
        allowance_stop = .true.       &!
    )

    deallocate(&!
        val_array(len_len_array)     , &!
        stat   = errmsg_handler%code , &!
        errmsg = errmsg_handler%msg    &!
    )

    call errmsg_handler%validate_deallocate(&!
        unit_display   = ERROR_UNIT , &!
        allowance_stop = .true.       &!
    )

end program sample
```

### `IOMSG`

```fortran
program sample

    use , intrinsic :: iso_fortran_env
    implicit none

    integer :: read_value

    type(type_iomsg_handler) :: iomsg_handler

    read(&!
        unit   = INPUT_UNIT         , &!
        fmt    = *                  , &!
        iostat = iomsg_handler%code , &!
        iomsg  = iomsg_handler%msg    &!
    ) &!
        read_value

    call iomsg_handler%validate_read(&!
        unit_display   = ERROR_UNIT , &!
        allowance_stop = .true.       &!
    )

end program sample
```

<!-- EOF -->