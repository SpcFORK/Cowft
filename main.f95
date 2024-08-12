subroutine cft_throw_error(msg)
  implicit none
  character(*) :: msg
  print '(a,a)', 'Error: ', msg
  error stop
end subroutine cft_throw_error

subroutine cft_assert(check, message)
  implicit none
  logical :: check
  character(*) :: message
  if (.not. check) call cft_throw_error(message)
end subroutine cft_assert

! ---

logical function cft_in_char_range(input, a, z) result(harvest)
  implicit none
  character, intent(in) :: input, a, z
  harvest = input >= a .and. input <= z
end function cft_in_char_range

logical function cft_in_string_char(input) result(harvest)
  implicit none
  character, intent(in) :: input
  harvest = &
    (input == '"') .or. &
    (input == "'") .or. &
    (input == '`')
end function cft_in_string_char

logical function cft_in_string_block(input) result(harvest)
  implicit none
  character, intent(in) :: input
  logical :: cft_in_string_char

  character :: ld, rd
  integer :: length

  ! ---

  length = len(input)

  harvest = &
    cft_in_string_char(input(1:1)) &
    .and. cft_in_string_char(input(length:length))
end function cft_in_string_block

logical function cft_is_alphabetical(input) result(harvest)
  implicit none
  character, intent(in) :: input
  logical :: cft_in_char_range

  harvest = cft_in_char_range(input, 'a', 'z') .or. cft_in_char_range(input, 'A', 'Z')
end function cft_is_alphabetical

subroutine cft_trim_newlines(input, harvest)
  implicit none
  character(len=*), intent(in) :: input
  character(len=*), intent(out) :: harvest
  integer :: i, j, length

  length = len(input)

  ! Remove leading newlines
  i = 1
  do while (i <= length .and. input(i:i) == new_line('a'))
    print *, 1
    i = i + 1
  end do

  ! Remove trailing newlines
  j = length
  do while (j >= i .and. input(j:j) == new_line('a'))
    print *, 2
    j = j - 1
  end do

  ! Assign trimmed string to harvest
  if (i <= j) then
    harvest = input(i:j)
  else
    harvest = ''
  end if
end subroutine cft_trim_newlines

! ---

program cft_tran
  implicit none

  type cft_head_state
    logical :: hasFTHead ! =) ft
    logical :: headMade ! =) ft ...
  end type cft_head_state

  type cft_block_state
    character :: delim_cache
    integer :: hasMkDepth ! { ... }
  end type cft_block_state
  
  type cft_prop_state
    integer :: hasMkDepth ! : ...
  end type cft_prop_state

  type cft_fn_state
    logical :: hasMk ! =)
  end type cft_fn_state

  type cft_string_state
    character :: delim_cache
    integer :: hasMkDepth ! `"'` + "'" `"'\`'`"'`'"'`
    integer :: string_start
    logical :: token_in_string
  end type cft_string_state

  type cft_state
    type(cft_head_state) :: head_state
    type(cft_prop_state) :: prop_state
    type(cft_block_state) :: block_state
    type(cft_fn_state) :: fn_state
    type(cft_string_state) :: string_state
  end type cft_state

  type(cft_state) :: current_cft

  character(128) :: run_path
  character(len=:), allocatable :: file_content, file_output

  ! ---

  call get_command_argument(1, run_path)
  call cft_read_file_as_char(run_path, file_content)
  print '(1a)', run_path, file_content

  call cft_default_state(current_cft)
  call cft_split_and_iterate(file_content, file_output, current_cft)

  print *, file_output

contains

! ---

subroutine cft_default_head_state(cft_s)
  implicit none
  type(cft_state), intent(inout) :: cft_s

  cft_s%head_state%headMade = .false.
  cft_s%head_state%hasFTHead = .false.
end subroutine cft_default_head_state

subroutine cftstate_scrub_char_cache(cft_s_prp)
  implicit none
  class(*), intent(inout) :: cft_s_prp

  select type (cft_s_prp)
    type is (cft_block_state)
      cft_s_prp%delim_cache = ''
    type is (cft_string_state)
      cft_s_prp%delim_cache = ''
    class default
      call cft_throw_error('Unknown type in cftstate_scrub_char_cache')
  end select
end subroutine cftstate_scrub_char_cache

subroutine cftstate_scrub_mk_depth(cft_s_prp)
  implicit none
  class(*), intent(inout) :: cft_s_prp

  select type (cft_s_prp)
    type is (cft_block_state)
      cft_s_prp%hasMkDepth = 0
    type is (cft_prop_state)
      cft_s_prp%hasMkDepth = 0
    type is (cft_string_state)
      cft_s_prp%hasMkDepth = 0
    class default
      call cft_throw_error('Unknown type in cftstate_scrub_mk_depth')
  end select
end subroutine cftstate_scrub_mk_depth

subroutine cft_default_block_state(cft_s)
  implicit none
  type(cft_state), intent(inout) :: cft_s

  call cftstate_scrub_char_cache(cft_s%block_state)
  call cftstate_scrub_mk_depth(cft_s%block_state)
end subroutine cft_default_block_state

subroutine cft_default_prop_state(cft_s)
  implicit none
  type(cft_state), intent(inout) :: cft_s

  call cftstate_scrub_mk_depth(cft_s%prop_state)
end subroutine cft_default_prop_state

subroutine cft_default_fn_state(cft_s)
  implicit none
  type(cft_state), intent(inout) :: cft_s

  cft_s%fn_state%hasMk = .false.
end subroutine cft_default_fn_state

subroutine cft_default_string_state(cft_s)
  implicit none
  type(cft_state), intent(inout) :: cft_s

  cft_s%string_state%token_in_string = .false.
  cft_s%string_state%string_start = 0

  call cftstate_scrub_char_cache(cft_s%string_state)
  call cftstate_scrub_mk_depth(cft_s%string_state)
end subroutine cft_default_string_state

subroutine cft_default_state(cft_s)
  implicit none
  type(cft_state), intent(inout) :: cft_s

  call cft_default_head_state(cft_s)
  call cft_default_block_state(cft_s)
  call cft_default_prop_state(cft_s)
  call cft_default_fn_state(cft_s)
  call cft_default_string_state(cft_s)
end subroutine cft_default_state

! ---

subroutine cft_read_file_as_char(filename, content_harvest)
  implicit none
  character(len=*), intent(in) :: filename
  character(len=:), allocatable, intent(out) :: content_harvest
  character(len=1000) :: buffer
  integer :: unit, ios, length

  open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
  call cft_assert(ios == 0, 'Error opening file')

  content_harvest = ''
  do
    read(unit, '(A)', iostat=ios) buffer
    if (ios /= 0) exit
    length = len_trim(buffer)
    content_harvest = content_harvest // buffer(:length) // new_line('a')
  end do

  close(unit)
end subroutine cft_read_file_as_char

subroutine cft_split_and_iterate(text, output, cft_s)
  implicit none
  type(cft_state), intent(inout) :: cft_s
  ! Str
  character(*), intent(in) :: text
  character(len=:), allocatable, intent(out) :: output
  character(len=:), allocatable :: current_block, next_char, current_char, last_char
  ! Log
  logical :: should_split, was_split, implicit_split, &
             cft_in_string_char, cft_is_alphabetical, cft_in_char_range
  ! Int 
  integer :: i, start, text_len

  start = 1
  text_len = len(text)

  output = ''
  current_block = ''

  was_split = .false.
  should_split = .false.
  implicit_split = .false.

  do i = 1, text_len
    last_char = text(i-1:i-1)
    next_char = text(i:i)
    current_block = text(start:i-1)

    implicit_split = (last_char == new_line('a')) .or. &
                     (next_char == ' ')

    should_split = cft_is_alphabetical(next_char) .or. &
                   cft_in_char_range(next_char, '0', '9') .or. &
                   cft_in_string_char(next_char)

    !print *, 'Bi: ', '`' // next_char // '`', implicit_split, should_split, start, i, '|'

    if ( (i > 1 .and. (should_split .neqv. was_split)) .or. implicit_split .or. (i == text_len)) then
      call cft_parse_token(current_block, next_char, output, cft_s)
      start = i
    end if

    was_split = should_split
  end do
end subroutine cft_split_and_iterate

subroutine cft_parse_token(token, next_t, output_str, cft_s)
  implicit none
  type(cft_state), intent(inout) :: cft_s
  character(*), parameter :: delimiter = ';' // new_line('a')
  ! ---
  character, intent(in) :: next_t
  character(len=:), allocatable, intent(in) :: token
  character(len=:), allocatable, intent(inout) :: output_str
  ! ---
  logical :: is_string, &
             cft_in_string_block
  character(len=:), allocatable :: trimmed_token
  ! ---

  trimmed_token = trim(adjustl(token))

  is_string = cft_in_string_block(trimmed_token)

  ! Transpile
  transpile_l: select case (trimmed_token)
  case (delimiter(:1), delimiter(2:))
    ! On double delim, end declaration.
    nlnl_l: select case (next_t)
    case (delimiter(:1), delimiter(2:))
      if (cft_s%head_state%hasFTHead .and. .not. cft_s%head_state%headMade) &
        cft_s%head_state%headMade = .true.
    end select nlnl_l
  case ('=)')
    if (cft_s%fn_state%hasMk) call cft_throw_error('Invalid fn construct.')

    cft_s%fn_state%hasMk = .true.

    ! Generate Function.
    if (cft_s%head_state%headMade) &
      output_str = output_str // 'function'
  case ('ft')
    if (cft_s%head_state%headMade) &
      call cft_throw_error('Head already declared.')
    if (cft_s%fn_state%hasMk .and. cft_s%head_state%hasFTHead) &
      call cft_throw_error('Invalid Head construct.')

    cft_s%head_state%hasFTHead = .true.
    output_str = output_str // 'program'
    call cft_default_fn_state(cft_s)
  case (':')
    cft_s%prop_state%hasMkDepth = cft_s%prop_state%hasMkDepth + 1
  end select transpile_l

  ! if ()

  if (is_string) then
    if (cft_s%string_state%token_in_string) then
      call cft_default_string_state(cft_s)
    else
      cft_s%string_state%token_in_string = .true.
    end if
  end if

  print *, 'PC: ', '`', cft_s, '`'
  print '(4a)', 'P>: ', '`', token, '`'
  print '(4a)', 'P_: ', '`', trimmed_token, '`'
  print '(4a)', 'P+: ', '`', next_t, '`'
  print '(4a)', 'Po: ', '`', output_str, '`'
  print *, ''

end subroutine cft_parse_token

end program cft_tran