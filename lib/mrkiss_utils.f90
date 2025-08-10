! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_utils.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Utilities for MRKISS.@EOL
!! @std       F2023
!! @see       https://github.com/richmit/MRKISS/
!! @copyright 
!!  @parblock
!!  Copyright (c) 2025, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
!!  
!!  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
!!  conditions are met:
!!  
!!  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following
!!     disclaimer.
!!  
!!  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following
!!     disclaimer in the documentation and/or other materials provided with the distribution.
!!  
!!  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products
!!     derived from this software without specific prior written permission.
!!  
!!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
!!  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
!!  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
!!  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
!!  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
!!  OF THE POSSIBILITY OF SUCH DAMAGE.
!!  @endparblock
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
!> Utilities.
!!
module mrkiss_utils
  implicit none
  private

  public :: print_solution, print_istats, analyze_solution
  public :: seq
  public :: status_to_origin, status_to_message

contains
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @name I/O

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Output an RK solution matrix.
  !! 
  !! Inappropriate width_o, digits_o, and separator_o values may cause a runtime error.
  !!
  !! - Example 1: The default values will produce CSV output
  !!   print_solution(solution)
  !! - Example 2: For columnar output we just need to add a width
  !!   print_solution(solution, width_o=30)
  !! 
  !! @verbatim
  !! status ......... Exit status
  !!                   - -inf-0 ..... Everything worked
  !!                   - 1152-1183 .. Error in this routine
  !!                                   - 1152 .. Could not open file for write
  !!                                   - 1153 .. Could not close file         
  !!                   - others ..... No other values allowed
  !! solution ....... Matrix with solution values
  !!                   t ... row 1
  !!                   y ... rows 2:(2+y_dim)
  !!                   dy .. rows (2+y_dim+1):(2+2*y_dim+1)
  !! filename_o ..... Filename to which we print.  Default: NONE
  !!                  If not present, then output will be to output_unit (STDOUT).  
  !! digits_o ....... Number of digits for floating point numbers.  Default: 15
  !! width_o ........ Width of print format for all entities. Default: 0
  !! start_o ........ Starting index to print in solution. Default: 1
  !! end_o .......... Ending index to print in solution.  Default: Number of columns in solution.
  !! step_o ......... Print only every step_o'th value in solution. Default: 1
  !! prt_titles_o ... Print titles if .true.  Default: .true.
  !! separator_o .... String to place between fields.  Default: ',' if width_o missing, and ' ' otherwise.
  !! t_min_o ........ Print only solutions with time values >= t_min_o
  !! t_max_o ........ Print only solutions with time values <= t_min_o
  !! tag_o .......... If non-negative, this integer that will become the first column of the output. Default: -1
  !! append_o ....... Append to file instead of overwriting.  Ignored if filename_o not present. Default: .false.
  !! @endverbatim
  !! @see mrkiss_utils::status_to_message() 
  !! 
  subroutine print_solution(status, solution, filename_o, separator_o, digits_o, width_o, start_o, end_o, step_o, prt_titles_o, &
                            t_min_o, t_max_o, tag_o, append_o) 
    use, intrinsic :: iso_fortran_env, only: output_unit
    use            :: mrkiss_config,   only: rk
    implicit none
    ! Arguments
    integer,          intent(out)          :: status
    real(kind=rk),    intent(in)           :: solution(:,:)
    character(len=*), intent(in), optional :: filename_o, separator_o
    integer,          intent(in), optional :: digits_o, width_o, start_o, end_o, step_o
    real(kind=rk),    intent(in), optional :: t_min_o, t_max_o
    integer,          intent(in), optional :: tag_o
    logical,          intent(in), optional :: prt_titles_o, append_o
    ! Local variables
    integer                                :: digits, width, start_idx, end_idx, step, y_dim, sol_y_idx, tag
    logical                                :: prt_titles
    integer                                :: i, num_int, num_real
    integer                                :: out_io_stat, out_io_unit 
    character(len=:), allocatable          :: fmt, separator, access_mode
    character(len=512)                     :: digits_str, width_str, tmp_str
    ! Process arguments
    access_mode = 'SEQUENTIAL'
    if (present(append_o)) then
       if (append_o) then
          access_mode = 'APPEND'
       end if
    end if
    tag = -1
    if (present(tag_o)) tag = tag_o
    prt_titles = .true.
    if (present(prt_titles_o)) prt_titles = prt_titles_o
    step = 1
    if (present(step_o)) step = step_o
    digits = 15
    if (present(digits_o)) digits = digits_o
    width = 0
    if (present(width_o)) width = width_o
    start_idx = 1
    if (present(start_o)) start_idx = start_o
    end_idx = size(solution, 2)
    if (present(end_o)) end_idx = min(end_o, size(solution, 2))
    if (present(separator_o)) then
       separator = separator_o
    else
       if (present(width_o)) then
          separator = " "
       else
          separator = ","
       end if
    end if
    ! Compute solution meta-data
    y_dim = (size(solution, 1) - 1) / 2
    sol_y_idx = 2
    ! Create string from for format bits
    write (digits_str, '(i0)') digits
    write (width_str,  '(i0)') width
    ! Figure out how many items per line
    num_real = size(solution, 1)
    num_int = 0
    if (tag<0) then
       num_int = 1
    else
       num_int = 2
    end if
    ! Open file
    if (present(filename_o)) then
       open(newunit=out_io_unit, file=filename_o, form='formatted', action='write', access=access_mode, iostat=out_io_stat)
       if (out_io_stat /= 0) then
          status = 1152
          return
       end if
    else
       out_io_unit = output_unit
    end if
    ! Print titles
    if (prt_titles) then
       if (present(width_o)) then
          fmt='(a' // trim(width_str) // ')'
       else
          fmt='(a)'
       end if
       if (tag>=0)   then
          write(out_io_unit, fmt=fmt, advance="no") "tag"
          write(out_io_unit, fmt='(a)', advance="no") separator
       end if
       write(out_io_unit, fmt=fmt, advance="no") "i"
       write(out_io_unit, fmt='(a)', advance="no") separator
       write(out_io_unit, fmt=fmt, advance="no") "t" 
       do i=1,y_dim
          write(out_io_unit, fmt='(a)', advance="no") separator
          write(tmp_str, fmt='("y",i0)') i
          write(out_io_unit, fmt=fmt, advance="no") trim(tmp_str)
       end do
       do i=1,y_dim
          write(out_io_unit, fmt='(a)', advance="no") separator
          write(tmp_str, fmt='("dy",i0)') i
          write(out_io_unit, fmt=fmt, advance="no") trim(tmp_str)
       end do
       write(out_io_unit, fmt='()')
    end if
    fmt = '('
    if (tag>=0) fmt = fmt // 'i' // trim(width_str) // ',"' // separator // '",'
    fmt = fmt // 'i' // trim(width_str)
    fmt = fmt // repeat(',"' // separator // '",' // 'f' // trim(width_str) // '.' // trim(digits_str), num_real)
    fmt = fmt // ')'
    do i=start_idx,end_idx,step
       if (present(t_min_o)) then
          if (solution(1, i) < t_min_o) cycle
       end if
       if (present(t_max_o)) then
          if (solution(1, i) > t_max_o) cycle
       end if
       if (tag<0) then
          write (out_io_unit, fmt=fmt) i, solution(1, i), solution(sol_y_idx:(sol_y_idx+2*y_dim-1), i)
       else
          write (out_io_unit, fmt=fmt) tag, i, solution(1, i), solution(sol_y_idx:(sol_y_idx+2*y_dim-1), i)
       end if
    end do
    ! Close file
    if (present(filename_o)) then
       close(unit=out_io_unit, status='keep', iostat=out_io_stat)
       if (out_io_stat /= 0) then
          status = 1153
          return
       end if
    end if
    status = 0
  end subroutine print_solution

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Output an istat array.
  !! 
  !! @verbatim
  !! status ............ Exit status
  !!                     - -inf-0 ..... Everything worked
  !!                     - 1365-1381 .. Error in this routine
  !!                                     - 1365 .. Could not open file for write
  !!                                     - 1366 .. Could not close file         
  !!                     - others ..... No other values allowed
  !! istats(:) ......... Integer statistics from a solver run
  !! idxs_to_prt_o(:) .. Indexes of istats to print.  Indexes too large are silently ignored.
  !! filename_o ..... Filename to which we print.  Default: NONE
  !!                  If not present, then output will be to output_unit (STDOUT).  
  !! @endverbatim
  !! @see mrkiss_utils::status_to_message() 
  !! 
  subroutine print_istats(status, istats, idxs_to_prt_o, filename_o) 
    use, intrinsic :: iso_fortran_env, only: output_unit
    use            :: mrkiss_config,   only: istats_size, istats_max_idx
    implicit none
    ! Arguments
    integer,          intent(out)          :: status
    integer,          intent(in)           :: istats(istats_size)
    integer,          intent(in), optional :: idxs_to_prt_o(:)
    character(len=*), intent(in), optional :: filename_o
    ! Local paramaters
    integer, parameter                     :: ml = 73
    character(len=ml), parameter           :: desc(istats_size) = [ "Computed solution points                                                 ", &
                                                                    "Number of one_step_* calls not triggered by an event                     ", &
                                                                    "Number of one_step_* calls triggered by y_delta length constraint        ", &
                                                                    "Number of one_step_* calls triggered by y_delta error constraint         ", &
                                                                    "Number of one_step_* calls triggered by step processing with new t_delta ", &
                                                                    "Number of one_step_* calls triggered by SDF bisection                    ", &
                                                                    "Bisection failures due to max_bisect_o                                   ", &
                                                                    "Bisection failures due to target containment                             ", &
                                                                    "                                                                         ", &
                                                                    "                                                                         ", &
                                                                    "                                                                         ", &
                                                                    "                                                                         ", &
                                                                    "                                                                         ", &
                                                                    "                                                                         ", &
                                                                    "                                                                         ", &
                                                                    "                                                                         "  ]
    ! Local variables
    integer,          allocatable          :: idxs_to_prt(:)
    integer                                :: out_io_stat, out_io_unit, i
    character(len=32)                      :: tmp_str1, tmp_str2
    ! Process arguments
    idxs_to_prt = [1, 2, 3, 4, 5, 6, 7, 8]
    if (present(idxs_to_prt_o)) idxs_to_prt = idxs_to_prt_o
    ! Open file
    if (present(filename_o)) then
       open(newunit=out_io_unit, file=filename_o, form='formatted', action='write', iostat=out_io_stat)
       if (out_io_stat /= 0) then
          status = 1365
          return
       end if
    else
       out_io_unit = output_unit
    end if
    ! Print 
    write(tmp_str1, '("(a",i0,",i10)")') ml + 7 + 2 + 3 + 2 + 1
    do i=1,size(idxs_to_prt)
       if (i <= istats_max_idx) then
          write(tmp_str2, '(i2.2)') i
          write (out_io_unit, fmt=tmp_str1) ("istats(" // trim(tmp_str2) // "): " // trim(desc(idxs_to_prt(i))) // ": "), istats(idxs_to_prt(i))
       end if
    end do
    ! Close file
    if (present(filename_o)) then
       close(unit=out_io_unit, status='keep', iostat=out_io_stat)
       if (out_io_stat /= 0) then
          status = 1366
          return
       end if
    end if
    status = 0
  end subroutine print_istats

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Analyze an RK solution matrix.
  !! 
  !! @verbatim
  !! status .............. Exit status
  !!                        - -inf-0 ..... Everything worked
  !!                        - 1297-1313 .. Error in this routine
  !!                                        - 1297 .. Could not open file for write
  !!                                        - 1313 .. Could not close file         
  !!                        - others ..... No other values allowed
  !! solution ............ Matrix with solution values
  !! y_dim_o ............. Number of elements in y
  !! start_o ............. Starting index to print in solution. Default: 1
  !! end_o ............... Ending index to print in solution.  Default: Number of columns in solution.
  !! sol_y_idx_o ......... Index of y in solution.  Default: 2
  !! y_delta_len_idxs_o .. Components of y_delta to use for y_delta length computation
  !! filename_o .......... Filename to which we print.  Default: NONE
  !!                       If not present, then output will be to output_unit (STDOUT).  
  !! @endverbatim
  !! @see mrkiss_utils::status_to_message() 
  !! 
  subroutine analyze_solution(status, solution, filename_o, y_dim_o, start_o, end_o, sol_y_idx_o, y_delta_len_idxs_o) 
    use, intrinsic :: iso_fortran_env, only: output_unit
    use            :: mrkiss_config,   only: rk
    implicit none
    ! Arguments
    integer,          intent(out)          :: status
    real(kind=rk),    intent(in)           :: solution(:,:)
    character(len=*), intent(in), optional :: filename_o
    integer,          intent(in), optional :: y_dim_o, sol_y_idx_o, y_delta_len_idxs_o(:), start_o, end_o
    ! Local variables
    integer                                :: y_dim, sol_y_idx, end_idx, start_idx
    integer                                :: i
    integer                                :: out_io_stat, out_io_unit
    real(kind=rk)                          :: t_max, t_min, t_delta_max, t_delta_min, y_delta_len_max, y_delta_len_min
    real(kind=rk), allocatable             :: dy_max(:), dy_min(:), y_max(:), y_min(:)
    real(kind=rk), allocatable             :: y_delta_max(:), y_delta_min(:)
    ! Process arguments
    start_idx = 1
    if (present(start_o)) start_idx = start_o
    end_idx = size(solution, 2)
    if (present(end_o)) end_idx = min(end_o, size(solution, 2))
    sol_y_idx = 2
    if (present(sol_y_idx_o)) sol_y_idx = sol_y_idx_o
    if (present(y_dim_o)) then
       y_dim = y_dim_o
    else ! If solution contains exactily one solution set with no extra columns and sol_y_idx_o==1, then we can guess y_dim
       y_dim = size(solution, 1)
       y_dim = y_dim - 1
       y_dim = y_dim / 2
    end if
    ! Compute 
    if(end_idx - start_idx > 1) then
       t_delta_max =  abs(solution(1, start_idx+1) - solution(1, start_idx))
       t_delta_min =  t_delta_max
       if (present(y_delta_len_idxs_o)) then
          y_delta_len_max =  norm2(solution(y_delta_len_idxs_o + sol_y_idx - 1, start_idx+1) - solution(y_delta_len_idxs_o + sol_y_idx - 1, start_idx))
       else
          y_delta_len_max =  norm2(solution(sol_y_idx:(sol_y_idx+y_dim-1), start_idx+1) - solution(sol_y_idx:(sol_y_idx+y_dim-1), start_idx))
       end if
       y_delta_len_min =  y_delta_len_max
       y_delta_max =  abs(solution(sol_y_idx:(sol_y_idx+y_dim-1), start_idx+1) - solution(sol_y_idx:(sol_y_idx+y_dim-1), start_idx))
       y_delta_min =  y_delta_max
    end if
    y_max =  solution(sol_y_idx:(sol_y_idx+y_dim-1), start_idx)
    y_min =  y_max
    dy_max = solution((sol_y_idx+y_dim):(sol_y_idx+2*y_dim-1), start_idx)
    dy_min = dy_max
    t_max =  solution(1, start_idx)
    t_min =  t_max
    do i=(start_idx+1),end_idx
       y_max =  max(y_max, solution(sol_y_idx:(sol_y_idx+y_dim-1), i))
       y_min =  min(y_min, solution(sol_y_idx:(sol_y_idx+y_dim-1), i))
       dy_max = max(dy_max, solution((sol_y_idx+y_dim):(sol_y_idx+2*y_dim-1), i))
       dy_min = min(dy_min, solution((sol_y_idx+y_dim):(sol_y_idx+2*y_dim-1), i))
       t_max =  max(t_max, solution(1, i))
       t_min =  min(t_min, solution(1, i))
       t_delta_max =  max(t_delta_max, abs(solution(1, i) - solution(1, i-1)))
       t_delta_min =  min(t_delta_min, abs(solution(1, i) - solution(1, i-1)))
       if (present(y_delta_len_idxs_o)) then
          y_delta_len_max =  max(y_delta_len_max, norm2(solution(y_delta_len_idxs_o + sol_y_idx - 1, i) - solution(y_delta_len_idxs_o + sol_y_idx - 1, i-1)))
          y_delta_len_min =  min(y_delta_len_min, norm2(solution(y_delta_len_idxs_o + sol_y_idx - 1, i) - solution(y_delta_len_idxs_o + sol_y_idx - 1, i-1)))
       else
          y_delta_len_max =  max(y_delta_len_max, norm2(solution(sol_y_idx:(sol_y_idx+y_dim-1), i) - solution(sol_y_idx:(sol_y_idx+y_dim-1), i-1)))
          y_delta_len_min =  min(y_delta_len_min, norm2(solution(sol_y_idx:(sol_y_idx+y_dim-1), i) - solution(sol_y_idx:(sol_y_idx+y_dim-1), i-1)))
       end if
       y_delta_max =  max(y_delta_max, abs(solution(sol_y_idx:(sol_y_idx+y_dim-1), i) - solution(sol_y_idx:(sol_y_idx+y_dim-1), i-1)))
       y_delta_min =  min(y_delta_min, abs(solution(sol_y_idx:(sol_y_idx+y_dim-1), i) - solution(sol_y_idx:(sol_y_idx+y_dim-1), i-1)))
    end do
    ! Open file
    if (present(filename_o)) then
       open(newunit=out_io_unit, file=filename_o, form='formatted', action='write', iostat=out_io_stat)
       if (out_io_stat /= 0) then
          status = 1297
          return
       end if
    else
       out_io_unit = output_unit
    end if
    ! Print report
    out_io_unit = output_unit
    write(out_io_unit, fmt='(a20)')           "Solution Analysis   "
    write(out_io_unit, fmt='(a25,*(i20))')    "            size 1: ", size(solution, 1)
    write(out_io_unit, fmt='(a25,*(i20))')    "            size 2: ", size(solution, 2)
    write(out_io_unit, fmt='(a25,*(f20.15))') "             Max y: ", y_max
    write(out_io_unit, fmt='(a25,*(f20.15))') "             Min y: ", y_min
    write(out_io_unit, fmt='(a25,*(f20.15))') "            Max dy: ", dy_max
    write(out_io_unit, fmt='(a25,*(f20.15))') "            Min dy: ", dy_min
    write(out_io_unit, fmt='(a25,*(f20.15))') "             Max t: ", t_max
    write(out_io_unit, fmt='(a25,*(f20.15))') "             Min t: ", t_min
    write(out_io_unit, fmt='(a25,*(f20.15))') "       Max delta t: ", t_delta_max       
    write(out_io_unit, fmt='(a25,*(f20.15))') "       Min delta t: ", t_delta_min       
    write(out_io_unit, fmt='(a25,*(f20.15))') "       Max delta y: ", y_delta_max       
    write(out_io_unit, fmt='(a25,*(f20.15))') "       Min delta y: ", y_delta_min       
    write(out_io_unit, fmt='(a25,*(f20.15))') "   Max delta y len: ", y_delta_len_max       
    write(out_io_unit, fmt='(a25,*(f20.15))') "   Min delta y len: ", y_delta_len_min       
    ! Close file
    if (present(filename_o)) then
       close(unit=out_io_unit, status='keep', iostat=out_io_stat)
       if (out_io_stat /= 0) then
          status = 1313
          return
       end if
    end if
    status = 0
  end subroutine analyze_solution

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @name Miscellaneous

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Produce a sequence of values with fixed seporation.  Modeled after R's seq() function.
  !!
  !! @verbatim
  !! status .............. Exit status
  !!                        - -inf-0 ..... Everything worked
  !!                        - 1314:1330 .. Error in this routine
  !!                                        - 1314 .. Inconsistant sequence values: step_v_o * (size(t)-1) /= to_v - from_v
  !! y_at_t .............. The value of the function at t
  !! t ................... Vector to fill
  !! from_o .............. Starting value
  !! to_o ................ Ending value
  !! step_o .............. Delta between valeus
  !! @endverbatim
  !! @see mrkiss_utils::status_to_message() 
  !! 
  subroutine seq(status, t, from_o, to_o, step_o) 
    use :: mrkiss_config, only: rk, zero_epsilon
    implicit none
    ! Arguments
    integer,                 intent(out) :: status
    real(kind=rk),           intent(out) :: t(:)
    real(kind=rk), optional, intent(in)  :: from_o, to_o, step_o
    ! Variables
    integer                              :: n_v, i
    real(kind=rk)                        :: from_v, to_v, step_v
    ! Compute paramaters
    if     (.not. (present(from_o))) then
       status = 0
       n_v    = size(t, 1) - 1
       to_v   = to_o
       step_v = step_o
       from_v = -step_v * n_v + to_v
    elseif (.not. (present(to_o))) then
       n_v    = size(t, 1) - 1
       step_v = step_o
       from_v = from_o
       to_v   = step_v * n_v + from_v
    elseif (.not. (present(step_o))) then
       status = 0
       n_v    = size(t, 1) - 1
       to_v   = to_o
       from_v = from_o
       step_v = -(-to_v + from_v) / n_v
    else
       n_v    = size(t, 1) - 1
       to_v   = to_o
       from_v = from_o
       step_v = step_o
       if (abs((step_v * n_v) - (to_v - from_v)) > zero_epsilon) then
          status = 1314
       else
          status = 0
       end if
    end if
    ! Compute seqeunce
    if (status == 0) then
       t = (/(from_v+i*step_v, i=0,n_v,1)/)
    end if
  end subroutine seq

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @name Status Codes

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return, as a string, the source of a status code.
  !! 
  !! Status codes are assigned in blocks to subroutines and interfaces.  Status codes are frequently "passed up" the call chain.
  !! i.e. a routine may return a status code that was returned to it by another routine.  Assigning the codes in blocks allows
  !! the user to know from which subroutine a status originated.
  !! 
  !! Assigned Status Ranges
  !! - 0001-0255 ... interface  deq_iface_*t
  !! - 0256-0511 ... interface  stepp_iface_*t
  !! - 0512-0767 ... interface  sdf_iface_*t
  !! - 1232-1247 ... subroutine one_step_etab_*t
  !! - 1248-1263 ... subroutine one_step_stab_*t
  !! - 1216-1231 ... subroutine one_richardson_step_stab_*t
  !! - 1200-1215 ... subroutine one_step_rk4_*t
  !! - 1184-1199 ... subroutine one_step_rkf45_*t
  !! - 1263-1279 ... subroutine one_step_dp54_*t
  !! - 1120-1151 ... subroutine steps_fixed_stab_*t
  !! - 1024-1055 ... subroutine steps_condy_stab_*t
  !! - 1280-1296 ... subroutine steps_sloppy_condy_stab_*t
  !! - 1056-1119 ... subroutine steps_adapt_etab_*t
  !! - 1152-1183 ... subroutine print_solution
  !! - 1297-1313 ... subroutine analyze_solution
  !! - 1314:1330 ... subroutine seq
  !! - 1331:1347 ... subroutine interpolate_solution
  !! - 1348-1364 ... subroutine steps_points_stab_*t
  !! - 1365-1381 ... subroutine print_istats
  !! - 1382-1398 ... Unallocated
  !! - 1399-1415 ... Unallocated
  !! 
  !! I use the following bit of code to generate new blocks:
  !! @verbatim
  !! (let ((s "\n"))
  !!   (cl-loop for f from 1348 to 2000 by 17
  !!            do (print f)
  !!            do (setq s (concat s (format "%d-%d\n" f (+ 16 f)))))
  !!   s)
  !! @endverbatim
  !! 
  !! @verbatim
  !! status ...................... This is an intent(in) argument!!!!
  !! status_to_origin(len=32) .... A string identifying the origin of the status code.
  !!                                - A subroutine or interface name
  !!                                - "NO ERROR" for a non-error status of unknown origin
  !!                                - "UNKNOWN SOURCE" for an error status of unknown origin
  !! @endverbatim
  !! @see mrkiss_utils::status_to_message() 
  !!
  character(len=32) function status_to_origin(status)
    implicit none
    ! Arguments
    integer,          intent(in) :: status
    ! Process Input
    if     (status  <=    0) then
       status_to_origin = "NO ERROR"
    elseif ((status >=    1) .and. (status <=  255)) then
       status_to_origin = "deq_iface_*t"
    elseif ((status >=  256) .and. (status <=  511)) then
       status_to_origin = "stepp_iface_*t"
    elseif ((status >=  512) .and. (status <=  767)) then
       status_to_origin = "sdf_iface_*t"
    elseif ((status >= 1232) .and. (status <= 1247)) then
       status_to_origin = "one_step_etab_*t"
    elseif ((status >= 1248) .and. (status <= 1263)) then
       status_to_origin = "one_step_stab_*t"
    elseif ((status >= 1216) .and. (status <= 1231)) then
       status_to_origin = "one_richardson_step_stab_*t"
    elseif ((status >= 1200) .and. (status <= 1215)) then
       status_to_origin = "one_step_rk4_*t"
    elseif ((status >= 1184) .and. (status <= 1199)) then
       status_to_origin = "one_step_rkf45_*t"
    elseif ((status >= 1263) .and. (status <= 1279)) then
       status_to_origin = "one_step_dp54_*t"
    elseif ((status >= 1120) .and. (status <= 1151)) then
       status_to_origin = "steps_fixed_stab_*t"
    elseif ((status >= 1024) .and. (status <= 1055)) then
       status_to_origin = "steps_condy_stab_*t"
    elseif ((status >= 1280) .and. (status <= 1296)) then
       status_to_origin = "steps_sloppy_condy_stab_*t"
    elseif ((status >= 1056) .and. (status <= 1119)) then
       status_to_origin = "steps_adapt_etab_*t"
    elseif ((status >= 1152) .and. (status <= 1183)) then
       status_to_origin = "print_solution"
    elseif ((status >= 1297) .and. (status <= 1313)) then
       status_to_origin = "analyze_solution"
    elseif ((status >= 1314) .and. (status <= 1330)) then
       status_to_origin = "seq"
    elseif ((status >= 1331) .and. (status <= 1347)) then
       status_to_origin = "interpolate_solution"
    elseif ((status >= 1348) .and. (status <= 1364)) then
       status_to_origin = "steps_points_stab_*t"
    elseif ((status >= 1365) .and. (status <= 1381)) then
       status_to_origin = "print_istats"
    else
       status_to_origin = "UNKNOWN SOURCE"
    end if
  end function status_to_origin

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return, as a string, the source of a status code.
  !! 
  !! @verbatim
  !! status ...................... This is an intent(in) argument!!!!
  !! status_to_message(len=128) .. A string identifying the message of the status code.
  !!                                - "SOURCE: MESSAGE"
  !!                                  - SOURCE will be the subroutine or interface if known, and "UNKNOWN SOURCE" otherwise
  !!                                  - MESSAGE will be the error message if known, and "UNKNOWN ERROR" otherwise
  !!                                - "NO ERROR" for a non-error status of unknown message
  !! @endverbatim
  !! @see mrkiss_utils::status_to_origin() 
  !!
  character(len=128) function status_to_message(status)
    implicit none
    ! Arguments
    integer,          intent(in) :: status
    ! Process Input
    if (status <= 0) then
       status_to_message = "NO ERROR"
       return
    elseif (status == 1024) then
       status_to_message = "t_delta_min yielded a longer step than t_delta_max"
    elseif (status == 1025) then
       status_to_message = "no_bisect_error_o not present and max_bisect_o violated"
    elseif (status == 1056) then
       status_to_message = ". no_bisect_error_o not present and max_bisect_o violated"
    elseif (status == 1152) then
       status_to_message = "Could not open file for write"
    elseif (status == 1153) then
       status_to_message = "Could not close file         "
    elseif (status == 1297) then
       status_to_message = "Could not open file for write"
    elseif (status == 1313) then
       status_to_message = "Could not close file         "
    elseif (status == 1314) then
       status_to_message = "Inconsistant sequence values: step_v_o * (size(t)-1) /= to_v - from_v"
    elseif (status == 1331) then
       status_to_message = "... new_solution t value out of bounds"
    elseif (status == 1365) then
       status_to_message = "Could not open file for write"
    elseif (status == 1366) then
       status_to_message = "Could not close file"
    else
       status_to_message = "UNKNOWN ERROR"
    end if
    status_to_message = trim(status_to_origin(status)) // ": " // status_to_message 
  end function status_to_message

end module mrkiss_utils





