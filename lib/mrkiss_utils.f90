! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
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
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

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
  !! Examples
  !!  - 1: The default values will produce CSV output @n
  !!    `print_solution(solution)`
  !!  - 2: For columnar output we just need to add a width @n
  !!    `print_solution(solution, fmt_w_o=30)`
  !!
  !! @param status        Exit status
  !!                        | Value     | Description
  !!                        |-----------|------------
  !!                        | -inf-0    | Everything worked
  !!                        | 1152      | Could not open file for write
  !!                        | 1153      | Could not close file
  !!                        | 1154-1183 | Unknown error in this routine
  !! @param solution       Array containing the solution.
  !!                       Each COLUMN is a solution:
  !!                        - First element is the @f$t@f$ variable
  !!                        - `size(y, 1)` elements starting with 2 have @f$\mathbf{y}@f$ values
  !!                        - The next `size(y, 1)` elements have @f$\mathbf{y}'@f$ values
  !! @param filename_o    Filename to which we print.  Default: NONE
  !!                       - If not present, then output will be to output_unit (`STDOUT`).
  !! @param fmt_d_o       Number of digits for floating point numbers.  Default: mrkiss_config::fmt_d_ai
  !! @param fmt_w_o       Width of print format for all entities. Default: 0
  !!                       - If set to -1, then mrkiss_config::fmt_w_ai will be uesd.
  !! @param fmt_e_o       Number of digits in the exponent of floating point numbers.  Default: mrkiss_config::fmt_e_ai
  !!                       - Ignored if `fmt_w_o==0`
  !! @param start_o       Starting index to print in \p solution. Default: 1
  !! @param end_o         Ending index to print in \p solution.  Default: Number of columns in \p solution.
  !! @param step_o        Print only every \p step_o th value in solution. Default: 1
  !! @param prt_titles_o  Print titles if `.TRUE.`  Default: `.not. append_o`
  !! @param separator_o   String to place between fields.  Default: ',' if fmt_w_o missing, and ' ' otherwise.
  !! @param t_min_o       Print only solutions with time values `>= t_min_o`
  !! @param t_max_o       Print only solutions with time values `<= t_min_o`
  !! @param tag_o         If non-negative, this integer that will become the first column of the output. Default: -1
  !! @param append_o      Append to file instead of overwriting.  Default: `.FALSE.`
  !!
  subroutine print_solution(status, solution, filename_o, separator_o, fmt_w_o, fmt_d_o, fmt_e_o, start_o, end_o, step_o, &
       &                    prt_titles_o, t_min_o, t_max_o, tag_o, append_o)
    use, intrinsic :: iso_fortran_env, only: output_unit
    use            :: mrkiss_config,   only: rk, fmt_d_ai, fmt_w_ai, fmt_e_ai
    implicit none
    ! Arguments
    integer,          intent(out)          :: status
    real(kind=rk),    intent(in)           :: solution(:,:)
    character(len=*), intent(in), optional :: filename_o, separator_o
    integer,          intent(in), optional :: fmt_e_o, fmt_d_o, fmt_w_o, start_o, end_o, step_o, tag_o
    real(kind=rk),    intent(in), optional :: t_min_o, t_max_o
    logical,          intent(in), optional :: prt_titles_o, append_o
    ! Local variables
    integer                                :: fmt_e, fmt_d, fmt_w, start_idx, end_idx, step, y_dim, sol_y_idx, tag
    logical                                :: prt_titles, append
    integer                                :: i, num_int, num_real, out_io_stat, out_io_unit
    character(len=:), allocatable          :: fmt, separator, access_mode, fmt_r_str, fmt_i_str, fmt_a_str
    character(len=512)                     :: tmp_str
    ! Process arguments
    append = .false.
    if (present(append_o)) append = append_o
    prt_titles = .not. append
    if (present(prt_titles_o)) prt_titles = prt_titles_o
    tag = -1
    if (present(tag_o)) tag = tag_o
    step = 1
    if (present(step_o)) step = step_o
    fmt_d = fmt_d_ai
    if (present(fmt_d_o)) fmt_d = fmt_d_o
    fmt_e = fmt_e_ai
    if (present(fmt_e_o)) fmt_e = fmt_e_o
    fmt_w = 0
    if (present(fmt_w_o)) fmt_w = fmt_w_o
    if (fmt_w == -1) fmt_w = fmt_w_ai
    start_idx = 1
    if (present(start_o)) start_idx = start_o
    end_idx = size(solution, 2)
    if (present(end_o)) end_idx = min(end_o, size(solution, 2))
    if (present(separator_o)) then
       separator = separator_o
    else
       if (fmt_w == 0) then
          separator = ","
       else
          separator = " "
       end if
    end if
    ! Compute solution meta-data
    y_dim = (size(solution, 1) - 1) / 2
    sol_y_idx = 2
    ! Create format string components
    if (fmt_w == 0) then
       ! use f edit descriptor.  Ignore fmt_e.
       write(tmp_str, '("f",i0,".",i0)') fmt_w, fmt_d
       fmt_r_str = trim(tmp_str)
    else
       ! use e edit descriptor
       write(tmp_str, '("e",i0,".",i0)') fmt_w, fmt_d
       fmt_r_str = trim(tmp_str)
       if (fmt_e > 0) then
          write(tmp_str, '("e",i0)') fmt_e
          fmt_r_str = fmt_r_str // trim(tmp_str)
       end if
    end if
    write(tmp_str, '("i",i0)') fmt_w
    fmt_i_str = trim(tmp_str)
    if (fmt_w == 0) then
       fmt_a_str = "a"
    else
       write(tmp_str, '("a",i0)') fmt_w
       fmt_a_str = trim(tmp_str)
    end if
    ! Figure out how many items per line
    num_real = size(solution, 1)
    num_int = 0
    if (tag<0) then
       num_int = 1
    else
       num_int = 2
    end if
    ! Open file
    if (append) then
       access_mode = 'APPEND'
    else
       access_mode = 'SEQUENTIAL'
    end if
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
       fmt = "(" // fmt_a_str // ")"
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
    if (tag>=0) fmt = fmt // fmt_i_str // ',"' // separator // '",'
    fmt = fmt // fmt_i_str
    fmt = fmt // repeat(',"' // separator // '",' // fmt_r_str, num_real)
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
  !! @param status            Exit status
  !!                            | Value     | Description
  !!                            |-----------|------------
  !!                            | -inf-0    | Everything worked
  !!                            | 1365      | Could not open file for write
  !!                            | 1366      | Could not close file
  !!                            | 1367-1381 | Unknown error in this routine
  !! @param istats(:)         Integer statistics from a solver run. See mrkiss_utils::print_istats() for description of elements.
  !! @param idxs_to_prt_o(:)  Indexes of \p istats to print.  Indexes too large are silently ignored.
  !! @param filename_o        Filename to which we print.  Default: NONE
  !!                           - If not present, then output will be to output_unit (`STDOUT`).
  !! @param fmt_w_o           Width of print format for all entities. Default: mrkiss_config::fmt_w_ai
  !! @param prt_zeros_o       If `.TRUE.`, then print zero values.  Default: `.FALSE.`
  !!
  subroutine print_istats(status, istats, idxs_to_prt_o, filename_o, fmt_w_o, prt_zeros_o)
    use, intrinsic :: iso_fortran_env, only: output_unit
    use            :: mrkiss_config,   only: istats_size, istats_max_idx, fmt_w_ai, istats_str_lng, istats_strs
    implicit none
    ! Arguments
    integer,          intent(out)          :: status
    integer,          intent(in)           :: istats(istats_size)
    integer,          intent(in), optional :: idxs_to_prt_o(:), fmt_w_o
    logical,          intent(in), optional :: prt_zeros_o
    character(len=*), intent(in), optional :: filename_o
    ! Local variables
    integer, allocatable                   :: idxs_to_prt(:)
    integer                                :: out_io_stat, out_io_unit, i, fmt_w
    logical                                :: prt_zeros
    character(len=32)                      :: tmp_str1, tmp_str2
    ! Process arguments
    prt_zeros = .false.
    if (present(prt_zeros_o)) prt_zeros = prt_zeros_o
    idxs_to_prt = [(i, i=1,istats_max_idx)]
    if (present(idxs_to_prt_o)) idxs_to_prt = idxs_to_prt_o
    fmt_w = fmt_w_ai
    if (present(fmt_w_o)) fmt_w = fmt_w_o
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
    write(out_io_unit, fmt='(a25)') "ISTATS Contents"
    write(tmp_str1, '("(a",i0,",i",i0,")")') istats_str_lng + 7 + 2 + 3 + 2 + 1, fmt_w
    do i=1,size(idxs_to_prt)
       if (i <= istats_max_idx) then
          if ((istats(idxs_to_prt(i)) > 0) .or. prt_zeros) then
             write(tmp_str2, '(i2.2)') i
             write (out_io_unit, fmt=tmp_str1) (trim(istats_strs(idxs_to_prt(i))) // ": istats(" // trim(tmp_str2) // "): "), istats(idxs_to_prt(i))
          end if
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
  !! @param status              Exit status
  !!                              | Value     | Description
  !!                              |-----------|------------
  !!                              | -inf-0    | Everything worked
  !!                              | 1297      | Could not open file for write
  !!                              | 1298      | Could not close file
  !!                              | 1299-1313 | Unknown error in this routine
  !! @param solution            Array containing the solution.
  !!                            Each COLUMN is a solution:
  !!                             - First element is the @f$t@f$ variable
  !!                             - `size(y, 1)` elements starting with 2 have @f$\mathbf{y}@f$ values
  !!                             - The next `size(y, 1)` elements have @f$\mathbf{y}'@f$ values
  !! @param start_o             Starting index to use in \p solution. Default: 1
  !! @param end_o               Ending index to use in \p solution.  Default: Number of columns in \p solution.
  !! @param y_delta_len_idxs_o  Components of @f$\mathbf{\Delta{y}}@f$ to use for length computation
  !! @param filename_o          Filename to which we print.  Default: NONE
  !!                             - If not present, then output will be to output_unit (`STDOUT`).
  !! @param fmt_d_o             Number of digits for floating point numbers.  Default: mrkiss_config::fmt_d_ai
  !! @param fmt_w_o             Width of print format for all entities. Default: mrkiss_config::fmt_w_ai
  !! @param fmt_e_o             Number of digits in the exponent of floating point numbers.  Default: mrkiss_config::fmt_e_ai
  !!
  subroutine analyze_solution(status, solution, filename_o, start_o, end_o, y_delta_len_idxs_o, fmt_w_o, fmt_d_o, fmt_e_o)
    use, intrinsic :: iso_fortran_env, only: output_unit
    use            :: mrkiss_config,   only: rk, fmt_d_ai, fmt_w_ai, fmt_e_ai
    implicit none
    ! Arguments
    integer,          intent(out)          :: status
    real(kind=rk),    intent(in)           :: solution(:,:)
    character(len=*), intent(in), optional :: filename_o
    integer,          intent(in), optional :: y_delta_len_idxs_o(:), start_o, end_o, fmt_e_o, fmt_d_o, fmt_w_o
    ! Local variables
    integer                                :: y_dim, end_idx, start_idx, fmt_e, fmt_d, fmt_w, i, out_io_stat, out_io_unit
    real(kind=rk)                          :: t_max, t_min, t_delta_max, t_delta_min, y_delta_len_max, y_delta_len_min
    real(kind=rk), allocatable             :: dy_max(:), dy_min(:), y_max(:), y_min(:), y_delta_max(:), y_delta_min(:)
    character(len=32)                      :: tmp_str
    character(len=:), allocatable          :: fmt, fmt_r_str, fmt_i_str
    ! Process arguments
    fmt_d = fmt_d_ai
    if (present(fmt_d_o)) fmt_d = fmt_d_o
    fmt_e = fmt_e_ai
    if (present(fmt_e_o)) fmt_e = fmt_e_o
    fmt_w = fmt_w_ai
    if (present(fmt_w_o)) fmt_w = fmt_w_o
    start_idx = 1
    if (present(start_o)) start_idx = start_o
    end_idx = size(solution, 2)
    if (present(end_o)) end_idx = min(end_o, size(solution, 2))
    ! Create format string components
    if (fmt_w == 0) then
       ! use f edit descriptor.  Ignore fmt_e.
       write(tmp_str, '("f",i0,".",i0)') fmt_w, fmt_d
       fmt_r_str = trim(tmp_str)
    else
       ! use e edit descriptor
       write(tmp_str, '("e",i0,".",i0)') fmt_w, fmt_d
       fmt_r_str = trim(tmp_str)
       if (fmt_e > 0) then
          write(tmp_str, '("e",i0)') fmt_e
          fmt_r_str = fmt_r_str // trim(tmp_str)
       end if
    end if
    write(tmp_str, '("i",i0)') fmt_w
    fmt_i_str = trim(tmp_str)
    ! Compute
    y_dim = (size(solution, 1) - 1) / 2
    if(end_idx - start_idx > 1) then
       t_delta_max =  abs(solution(1, start_idx+1) - solution(1, start_idx))
       t_delta_min =  t_delta_max
       if (present(y_delta_len_idxs_o)) then
          y_delta_len_max =  norm2(solution(y_delta_len_idxs_o + 2 - 1, start_idx+1) - solution(y_delta_len_idxs_o + 2 - 1, start_idx))
       else
          y_delta_len_max =  norm2(solution(2:(2+y_dim-1), start_idx+1) - solution(2:(2+y_dim-1), start_idx))
       end if
       y_delta_len_min =  y_delta_len_max
       y_delta_max =  abs(solution(2:(2+y_dim-1), start_idx+1) - solution(2:(2+y_dim-1), start_idx))
       y_delta_min =  y_delta_max
    end if
    y_max =  solution(2:(2+y_dim-1), start_idx)
    y_min =  y_max
    dy_max = solution((2+y_dim):(2+2*y_dim-1), start_idx)
    dy_min = dy_max
    t_max =  solution(1, start_idx)
    t_min =  t_max
    do i=(start_idx+1),end_idx
       y_max =  max(y_max, solution(2:(2+y_dim-1), i))
       y_min =  min(y_min, solution(2:(2+y_dim-1), i))
       dy_max = max(dy_max, solution((2+y_dim):(2+2*y_dim-1), i))
       dy_min = min(dy_min, solution((2+y_dim):(2+2*y_dim-1), i))
       t_max =  max(t_max, solution(1, i))
       t_min =  min(t_min, solution(1, i))
       t_delta_max =  max(t_delta_max, abs(solution(1, i) - solution(1, i-1)))
       t_delta_min =  min(t_delta_min, abs(solution(1, i) - solution(1, i-1)))
       if (present(y_delta_len_idxs_o)) then
          y_delta_len_max =  max(y_delta_len_max, norm2(solution(y_delta_len_idxs_o + 2 - 1, i) - solution(y_delta_len_idxs_o + 2 - 1, i-1)))
          y_delta_len_min =  min(y_delta_len_min, norm2(solution(y_delta_len_idxs_o + 2 - 1, i) - solution(y_delta_len_idxs_o + 2 - 1, i-1)))
       else
          y_delta_len_max =  max(y_delta_len_max, norm2(solution(2:(2+y_dim-1), i) - solution(2:(2+y_dim-1), i-1)))
          y_delta_len_min =  min(y_delta_len_min, norm2(solution(2:(2+y_dim-1), i) - solution(2:(2+y_dim-1), i-1)))
       end if
       y_delta_max =  max(y_delta_max, abs(solution(2:(2+y_dim-1), i) - solution(2:(2+y_dim-1), i-1)))
       y_delta_min =  min(y_delta_min, abs(solution(2:(2+y_dim-1), i) - solution(2:(2+y_dim-1), i-1)))
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
    write(out_io_unit, fmt='(a25)') "Solution Analysis   "
    fmt = '(a25,' // fmt_i_str // ')'
    write(out_io_unit, fmt=fmt)   "            size 1: ", size(solution, 1)
    write(out_io_unit, fmt=fmt)   "            size 2: ", size(solution, 2)
    fmt = '(a25,*(' // fmt_r_str // '))'
    write(out_io_unit, fmt=fmt)   "             Max y: ", y_max
    write(out_io_unit, fmt=fmt)   "             Min y: ", y_min
    write(out_io_unit, fmt=fmt)   "            Max dy: ", dy_max
    write(out_io_unit, fmt=fmt)   "            Min dy: ", dy_min
    write(out_io_unit, fmt=fmt)   "             Max t: ", t_max
    write(out_io_unit, fmt=fmt)   "             Min t: ", t_min
    write(out_io_unit, fmt=fmt)   "       Max delta t: ", t_delta_max
    write(out_io_unit, fmt=fmt)   "       Min delta t: ", t_delta_min
    write(out_io_unit, fmt=fmt)   "       Max delta y: ", y_delta_max
    write(out_io_unit, fmt=fmt)   "       Min delta y: ", y_delta_min
    write(out_io_unit, fmt=fmt)   "   Max delta y len: ", y_delta_len_max
    write(out_io_unit, fmt=fmt)   "   Min delta y len: ", y_delta_len_min
    ! Close file
    if (present(filename_o)) then
       close(unit=out_io_unit, status='keep', iostat=out_io_stat)
       if (out_io_stat /= 0) then
          status = 1298
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
  !! Exactly two of @p from_o, @p to_o, and @p step_o should be provided.  The one not provided will be computed from the others.
  !! If all three are are provided, then they will be checked for consistency.  Note that this consistency check is not
  !! completely reliable due to round-off error.  This is why only two should be provided.
  !!
  !! @param status     Exit status
  !!                     | Value     | Description
  !!                     |-----------|------------
  !!                     | -inf-0    | Everything worked
  !!                     | 1314      | Inconsistant sequence values: `step_v_o * (size(t)-1) /= to_v - from_v`
  !!                     | 1315      | Not enough arguments
  !!                     | 1316-1330 | Unknown error in this routine
  !! @param t          Vector to fill with @f$t@f$ values
  !! @param from_o     Starting value for @f$t@f$
  !! @param to_o       Ending value for @f$t@f$
  !! @param step_o     Delta between valeus
  !! @param max_pts_o  Maximum number of points to produce.  Default: `size(t, 1)`
  !!
  subroutine seq(status, t, from_o, to_o, step_o, max_pts_o)
    use :: mrkiss_config, only: rk, zero_epsilon
    implicit none
    ! Arguments
    integer,                 intent(out) :: status
    real(kind=rk),           intent(out) :: t(:)
    real(kind=rk), optional, intent(in)  :: from_o, to_o, step_o
    integer,       optional, intent(in)  :: max_pts_o
    ! Variables
    integer                              :: n_v, i, max_pts, num_args
    real(kind=rk)                        :: from_v, to_v, step_v
    ! Process arguments
    max_pts = size(t, 1)
    if (present(max_pts_o)) max_pts = min(max_pts, max_pts_o)
    num_args = 0
    if (present(from_o)) num_args = num_args + 1
    if (present(to_o))   num_args = num_args + 1
    if (present(step_o)) num_args = num_args + 1
    if (num_args < 2) then
       status = 1315
       return
    end if
    ! Compute paramaters
    if     (.not. (present(from_o))) then
       status = 0
       n_v    = max_pts - 1
       to_v   = to_o
       step_v = step_o
       from_v = -step_v * n_v + to_v
    elseif (.not. (present(to_o))) then
       n_v    = max_pts - 1
       step_v = step_o
       from_v = from_o
       to_v   = step_v * n_v + from_v
    elseif (.not. (present(step_o))) then
       status = 0
       n_v    = max_pts - 1
       to_v   = to_o
       from_v = from_o
       step_v = -(-to_v + from_v) / n_v
    else
       n_v    = max_pts - 1
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
  !! i.e. a routine may return a status code that was returned to it by another routine it called.  Assigning the codes in blocks
  !! allows the user to know from which subroutine a status code originated.
  !!
  !! Assigned Status Ranges
  !!   | Range     | Assignment
  !!   |-----------|-----------
  !!   | 0001-0255 | mrkiss_solvers_wt::deq_iface & mrkiss_solvers_nt::deq_iface
  !!   | 0256-0511 | mrkiss_solvers_wt::stepp_iface & mrkiss_solvers_nt::stepp_iface
  !!   | 0512-0767 | mrkiss_solvers_wt::sdf_iface & mrkiss_solvers_nt::sdf_iface
  !!   | 1232-1247 | mrkiss_solvers_wt::step_all & mrkiss_solvers_nt::step_all
  !!   | 1248-1263 | mrkiss_solvers_wt::step_one & mrkiss_solvers_nt::step_one
  !!   | 1216-1231 | mrkiss_solvers_wt::step_richardson & mrkiss_solvers_nt::step_richardson
  !!   | 1200-1215 | mrkiss_solvers_wt::step_rk4 & mrkiss_solvers_nt::step_rk4
  !!   | 1184-1199 | mrkiss_solvers_wt::step_rkf45 & mrkiss_solvers_nt::step_rkf45
  !!   | 1263-1279 | mrkiss_solvers_wt::step_dp54 & mrkiss_solvers_nt::step_dp54
  !!   | 1120-1151 | mrkiss_solvers_wt::fixed_t_steps & mrkiss_solvers_nt::fixed_t_steps
  !!   | 1024-1055 | mrkiss_solvers_wt::fixed_y_steps & mrkiss_solvers_nt::fixed_y_steps
  !!   | 1280-1296 | mrkiss_solvers_wt::sloppy_fixed_y_steps & mrkiss_solvers_nt::sloppy_fixed_y_steps
  !!   | 1056-1119 | mrkiss_solvers_wt::adaptive_steps & mrkiss_solvers_nt::adaptive_steps
  !!   | 1152-1183 | mrkiss_utils::print_solution
  !!   | 1297-1313 | mrkiss_utils::analyze_solution
  !!   | 1314:1330 | mrkiss_utils::seq
  !!   | 1331:1347 | mrkiss_solvers_wt::interpolate_solution & mrkiss_solvers_nt::interpolate_solution
  !!   | 1348-1364 | mrkiss_solvers_wt::fixed_t_steps_between & mrkiss_solvers_nt::fixed_t_steps_between
  !!   | 1365-1381 | mrkiss_utils::print_istats
  !!   | 1382-1398 | Unallocated
  !!   | 1399-1415 | Unallocated
  !!
  !! I use the following bit of Emacs lisp code to generate new blocks:
  !! @verbatim
  !! (let ((s "\n"))
  !!   (cl-loop for f from 1348 to 2000 by 17
  !!            do (print f)
  !!            do (setq s (concat s (format "%d-%d\n" f (+ 16 f)))))
  !!   s)
  !! @endverbatim
  !!
  !! @param status This is an `intent(in)` argument!!!!
  !! @return  A string `(len=32)` identifying the origin of the status code.
  !!            - A subroutine or interface name
  !!            - "NO ERROR" for a non-error status of unknown origin
  !!            - "UNKNOWN SOURCE" for an error status of unknown origin
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
       status_to_origin = "step_all_*t"
    elseif ((status >= 1248) .and. (status <= 1263)) then
       status_to_origin = "step_one_*t"
    elseif ((status >= 1216) .and. (status <= 1231)) then
       status_to_origin = "step_richardson_*t"
    elseif ((status >= 1200) .and. (status <= 1215)) then
       status_to_origin = "step_rk4_*t"
    elseif ((status >= 1184) .and. (status <= 1199)) then
       status_to_origin = "step_rkf45_*t"
    elseif ((status >= 1263) .and. (status <= 1279)) then
       status_to_origin = "step_dp54_*t"
    elseif ((status >= 1120) .and. (status <= 1151)) then
       status_to_origin = "fixed_t_steps_*t"
    elseif ((status >= 1024) .and. (status <= 1055)) then
       status_to_origin = "fixed_y_steps_*t"
    elseif ((status >= 1280) .and. (status <= 1296)) then
       status_to_origin = "sloppy_fixed_y_steps_*t"
    elseif ((status >= 1056) .and. (status <= 1119)) then
       status_to_origin = "adaptive_steps_*t"
    elseif ((status >= 1152) .and. (status <= 1183)) then
       status_to_origin = "print_solution"
    elseif ((status >= 1297) .and. (status <= 1313)) then
       status_to_origin = "analyze_solution"
    elseif ((status >= 1314) .and. (status <= 1330)) then
       status_to_origin = "seq"
    elseif ((status >= 1331) .and. (status <= 1347)) then
       status_to_origin = "interpolate_solution"
    elseif ((status >= 1348) .and. (status <= 1364)) then
       status_to_origin = "fixed_t_steps_between_*t"
    elseif ((status >= 1365) .and. (status <= 1381)) then
       status_to_origin = "print_istats"
    else
       status_to_origin = "UNKNOWN SOURCE"
    end if
  end function status_to_origin

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return, as a string, the source of a status code.
  !!
  !! @param status This is an intent(in) argument!!!!
  !! @return  A string `(len=128)` identifying the message of the status code.
  !!           - Format: "SOURCE: MESSAGE"
  !!             - SOURCE will be the subroutine or interface if known, and "UNKNOWN SOURCE" otherwise
  !!             - MESSAGE will be the error message if known, and "UNKNOWN ERROR" otherwise
  !!           - "NO ERROR" for a non-error status of unknown message
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
       status_to_message = "no_bisect_error_o not present and max_bisect_o violated"
    elseif (status == 1152) then
       status_to_message = "Could not open file for write"
    elseif (status == 1153) then
       status_to_message = "Could not close file"
    elseif (status == 1297) then
       status_to_message = "Could not open file for write"
    elseif (status == 1298) then
       status_to_message = "Could not close file"
    elseif (status == 1314) then
       status_to_message = "Inconsistant sequence values: step_v_o * (size(t)-1) /= to_v - from_v"
    elseif (status == 1315) then
       status_to_message = "Not enough arguments"
    elseif (status == 1331) then
       status_to_message = "new_solution t value out of bounds"
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
