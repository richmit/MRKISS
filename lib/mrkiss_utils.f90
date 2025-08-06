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

  public :: print_solution, analyze_solution
  public :: seq
  public :: interpolate_solution
  public :: status_to_origin, status_to_message

contains
  
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
  !! status ....... Exit status
  !!                 - -inf-0 ..... Everything worked
  !!                 - 1152-1183 .. Error in this routine
  !!                                 - 1152 .. Could not open file for write
  !!                                 - 1153 .. Could not close file         
  !!                 - others ..... Other values are not allowed
  !! solution ..... Matrix with solution values
  !!                 t ... row 1 unless sol_w_t_o==.false.
  !!                 y ... rows sol_y_idx:(sol_y_idx+y_dim)
  !!                 dy .. rows (sol_y_idx+y_dim+1):(sol_y_idx+2*y_dim+1) unless sol_w_dy_o=.false.
  !! filename_o ... Filename to which we print.  Default: NONE
  !!                If not present, then output will be to output_unit (STDOUT).  
  !! digits_o ..... Number of digits for floating point numbers.  Default: 15
  !! width_o ...... Width of print format for all entities. Default: 0
  !! start_o ...... Starting index to print in solution. Default: 1
  !! end_o ........ Ending index to print in solution.  Default: Number of columns in solution.
  !! step_o ....... Print only every step_o'th value in solution. Default: 1
  !! titles_o ..... Print titles if .true.  Default: .true.
  !! separator_o .. String to place between fields.  Default: ',' if width_o missing, and ' ' otherwise.
  !! t_min_o ...... Print only solutions with time values >= t_min_o
  !! t_max_o ...... Print only solutions with time values <= t_min_o
  !! y_dim ........ Number of elements in y.  Infered from solution, sol_w_t_o, & sol_w_dy_o when sol_y_idx_o==2
  !! sol_y_idx_o .. Index of y in solution.  Default: 2
  !! sol_w_t_o .... Solution has t when .true.  Default: .true.
  !! sol_w_dy_o ... Solution has dy when .true.  Default: .true.
  !! @endverbatim
  !! 
  subroutine print_solution(status, solution, filename_o, separator_o, digits_o, width_o, start_o, end_o, step_o, titles_o, &
                            t_min_o, t_max_o, y_dim_o, sol_y_idx_o, sol_w_t_o, sol_w_dy_o)
    use, intrinsic :: iso_fortran_env, only: output_unit
    use            :: mrkiss_config,   only: rk, ik, bk
    implicit none
    ! Arguments
    integer(kind=ik), intent(out)          :: status
    real(kind=rk),    intent(in)           :: solution(:,:)
    character(len=*), intent(in), optional :: filename_o, separator_o
    integer(kind=ik), intent(in), optional :: digits_o, width_o, start_o, end_o, step_o
    real(kind=rk),    intent(in), optional :: t_min_o, t_max_o
    integer(kind=ik), intent(in), optional :: y_dim_o, sol_y_idx_o
    logical(kind=bk), intent(in), optional :: sol_w_t_o, sol_w_dy_o, titles_o
    ! Local variables
    integer(kind=ik)                       :: digits, width, start_idx, end_idx, step, y_dim, sol_y_idx
    logical(kind=bk)                       :: sol_w_t, sol_w_dy, titles
    integer(kind=ik)                       :: i, out_io_stat, out_io_unit
    character(len=:), allocatable          :: fmt, separator
    character(len=512)                     :: digits_str, width_str, tmp_str
    ! Process arguments
    titles = .true.
    if (present(titles_o)) titles = titles_o
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
    sol_y_idx = 2
    if (present(sol_y_idx_o)) sol_y_idx = sol_y_idx_o
    sol_w_t = .true._bk
    if (present(sol_w_t_o)) sol_w_t = sol_w_t_o
    sol_w_dy = .true._bk
    if (present(sol_w_dy_o)) sol_w_dy = sol_w_dy_o
    if (present(y_dim_o)) then
       y_dim = y_dim_o
    else ! If solution contains exactily one solution set with no extra space and sol_y_idx_o==1, then we can guess y_dim
       y_dim = size(solution, 1)
       if (sol_w_t)  y_dim = y_dim - 1
       if (sol_w_dy) y_dim = y_dim / 2
    end if
    ! Create string from for format bits
    write (digits_str, '(i0)') digits
    write (width_str,  '(i0)') width
    ! Open file
    if (present(filename_o)) then
       open(newunit=out_io_unit, file=filename_o, form='formatted', action='write', iostat=out_io_stat)
       if (out_io_stat /= 0) then
          status = 1152
          return
       end if
    else
       out_io_unit = output_unit
    end if
    ! Print titles
    if (titles) then
       if (present(width_o)) then
          write(out_io_unit, fmt='(a' // trim(width_str) // ')', advance="no") "i"
          if (sol_w_t) then
             write(out_io_unit, fmt='("' // separator // '",a' // trim(width_str) // ')', advance="no") "t" 
          end if
          do i=1,y_dim
             write (tmp_str, '("y",i0)') i
             write(out_io_unit, fmt='("' // separator // '",a' // trim(width_str) // ')', advance="no") trim(tmp_str)
          end do
       if (sol_w_dy) then
          do i=1,y_dim
             write (tmp_str, '("dy",i0)') i
             write(out_io_unit, fmt='("' // separator // '",a' // trim(width_str) // ')', advance="no") trim(tmp_str)
          end do
       end if
       else
          write(out_io_unit, fmt='(a)', advance="no") "i"
          if (sol_w_t) then
             write(out_io_unit, fmt='("' // separator // '",a)', advance="no") "t" 
          end if
          do i=1,y_dim
             write(out_io_unit, fmt='("' // separator // '","y",i0)', advance="no") i
          end do
          if (sol_w_dy) then
             do i=1,y_dim
                write(out_io_unit, fmt='("' // separator // '","dy",i0)', advance="no") i
             end do
          end if
       end if
       write(out_io_unit, fmt='()')
    end if
    i = y_dim
    if (sol_w_t)  i = i + 1
    if (sol_w_dy) i = i + y_dim
    fmt = ('(i' // trim(width_str) // repeat(',"' // separator // '",' // 'f' // trim(width_str) // '.' // trim(digits_str), i) // ')')
    do i=start_idx,end_idx,step
       if (present(t_min_o)) then
          if (solution(1, i) < t_min_o) cycle
       end if
       if (present(t_max_o)) then
          if (solution(1, i) > t_max_o) cycle
       end if
       if (sol_w_t) then
          if (sol_w_dy) then
             write (out_io_unit, fmt=fmt) i, solution(1, i), solution(sol_y_idx:(sol_y_idx+2*y_dim-1), i)
          else
             write (out_io_unit, fmt=fmt) i, solution(1, i), solution(sol_y_idx:(sol_y_idx+y_dim-1), i)
          end if
       else
          if (sol_w_dy) then
             write (out_io_unit, fmt=fmt) i, solution(sol_y_idx:(sol_y_idx+2*y_dim-1), i)
          else
             write (out_io_unit, fmt=fmt) i, solution(sol_y_idx:(sol_y_idx+y_dim-1), i)
          end if
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
  !> Analyze an RK solution matrix.
  !! 
  !! @verbatim
  !! status .............. Exit status
  !!                        - -inf-0 ..... Everything worked
  !!                        - 1297-1313 .. Error in this routine
  !!                                        - 1297 .. Could not open file for write
  !!                                        - 1313 .. Could not close file         
  !!                        - others ..... Other values are not allowed
  !! solution ............ Matrix with solution values
  !! y_dim_o ............. Number of elements in y
  !! start_o ............. Starting index to print in solution. Default: 1
  !! end_o ............... Ending index to print in solution.  Default: Number of columns in solution.
  !! sol_y_idx_o ......... Index of y in solution.  Default: 2
  !! sol_w_t_o ........... When .false. solution has no t values
  !! sol_w_dy_o .......... When .false. solution has no dy values
  !! y_delta_len_idxs_o .. Components of y_delta to use for y_delta length computation
  !! filename_o .......... Filename to which we print.  Default: NONE
  !!                       If not present, then output will be to output_unit (STDOUT).  
  !! @endverbatim
  !! 
  subroutine analyze_solution(status, solution, filename_o, y_dim_o, start_o, end_o, sol_y_idx_o, sol_w_t_o, sol_w_dy_o, y_delta_len_idxs_o)
    use, intrinsic :: iso_fortran_env, only: output_unit
    use            :: mrkiss_config,   only: rk, ik, bk
    implicit none
    ! Arguments
    integer(kind=ik), intent(out)          :: status
    real(kind=rk),    intent(in)           :: solution(:,:)
    character(len=*), intent(in), optional :: filename_o
    integer(kind=ik), intent(in), optional :: y_dim_o, sol_y_idx_o, y_delta_len_idxs_o(:), start_o, end_o
    logical(kind=bk), intent(in), optional :: sol_w_t_o, sol_w_dy_o
    ! Local variables
    integer(kind=ik)                       :: y_dim, sol_y_idx, end_idx, start_idx
    integer(kind=ik)                       :: i, out_io_stat, out_io_unit
    logical(kind=bk)                        :: sol_w_t, sol_w_dy
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
    sol_w_t = .true._bk
    if (present(sol_w_t_o)) sol_w_t = sol_w_t_o
    sol_w_dy = .true._bk
    if (present(sol_w_dy_o)) sol_w_dy = sol_w_dy_o
    if (present(y_dim_o)) then
       y_dim = y_dim_o
    else ! If solution contains exactily one solution set with no extra columns and sol_y_idx_o==1, then we can guess y_dim
       y_dim = size(solution, 1)
       if (sol_w_t)  y_dim = y_dim - 1
       if (sol_w_dy) y_dim = y_dim / 2
    end if
    ! Compute 
    if(end_idx - start_idx > 1) then
       if (sol_w_t) then
          t_delta_max =  abs(solution(1, start_idx+1) - solution(1, start_idx))
       else
          t_delta_max = 0.0_rk
       end if
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
    if (sol_w_dy) then
       dy_max = solution((sol_y_idx+y_dim):(sol_y_idx+2*y_dim-1), start_idx)
    else
       dy_max = 0.0_rk
    end if
    dy_min = dy_max
    if (sol_w_t) then
       t_max =  solution(1, start_idx)
    else
       t_max = 0.0_rk
    end if
    t_min =  t_max
    do i=(start_idx+1),end_idx
       y_max =  max(y_max, solution(sol_y_idx:(sol_y_idx+y_dim-1), i))
       y_min =  min(y_min, solution(sol_y_idx:(sol_y_idx+y_dim-1), i))
       if (sol_w_dy) then
          dy_max = max(dy_max, solution((sol_y_idx+y_dim):(sol_y_idx+2*y_dim-1), i))
          dy_min = min(dy_min, solution((sol_y_idx+y_dim):(sol_y_idx+2*y_dim-1), i))
       end if
       if (sol_w_t) then
          t_max =  max(t_max, solution(1, i))
          t_min =  min(t_min, solution(1, i))
          t_delta_max =  max(t_delta_max, abs(solution(1, i) - solution(1, i-1)))
          t_delta_min =  min(t_delta_min, abs(solution(1, i) - solution(1, i-1)))
       end if
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

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Produce a sequence of values with fixed seporation.  Modeled after R's seq() function.
  !!
  !! @verbatim
  !! status .............. Exit status
  !!                        - -inf-0 ..... Everything worked
  !!                        - 1314:1330 .. Error in this routine
  !!                          - 1314 .. Inconsistant sequence values: step_v_o * (size(t)-1) /= to_v - from_v
  !! y_at_t .............. The value of the function at t
  !! t ................... Vector to fill
  !! from_o .............. Starting value
  !! to_o ................ Ending value
  !! step_o .............. Delta between valeus
  !! @endverbatim
  !! 
  subroutine seq(status, t, from_o, to_o, step_o)
    use :: mrkiss_config, only: rk, ik
    implicit none
    ! Arguments
    integer(kind=ik), intent(out) :: status
    real(kind=rk),    intent(out) :: t(:)
    real(kind=rk), optional, intent(in)  :: from_o, to_o, step_o
    ! Variables
    integer(kind=ik)              :: n_v, i
    real(kind=rk)                 :: from_v, to_v, step_v
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
       if (abs((step_v * n_v) - (to_v - from_v)) > 1.0e-10_rk) then  !! TODO: Use epsilon here
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

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Interpolate an old solution to a new solution with a different time series.
  !! 
  !! @verbatim
  !! status ...................... Exit status
  !!                                - -inf-0 ..... Everything worked
  !!                                - 1331:1347 .. Error in this routine
  !!                                   - 1331 ...... new_solution t value out of bounds
  !!                                - others ..... Other values are not allowed
  !! new_solution(:,:) ........... Array for new solution.  
  !!                                This array *must* have a populated t sequence in new_solution(1,:)
  !!                                It must also have room for a new y solution series starting in at 
  !!                                column new_sol_y_idx_o, 2 by default.
  !! old_solution(:,:) ........... Array for old solution.  
  !!                                This array *must* have t!  It must have at least two solution points.
  !! y_dim ....................... Number of elements in y.  Default infered from size of old_solution.
  !! new_sol_y_idx_o ............. Index of y in new_solution.  Default: 2
  !! old_sol_y_idx_o ............. Index of y in old_solution.  Default: 2
  !! end_o ....................... The number of solutions in old_solution.  Default infered from size of old_solution.
  !! sol_w_dy_o .................. When .false. old_solution has no dy data.
  !!                               If dy data is present then Hermite interpolation is used.  
  !!                               Without dy, linear interpolation is used.
  !! @endverbatim
  !!
  subroutine interpolate_solution(status, new_solution, old_solution, new_sol_y_idx_o, old_sol_y_idx_o, y_dim_o, end_o, sol_w_dy_o)
    use :: mrkiss_config, only: rk, ik, bk
    implicit none
    ! Arguments
    integer(kind=ik),           intent(out)   :: status
    real(kind=rk),              intent(inout) :: new_solution(:,:)
    real(kind=rk),              intent(out)   :: old_solution(:,:)
    integer(kind=ik), optional, intent(in)    :: new_sol_y_idx_o, old_sol_y_idx_o, y_dim_o, end_o
    logical(kind=bk), optional, intent(in)    :: sol_w_dy_o
    ! Variables
    integer(kind=ik)                          :: new_sol_idx, old_sol_idx, max_idx, new_sol_y_idx, old_sol_y_idx, old_end, y_dim
    logical(kind=bk)                          :: sol_w_dy
    real(kind=rk)                             :: t, t0, t1, tu, td
    real(kind=rk), allocatable                :: y0(:), y1(:), dy0(:), dy1(:), yat(:)
    ! Process Arguments
    old_end = size(old_solution, 2)
    if (present(end_o)) old_end = min(end_o, size(old_solution, 2))
    new_sol_y_idx = 2
    if (present(new_sol_y_idx_o)) new_sol_y_idx = new_sol_y_idx_o
    old_sol_y_idx = 2
    if (present(old_sol_y_idx_o)) old_sol_y_idx = old_sol_y_idx_o
    sol_w_dy = .true._bk
    if (present(sol_w_dy_o)) sol_w_dy = sol_w_dy_o
    if (present(y_dim_o)) then
       y_dim = y_dim_o
    else ! The old_solution *must* have both t
       y_dim = size(old_solution, 1)
       y_dim = y_dim - 1
       if (sol_w_dy) y_dim = y_dim / 2
    end if
    ! Compute value
    max_idx = size(new_solution, 2)
    old_sol_idx = 2
    do new_sol_idx=1, max_idx
       t = new_solution(1, new_sol_idx)
       do while (t > old_solution(1, old_sol_idx))
          old_sol_idx = old_sol_idx + 1
          if (old_sol_idx > old_end) then
             status = 1331
             return
          end if
       end do
       ! If we get here, we are in the interval we want
       t0 = old_solution(1, old_sol_idx-1)
       t1 = old_solution(1, old_sol_idx)
       y0 = old_solution(old_sol_y_idx:(old_sol_y_idx+y_dim-1), old_sol_idx-1)
       y1 = old_solution(old_sol_y_idx:(old_sol_y_idx+y_dim-1), old_sol_idx)
       td = (t1 - t0)
       if (sol_w_dy) then 
          dy0 = td * old_solution((old_sol_y_idx+y_dim):(old_sol_y_idx+2*y_dim-1), old_sol_idx-1)
          dy1 = td * old_solution((old_sol_y_idx+y_dim):(old_sol_y_idx+2*y_dim-1), old_sol_idx)
          tu  = (t - t0) / td
          yat = tu * (tu * (tu * (2 * y0 + dy0 - 2 * y1 + dy1) - 2 * dy0 - 3 * y0 + 3 * y1 - dy1) + dy0) + y0
       else
          yat = (y0 * (t1 - t) + y1 * (t - t0)) / td
       end if
       new_solution(new_sol_y_idx:(new_sol_y_idx+y_dim-1), new_sol_idx) = yat
    end do
    status = 0;
  end subroutine interpolate_solution
  
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
  !! - 1365-1381 ... Unallocated
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
  !! status_to_origin(len=64) .... A string identifying the origin of the status code.
  !!                                - NO ERROR .. Non-error status of unknown origin
  !!                                - ERROR ..... Error status of unknown origin
  !!                                - Every other return is a known interface or subroutine 
  !! @endverbatim
  !!
  character(len=64) function status_to_origin(status)
    use :: mrkiss_config, only: rk, ik
    implicit none
    ! Arguments
    integer(kind=ik), intent(in) :: status
    ! Process Input
    if     (status  <=    0) then
       status_to_origin = "NO ERROR"
    elseif ((status >=    1) .and. (status <=  255)) then
       status_to_origin = "interface  deq_iface_*t"
    elseif ((status >=  256) .and. (status <=  511)) then
       status_to_origin = "interface  stepp_iface_*t"
    elseif ((status >=  512) .and. (status <=  767)) then
       status_to_origin = "interface  sdf_iface_*t"
    elseif ((status >= 1232) .and. (status <= 1247)) then
       status_to_origin = "subroutine one_step_etab_*t"
    elseif ((status >= 1248) .and. (status <= 1263)) then
       status_to_origin = "subroutine one_step_stab_*t"
    elseif ((status >= 1216) .and. (status <= 1231)) then
       status_to_origin = "subroutine one_richardson_step_stab_*t"
    elseif ((status >= 1200) .and. (status <= 1215)) then
       status_to_origin = "subroutine one_step_rk4_*t"
    elseif ((status >= 1184) .and. (status <= 1199)) then
       status_to_origin = "subroutine one_step_rkf45_*t"
    elseif ((status >= 1263) .and. (status <= 1279)) then
       status_to_origin = "subroutine one_step_dp54_*t"
    elseif ((status >= 1120) .and. (status <= 1151)) then
       status_to_origin = "subroutine steps_fixed_stab_*t"
    elseif ((status >= 1024) .and. (status <= 1055)) then
       status_to_origin = "subroutine steps_condy_stab_*t"
    elseif ((status >= 1280) .and. (status <= 1296)) then
       status_to_origin = "subroutine steps_sloppy_condy_stab_*t"
    elseif ((status >= 1056) .and. (status <= 1119)) then
       status_to_origin = "subroutine steps_adapt_etab_*t"
    elseif ((status >= 1152) .and. (status <= 1183)) then
       status_to_origin = "subroutine print_solution"
    elseif ((status >= 1297) .and. (status <= 1313)) then
       status_to_origin = "subroutine analyze_solution"
    elseif ((status >= 1314) .and. (status <= 1330)) then
       status_to_origin = "subroutine seq"
    elseif ((status >= 1331) .and. (status <= 1347)) then
       status_to_origin = "subroutine interpolate_solution"
    elseif ((status >= 1348) .and. (status <= 1364)) then
       status_to_origin = "subroutine steps_points_stab_*t"
    else
       status_to_origin = "ERROR"
    end if
  end function status_to_origin

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return, as a string, the source of a status code.
  !! 
  !! @verbatim
  !! status ...................... This is an intent(in) argument!!!!
  !! status_to_message(len=128) .. A string identifying the message of the status code.
  !!                                - NO ERROR .. Non-error status of unknown message
  !!                                - ERROR ..... Error status of unknown message
  !!                                - Every other return is a known error message
  !! @endverbatim
  !!
  character(len=128) function status_to_message(status)
    use :: mrkiss_config, only: rk, ik
    implicit none
    ! Arguments
    integer(kind=ik), intent(in) :: status
    ! Process Input
    if (status == 1024) then
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
    else
       status_to_message = status_to_origin(status)
    end if
  end function status_to_message

end module mrkiss_utils





