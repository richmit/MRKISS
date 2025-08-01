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

contains
  
  !--------------------------------------------------------------------------------------------------------------------------------
  !> Output an RK solution matrix.
  !! 
  !! Inappropriate width_o, digits_o, and separator_o values may cause a runtime error.
  !!
  !! Example 1: The default values will produce CSV output
  !!   print_solution(solution)
  !! Example 2: For columnar output we just need to add a width
  !!   print_solution(solution, width_o=30)
  !! 
  !! status ....... Exit status
  !!                 - -inf-0 ..... Everything worked
  !!                 - 1152-1183 .. Error in this routine
  !!                                 - 1152 .. Could not open file for write
  !!                                 - 1153 .. Could not close file         
  !!                 - others ..... Other values are not allowed
  !! solution ..... Matrix with solution values
  !!                 t ... row 1 unless sol_no_t_o
  !!                 y ... rows sol_y_idx:(sol_y_idx+y_dim)
  !!                 dy .. rows (sol_y_idx+y_dim+1):(sol_y_idx+2*y_dim+1) unless sol_no_dy_o
  !! filename_o ... Filename to which we print.  Default: NONE
  !!                If not present, then output will be to output_unit (STDOUT).  
  !! digits_o ..... Number of digits for floating point numbers.  Default: 15
  !! width_o ...... Width of print format for all entities. Default: 0
  !! start_o ...... Starting index to print in solution. Default: 1
  !! end_o ........ Ending index to print in solution.  Default: Number of columns in solution.
  !! step_o ....... Print only every step_o'th value in solution. Default: 1
  !! no_titles_o .. If present, don't print titles.
  !! separator_o .. String to place between fields.  Default: ',' if width_o missing, and ' ' otherwise.
  !! t_min_o ...... Print only solutions with time values >= t_min_o
  !! t_max_o ...... Print only solutions with time values <= t_min_o
  !! y_dim ........ Number of elements in y.  Infered from solution, sol_no_t_o, & sol_no_dy_o when sol_y_idx_o==1
  !! sol_y_idx_o .. Index of y in solution.  Default: 2
  !! sol_no_t_o ... When present means solution has no t values
  !! sol_no_dy_o .. When present means solution has no dy values
  !! 
  subroutine print_solution(status, solution, filename_o, separator_o, digits_o, width_o, start_o, end_o, step_o, no_titles_o, &
                            t_min_o, t_max_o, y_dim_o, sol_y_idx_o, sol_no_t_o, sol_no_dy_o)
    use, intrinsic :: iso_fortran_env, only: output_unit
    use            :: mrkiss_config,   only: rk, ik
    implicit none
    ! Arguments
    integer(kind=ik), intent(out)          :: status
    real(kind=rk),    intent(in)           :: solution(:,:)
    character(len=*), intent(in), optional :: filename_o, separator_o
    integer(kind=ik), intent(in), optional :: digits_o, width_o, start_o, end_o, step_o, no_titles_o
    real(kind=rk),    intent(in), optional :: t_min_o, t_max_o
    integer(kind=ik), intent(in), optional :: y_dim_o, sol_y_idx_o, sol_no_t_o, sol_no_dy_o
    ! Local variables
    integer(kind=ik)                       :: digits, width, start_idx, end_idx, step, y_dim, sol_y_idx
    integer(kind=ik)                       :: i, out_io_stat, out_io_unit
    character(len=:), allocatable          :: fmt, separator
    character(len=512)                     :: digits_str, width_str, tmp_str
    ! Process arguments
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
    if (present(y_dim_o)) then
       y_dim = y_dim_o
    else ! If solution contains exactily one solution set with no extra space and sol_y_idx_o==1, then we can guess y_dim
       y_dim = size(solution, 1)
       if ( .not. (present(sol_no_t_o)))  y_dim = y_dim - 1
       if ( .not. (present(sol_no_dy_o))) y_dim = y_dim / 2
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
    if ( .not. (present(no_titles_o))) then
       if (present(width_o)) then
          write(out_io_unit, fmt='(a' // trim(width_str) // ')', advance="no") "i"
          if ( .not. (present(sol_no_t_o))) then
             write(out_io_unit, fmt='("' // separator // '",a' // trim(width_str) // ')', advance="no") "t" 
          end if
          do i=1,y_dim
             write (tmp_str, '("y",i0)') i
             write(out_io_unit, fmt='("' // separator // '",a' // trim(width_str) // ')', advance="no") trim(tmp_str)
          end do
       if ( .not. (present(sol_no_dy_o))) then
          do i=1,y_dim
             write (tmp_str, '("dy",i0)') i
             write(out_io_unit, fmt='("' // separator // '",a' // trim(width_str) // ')', advance="no") trim(tmp_str)
          end do
       end if
       else
          write(out_io_unit, fmt='(a)', advance="no") "i"
          if ( .not. (present(sol_no_t_o))) then
             write(out_io_unit, fmt='("' // separator // '",a)', advance="no") "t" 
          end if
          do i=1,y_dim
             write(out_io_unit, fmt='("' // separator // '","y",i0)', advance="no") i
          end do
          if ( .not. (present(sol_no_dy_o))) then
             do i=1,y_dim
                write(out_io_unit, fmt='("' // separator // '","dy",i0)', advance="no") i
             end do
          end if
       end if
       write(out_io_unit, fmt='()')
    end if
    i = y_dim
    if ( .not. (present(sol_no_t_o)))  i = i + 1
    if ( .not. (present(sol_no_dy_o))) i = i + y_dim
    fmt = ('(i' // trim(width_str) // repeat(',"' // separator // '",' // 'f' // trim(width_str) // '.' // trim(digits_str), i) // ')')
    do i=start_idx,end_idx,step
       if (present(t_min_o)) then
          if (solution(1, i) < t_min_o) cycle
       end if
       if (present(t_max_o)) then
          if (solution(1, i) > t_max_o) cycle
       end if
       if (present(sol_no_t_o)) then
          if (present(sol_no_dy_o)) then
             write (out_io_unit, fmt=fmt) i, solution(sol_y_idx:(sol_y_idx+y_dim-1), i)
          else
             write (out_io_unit, fmt=fmt) i, solution(sol_y_idx:(sol_y_idx+2*y_dim-1), i)
          end if
       else
          if (present(sol_no_dy_o)) then
             write (out_io_unit, fmt=fmt) i, solution(1, i), solution(sol_y_idx:(sol_y_idx+y_dim-1), i)
          else
             write (out_io_unit, fmt=fmt) i, solution(1, i), solution(sol_y_idx:(sol_y_idx+2*y_dim-1), i)
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
  !! status .............. Exit status
  !!                        - -inf-0 ..... Everything worked
  !!                        - ????-???? .. Error in this routine
  !!                                        - ???? .. Could not open file for write
  !!                                        - ???? .. Could not close file         
  !!                        - others ..... Other values are not allowed
  !! solution ............ Matrix with solution values
  !! y_dim_o ............. Number of elements in y
  !! start_o ............. Starting index to print in solution. Default: 1
  !! end_o ............... Ending index to print in solution.  Default: Number of columns in solution.
  !! sol_y_idx_o ......... Index of y in solution.  Default: 2
  !! sol_no_t_o .......... solution has no t values
  !! sol_no_dy_o ......... solution has no dy values
  !! y_delta_len_idxs_o .. Components of y_delta to use for y_delta length computation
  !! filename_o .......... Filename to which we print.  Default: NONE
  !!                       If not present, then output will be to output_unit (STDOUT).  
  !! 
  subroutine analyze_solution(status, solution, filename_o, y_dim_o, start_o, end_o, sol_y_idx_o, sol_no_t_o, sol_no_dy_o, y_delta_len_idxs_o)
    use, intrinsic :: iso_fortran_env, only: output_unit
    use            :: mrkiss_config,   only: rk, ik
    implicit none
    ! Arguments
    integer(kind=ik), intent(out)          :: status
    real(kind=rk),    intent(in)           :: solution(:,:)
    character(len=*), intent(in), optional :: filename_o
    integer(kind=ik), intent(in), optional :: y_dim_o, sol_y_idx_o, sol_no_t_o, sol_no_dy_o, y_delta_len_idxs_o(:), start_o, end_o
    ! Local variables
    integer(kind=ik)                       :: y_dim, sol_y_idx, end_idx, start_idx
    integer(kind=ik)                       :: i, out_io_stat, out_io_unit
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
       if ( .not. (present(sol_no_t_o)))  y_dim = y_dim - 1
       if ( .not. (present(sol_no_dy_o))) y_dim = y_dim / 2
    end if
    ! Compute 
    if(end_idx - start_idx > 1) then
       if (present(sol_no_t_o)) then
          t_delta_max = 0.0_rk
       else
          t_delta_max =  abs(solution(1, start_idx+1) - solution(1, start_idx))
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
    if (present(sol_no_dy_o)) then
       dy_max = 0.0_rk
    else
       dy_max = solution((sol_y_idx+y_dim):(sol_y_idx+2*y_dim-1), start_idx)
    end if
    dy_min = dy_max
    if ( .not. (present(sol_no_t_o))) then
       t_max =  solution(1, start_idx)
    else
       t_max = 0.0_rk
    end if
    t_min =  t_max
    do i=(start_idx+1),end_idx
       y_max =  max(y_max, solution(sol_y_idx:(sol_y_idx+y_dim-1), i))
       y_min =  min(y_min, solution(sol_y_idx:(sol_y_idx+y_dim-1), i))
       if ( .not. (present(sol_no_dy_o))) then
          dy_max = max(dy_max, solution((sol_y_idx+y_dim):(sol_y_idx+2*y_dim-1), i))
          dy_min = min(dy_min, solution((sol_y_idx+y_dim):(sol_y_idx+2*y_dim-1), i))
       end if
       if ( .not. (present(sol_no_t_o))) then
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
          status = 1152
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
          status = 1153
          return
       end if
    end if
    status = 0
  end subroutine analyze_solution


end module mrkiss_utils
