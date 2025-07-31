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

  public :: print_solution

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
  !! solution ...... Matrix with solution values
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
  !! 
  subroutine print_solution(status, solution, filename_o, separator_o, digits_o, width_o, start_o, end_o, step_o, no_titles_o, &
                           t_min_o, t_max_o)
    use, intrinsic :: iso_fortran_env, only: output_unit
    use            :: mrkiss_config,   only: rk, ik
    implicit none
    ! Arguments
    integer(kind=ik), intent(out)          :: status
    real(kind=rk),    intent(in)           :: solution(:,:)
    character(len=*), intent(in), optional :: filename_o, separator_o
    integer(kind=ik), intent(in), optional :: digits_o, width_o, start_o, end_o, step_o, no_titles_o
    real(kind=rk),    intent(in), optional :: t_min_o, t_max_o
    ! Local variables
    integer(kind=ik)                       :: digits, width, start_idx, end_idx, step
    character(len=:), allocatable          :: fmt, separator
    character(len=512)                     :: digits_str, width_str, tmp_str
    integer                                :: i, out_io_stat, out_io_unit
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
          write(out_io_unit, fmt='(a' // width_str // ')', advance="no") "i"
          write(out_io_unit, fmt='("' // separator // '",a' // width_str // ')', advance="no") "t" 
          do i=1,(size(solution, 1)-1)
             write (tmp_str, '("y",i0)') i
             write(out_io_unit, fmt='("' // separator // '",a' // width_str // ')', advance="no") trim(tmp_str)
          end do
       else
          write(out_io_unit, fmt='(a)', advance="no") "i"
          write(out_io_unit, fmt='("' // separator // '",a)', advance="no") "t" 
          do i=1,(size(solution, 1)-1)
             write(out_io_unit, fmt='("' // separator // '","y",i0)', advance="no") i
          end do
       end if
       write(out_io_unit, fmt='()')
    end if
    ! Create numeric line print format
    fmt = '(i' // width_str // ',"' // separator // '",' // repeat('f' // width_str // '.' // trim(digits_str) &
          // ',"' // separator // '"', size(solution, 1)-1) // ',f' // width_str // '.' // trim(digits_str) // ')'
    ! Print numbers
    do i=start_idx,end_idx,step
       if (present(t_min_o)) then
          if (solution(1, i) < t_min_o) cycle
       end if
       if (present(t_max_o)) then
          if (solution(1, i) > t_max_o) cycle
       end if
       write (out_io_unit, fmt=fmt) i, solution(:,i)
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

end module mrkiss_utils
