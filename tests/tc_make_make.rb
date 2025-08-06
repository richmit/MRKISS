#!/usr/bin/env -S ruby
# -*- Mode:ruby; Coding:us-ascii; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      tc_make_make.rb
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Expand test case templates.@EOL
# @std       Ruby 3
# @see       https://github.com/richmit/MRKISS/
# @copyright 
#  @parblock
#  Copyright (c) 2025, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
#  
#  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#  
#  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
#  
#  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#  
#  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software
#     without specific prior written permission.
#  
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
#  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
#  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
#  DAMAGE.
#  @endparblock
# @filedetails
#
#  This script takes a Fortran source code template file.
#  It then finds all "test cases" embedded in that file, and
#    1) Creates new Fortran code for each test case
#    1) Genertes a makefile include with rules to build each test case
#
#  Test cases are identified by comments in the code that match this regex:
#       /TCASE_COM:\s+([^ ]+)
#  All test cases found in the file are "grouped" together based on the 
#  template file's name: GROUP_template.f90.  
#
#  New source is generated from the old source by processing the template file:
#    - The new file is named GROUP_TCASE.f90
#    - The first  exclimation point is removed from lines matching /^\s*!.*TCASE_COM:\s+[^ ]+.*/ 
#    - TCASEN is replaced by the test case name
#
#########################################################################################################################################################.H.E.##

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
require 'set'

tfile = ARGV[0]
tgroup = tfile.match(/^([^_]+)_/)[1]

template = Array.new
open(tfile, "rb") do |file|
  template = file.readlines()
end

all_tcases = Set.new
template.each do |line|
  matchDat = line.match(/TCASE_COM:\s+([^ ]+)/)
  if (matchDat) then
    all_tcases.add(matchDat[1].chomp)
  end
end

all_tcases.each do |tcase|
  open("#{tgroup}_#{tcase}.f90", "wb") do |src_file|
    template.each do |line|
      src_file.puts(line.sub(/^(\s*)!(.*TCASE_COM:\s+#{tcase})\W.*$/, '\1 \2').gsub(/TCASEN/, tcase))
    end
  end
end

open("#{tgroup}.mk", "wb") do |mk_file|
  mk_file.puts('# ' + ('-' * 200))
  mk_file.puts("# Run all tests for #{tgroup}")
  mk_file.puts(".PHONY: test_#{tgroup}")
  mk_file.puts( "test_#{tgroup} : " + (all_tcases.map { |tcase| "test_#{tgroup}_#{tcase}" }).join(' '))
  mk_file.puts
  mk_file.puts('# ' + ('-' * 200))
  mk_file.puts("# Make source code for #{tgroup}")

  mk_file.puts(".PHONY: src_#{tgroup}")
  mk_file.puts( (all_tcases.map { |tcase| "#{tgroup}_#{tcase}.f90" }).join(' ') + ' &: ' + tfile)
  mk_file.puts("\truby tc_make_make.rb #{tfile}")
  mk_file.puts
  mk_file.puts('# ' + ('-' * 200))
  mk_file.puts("# Clean source code for #{tgroup}")
  mk_file.puts(".PHONY: clean_src_#{tgroup}")
  mk_file.puts("clean_src_#{tgroup} :")
  mk_file.puts("\trm -f " + (all_tcases.map { |tcase| "#{tgroup}_#{tcase}.f90" }).join(' '))
  mk_file.puts
  mk_file.puts('# ' + ('-' * 200))
  mk_file.puts("# Clean non-source code for #{tgroup}")
  mk_file.puts(".PHONY: clean_#{tgroup}")
  mk_file.puts("clean_#{tgroup} :")
  mk_file.puts("\trm -f " + 
               (all_tcases.map { |tcase| "#{tgroup}_#{tcase}.out" }).join(' ') + ' ' +
               (all_tcases.map { |tcase| "#{tgroup}_#{tcase}" }).join(' ')     + ' ' +
               (all_tcases.map { |tcase| "#{tgroup}_#{tcase}$(EXE_SUFFIX)" }).join(' '))
  mk_file.puts
  mk_file.puts('# ' + ('-' * 200))
  mk_file.puts("# Make all the data files")
  mk_file.puts(".PHONY: data_#{tgroup}")
  mk_file.puts( "data_#{tgroup} : " + (all_tcases.map { |tcase| "#{tgroup}_#{tcase}.out" }).join(' '))
  mk_file.puts
  all_tcases.each do |tcase|
    mk_file.puts('# ' + ('-' * 200))
    mk_file.puts("#{tgroup}_#{tcase} : #{tgroup}_#{tcase}.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)")
    mk_file.puts("\t$(FC) $(FFLAGS) #{tgroup}_#{tcase}.f90 $(MRKISS_OBJ_FILES) -o $@")
    mk_file.puts
    mk_file.puts("#{tgroup}_#{tcase}.out : #{tgroup}_#{tcase}")
    mk_file.puts("\t#{tgroup}_#{tcase}$(EXE_SUFFIX)")
    mk_file.puts
    mk_file.puts(".PHONY: test_#{tcase}")
    mk_file.puts("test_#{tgroup}_#{tcase} : #{tgroup}_#{tcase}.out")
    mk_file.puts("\t$(FD) -Zdq -e 1e-10 data/#{tgroup}_#{tcase}.out #{tgroup}_#{tcase}.out")
    mk_file.puts
  end
end

