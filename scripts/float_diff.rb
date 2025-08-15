#!/usr/bin/env -S ruby
# -*- Mode:ruby; Coding:us-ascii; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      float_diff.rb
# @author    Mitch Richling http://www.mitchr.me/
# @brief     File diff comprehending floating point numbers.@EOL
# @std       Ruby 3
# @see       https://github.com/richmit/codeBits/
# @copyright 
#  @parblock
#  Copyright (c) 2024, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#
#  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
#
#  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#
#  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without
#     specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
#  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
#  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
#  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#  @endparblock
# @filedetails
#
#  Super simple file diff that ignores small differences in floating opts.separator point values.  The regex for floating point values is pretty specific to
#  what my software prints out, so be careful if pressing this script into service for other use cases.
#
#########################################################################################################################################################.H.E.##

require 'optparse'

epsilon        = 1.0e-5
brief          = false
identical      = false
ignore_ws_end  = false
ignore_ws_cnt  = false
ignore_ws_all  = false
dump           = false
ignore_non_num = false
pnames         = false
lines_re       = Regexp.new('.*')
gsubs          = Array.new
opts = OptionParser.new do |opts|
  opts.banner = "Usage: float_diff.rb [options] file1 file2"
  opts.separator ""
  opts.separator "Options:"
  opts.separator "  Output Options:"
  opts.on("-h",           "--help",                   "Show this message")                           { puts opts; exit 1;              }
  opts.on("-D",           "--dump-diffs",             "Print string differences with .dump")         { |v| dump=true;                  }
  opts.on("-q",           "--brief",                  "Print just first difference")                 { |v| brief=true;                 }
  opts.on("-n",           "--names",                  "Include file names in output")                { |v| pnames=true;                }
  opts.separator "  Content Options:"                                                                                                  
  opts.on("-e epsilon",   "--epsilon epsilon",        "Set floating point epsilon")                  { |v| epsilon=v.to_f;             }
  opts.on("-s",           "--report-identical-files", "Report identical files")                      { |v| identical=true;             }
  opts.on("-Z",           "--ignore-trailing-space",  "ignore white space at line end")              { |v| ignore_ws_end=true;         }
  opts.on("-b",           "--ignore-space-change",    "ignore changes in the amount of white space") { |v| ignore_ws_cnt=true;         }
  opts.on("-w",           "--ignore-all-space",       "ignore all white space")                      { |v| ignore_ws_all=true;         }
  opts.on("-a",           "--ignore-non-numeric",     "ignore non-numeric differences")              { |v| ignore_non_num=true;        }
  opts.separator "  Filter Options:"
  opts.separator "    Filtering occurs upon file read just as if the files had been prepossessed."
  opts.separator "    Line numbers in output refer to the line numbers in the filtered result."
  opts.on("-k regexp",    "--keep-lines regexp",      "Filter out non-matching lines")               { |v| lines_re=Regexp.new(v);     }
  opts.on("-g #/re/to/",  "--gsub #/re/to/",          "Replace re with 'to' in file #")              { |v| gsubs.push(v.split(v[-1])); }
  opts.separator "                                     # must be 0 (both files), 1, or 2."
  opts.separator "                                     Any character can be used for '/'."
  opts.separator "                                     May be repeated with substitutions "
  opts.separator "                                     occurring in the order given."
  opts.separator ""
  opts.separator "Super simple file diff that ignores small differences in floating"
  opts.separator "point values.  A non-zero exit code is returned if the files are"
  opts.separator "different or an error occurs."
  opts.separator ""
end
opts.parse!(ARGV)

if (ARGV.length != 2) then
  puts("ERROR: Two, and no more, files must be provided as arguments!")
  exit 2
end

msgPfx = 'Files'
if (pnames) then
  msgPfx = "#{ARGV[0]} & #{ARGV[1]}"
end

gsubs.map! do |num, from, to| 
  [num.to_i, Regexp.new(from), to]
end

file_lines = ARGV.map.with_index do |fname, fnum|
  open(fname, "r") do |file|
    tmp = file.readlines()
    tmp.select! { |line| line.match(lines_re) };
    if ( !(gsubs.empty?)) then
      tmp.each do |line| 
        gsubs.each do |num, from, to| 
          if ((num==0) || ((num-1)==fnum)) then
            line.gsub!(from, to)
          end
        end
      end
    end
    tmp
  end
end

if (file_lines[0].length != file_lines[1].length) then
  puts("#{msgPfx} have different line counts");
  exit 5
end

fpre = Regexp.new(/[+-]?((\d+\.\d*|\d*\.\d+)([eE][+-]?\d+)?|\d+[eE][+-]?\d+)/)

num_diffs = 0
file_lines[0].each_index do |idx|
  line_num = idx + 1
  line_floats = [0, 1].map { |i| file_lines[i][idx].scan(fpre).map { |m| m[0].to_f } }
  if (line_floats[0].size != line_floats[1].size) then
    puts("#{msgPfx} have different float counts on line #{line_num}");
    puts("  <<<#{file_lines[0][idx]}")
    puts("  >>>#{file_lines[1][idx]}")
    exit 6 if brief
    num_diffs += 1;
  elsif (line_floats[0].zip(line_floats[1]).any? { |f0, f1| (f0-f1).abs>=epsilon }) then
    puts("#{msgPfx} have different float values on line #{line_num}");
    puts("  <<<#{file_lines[0][idx]}")
    puts("  >>>#{file_lines[1][idx]}")
    exit 7 if brief
    num_diffs += 1;
  elsif ( !(ignore_non_num)) then
    cmp_strs = [ file_lines[0][idx], file_lines[1][idx] ]
    cmp_strs = cmp_strs.map { |v| v.gsub(fpre, '')    }
    cmp_strs = cmp_strs.map { |v| v.gsub(/\s+/, ' ') } if ignore_ws_cnt
    cmp_strs = cmp_strs.map { |v| v.gsub(/\s+$/, '') } if ignore_ws_end
    cmp_strs = cmp_strs.map { |v| v.gsub(/\s+/,  '') } if ignore_ws_all
    if (cmp_strs[0] != cmp_strs[1]) then
      puts("#{msgPfx} have different non-float content on line #{line_num}");
      if (dump) then
        puts("  <<<#{file_lines[0][idx].dump}")
        puts("  >>>#{file_lines[1][idx].dump}")
      else
        puts("  <<<#{file_lines[0][idx]}")
        puts("  >>>#{file_lines[1][idx]}")
      end
      exit 8 if brief
      num_diffs += 1;
    end
  end
end

if (num_diffs > 0) then
  exit 9
else
  puts("#{msgPfx} are identical!") if identical
  exit 0
end
