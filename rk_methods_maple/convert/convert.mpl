
# Convert a tableau into rational form for Fortran and C
convert_rk_rat := proc(mname, pin)
  uses LinearAlgebra, ArrayTools, FileTools:
  local a, b, c, ad, bd, cd;
  if not(IsDirectory("../tableaux/")) then
    error "Could not find ../tableaux/ directory"
  end if:
  a:=ImportMatrix(cat("../tableaux/", mname, "-a.txt")):
  if pin then
    print(cat(mname, " A")):
    print(a):  
  end if:
  c:=ImportMatrix(cat("../tableaux/", mname, "-c.txt")):
  if pin then
    print(cat(mname, " C")):
    print(c):  
  end if:
  b:=ImportMatrix(cat("../tableaux/", mname, "-b.txt")):
  if pin then
    print(cat(mname, " B")):
    print(b): 
  end if:
  print(cat(mname, " STAGES")):
  print(RowDimension(a)): 
  print(cat(mname, " METHOD COUNT")):
  print(ColumnDimension(b)):
  print(cat(mname, " AI")):
  ad := apply(lcm, op(map(denom, convert(a, list)))):
  print(ad):  
  print(convert(LinearAlgebra:-Transpose(a)*ad, list)):
  print(cat(mname, " CI")):
  cd := apply(lcm, op(map(denom, convert(c, list)))):
  print(cd):
  print(convert((c*cd), list)):
  print(cat(mname, " BI")):
  bd := apply(lcm, op(map(denom, convert(b, list)))):
  print(bd):
  print(convert((b*bd), list)):
  NULL:
end proc:


# Convert a tableau into floating form for Fortran and C
convert_rk_float := proc(mname, digits, pin)
  uses LinearAlgebra, ArrayTools, FileTools:
  local a, b, c, oldDigits, oldDisplayprecision;
  if not(IsDirectory("../tableaux/")) then
    error "Could not find ../tableaux/ directory"
  end if:
  # Setup floating point
  oldDigits := Digits:
  Digits := digits+10: 
  oldDisplayprecision := interface(displayprecision=digits):
  # Read and parse A
  a:=evalf(map((x)->ifelse(type(x, string), parse(x), x), ImportMatrix(cat("../tableaux/", mname, "-a.txt")))):
  if pin then
    print(cat(mname, " A")):
    print(a):  
  end if:
  # Read and parse C
  c:=evalf(map((x)->ifelse(type(x, string), parse(x), x), ImportMatrix(cat("../tableaux/", mname, "-c.txt")))):   
  if pin then
    print(cat(mname, " C")):
    print(c):  
  end if:
  # Read and parse B
  b:=evalf(map((x)->ifelse(type(x, string), parse(x), x), ImportMatrix(cat("../tableaux/", mname, "-b.txt")))):
  if pin then
    print(cat(mname, " B")):
    print(b): 
  end if:
  # Print counts
  print(cat(mname, " STAGES")):
  print(RowDimension(a)): 
  print(cat(mname, " METHOD COUNT")):
  print(ColumnDimension(b)):
  # Print floating point A  
  print(cat(mname, " AF")):
  print(convert(LinearAlgebra:-Transpose(a), list)):
  # Print floating point C  
  print(cat(mname, " CF")):
  print(convert(c, list)):
  # Print floating point B
  print(cat(mname, " BF")):
  print(convert(b, list)):
  # Restore floating point settings
  Digits := oldDigits:
  interface(displayprecision=oldDisplayprecision):
  # Return
  NULL:
end proc:



# Convert a tableau into split rational form for Fortran and C
# e - The base
convert_rk_rat_ext := proc(mname, e, pin)
  uses LinearAlgebra, ArrayTools, FileTools:
  local a, b, c, a0, b0, c0, a1, b1, c1, a0d, b0d, c0d, a1d, b1d, c1d;
  if not(IsDirectory("../tableaux/")) then
    error "Could not find ../tableaux/ directory"
  end if:
  # Read and parse A
  a:=map((x)->ifelse(type(x, string), parse(x), x), ImportMatrix(cat("../tableaux/", mname, "-a.txt"))):
  if pin then
    print(cat(mname, " A")):
    print(a):
  end if:
  a0 := map((x)->coeff('x', e, 0), a);
  if pin then
    print(cat(mname, " A0")):
    print(a0):  
  end if:
  a1 := map((x)->coeff('x', e, 1), a);
  if pin then
    print(cat(mname, " A1")):
    print(a1):
  end if:
  # Read and parse C
  c:=map((x)->ifelse(type(x, string), parse(x), x), ImportMatrix(cat("../tableaux/", mname, "-c.txt"))):
  if pin then
    print(cat(mname, " C")):
    print(c):  
  end if:
  c0 := map((x)->coeff('x', e, 0), c);  
  if pin then
    print(cat(mname, " C0")):
    print(c0): 
  end if:
  c1 := map((x)->coeff('x', e, 1), c);  
  if pin then
    print(cat(mname, " C1")):
    print(c1):
  end if:
  # Read and parse B
  b:=map((x)->ifelse(type(x, string), parse(x), x), ImportMatrix(cat("../tableaux/", mname, "-b.txt"))):  
  if pin then
    print(cat(mname, " B")):
    print(b):
  end if:
  b0 := map((x)->coeff('x', e, 0), b);
  if pin then
    print(cat(mname, " B0")):
    print(b0): 
  end if:
  b1 := map((x)->coeff('x', e, 1), b);
  if pin then
    print(cat(mname, " B1")):
    print(b1):
  end if:
  # Print counts
  print(cat(mname, " STAGES")):
  print(RowDimension(a)): 
  print(cat(mname, " METHOD COUNT")):
  print(ColumnDimension(b)):
  # Print rational A  
  print(cat(mname, " AI")):
  a0d := apply(lcm, op(map(denom, convert(a0, list)))):
  print(a0d):  
  print(convert(LinearAlgebra:-Transpose(a0)*a0d, list)):
  a1d := apply(lcm, op(map(denom, convert(a1, list)))):
  print(a1d, "  * ", e):  
  print(convert(LinearAlgebra:-Transpose(a1)*a1d, list)):
  # Print rational C
  print(cat(mname, " CI")):
  c0d := apply(lcm, op(map(denom, convert(c0, list)))):
  print(a0d):  
  print(convert(LinearAlgebra:-Transpose(c0)*c0d, list)):
  c1d := apply(lcm, op(map(denom, convert(c1, list)))):
  print(c1d, "  * ", e):  
  print(convert(LinearAlgebra:-Transpose(c1)*c1d, list)):
  # Print rational B
  print(cat(mname, " BI")):
  b0d := apply(lcm, op(map(denom, convert(b0, list)))):
  print(b0d):  
  print(convert(LinearAlgebra:-Transpose(b0)*b0d, list)):
  b1d := apply(lcm, op(map(denom, convert(b1, list)))):
  print(b1d, "  * ", e):  
  print(convert(LinearAlgebra:-Transpose(b1)*b1d, list)):
  # Return
  NULL:
end proc:
