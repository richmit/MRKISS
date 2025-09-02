# Stability Stuff For RK Methods (Explicit or Implicit)
# Produces graphs of absolute stability for each embedded method, a fancy combined graph of absolute stability for the first two methods, order stars for each method.  All graphs include roots and poles.  Saves the numerator and denominator polynomials for the stability function for each method. 
# pin - print inputs
# pp - print plots
stab_plot := proc(mname, pin, pp, { x_min := -6, x_max := 6, y_min := 6, y_max := -6 })
  uses LinearAlgebra, ArrayTools, plots, plottools, PolynomialTools, FileTools:
  local oldDigits, oldDisplayprecision, vue, grd, clrs, a, b, c, nstages, nmeth, R_Npoly, i, R_Dpoly, R_roots, R_poles, R_fun, symsiz, plt_rcpx, plt_stab, plt_star, plt_root, plt_pole, plt_plbg, plt_rtbg, cmb_stab, cmb_astab, cmb_star:
  print(cat("WORKING... ", mname)):
  # Check that we are running in the correct directory
  if not(IsDirectory("../tableaux/")) then
    error "Could not find ../tableaux/ directory"
  end if:
  if not(IsDirectory("RFunc")) then
    error "Could not find RFunc/ directory"
  end if:
  if not(IsDirectory("RPlot")) then
    error "Could not find RPlot/ directory"
  end if:
  # Setup floating point
  oldDigits := Digits:
  Digits := 60:
  oldDisplayprecision := interface(displayprecision=60):
  vue:= [x_min..x_max, y_min..y_max]:
  grd := [200,200]: 
  clrs := ["blue", "red", "darkgreen", "brown", "black"]:
  # Read and parse A
  a:=map((x)->ifelse(type(x, string), parse(x), x), ImportMatrix(cat("../tableaux/", mname, "-a.txt"))):
  if pin then
    print(cat(mname, " A")):
    print(a):
  end if:
  # Read and parse C
  c:=map((x)->ifelse(type(x, string), parse(x), x), ImportMatrix(cat("../tableaux/", mname, "-c.txt"))):
  if pin then
    print(cat(mname, " C")):
    print(c):  
  end if:
  # Read and parse B
  b:=map((x)->ifelse(type(x, string), parse(x), x), ImportMatrix(cat("../tableaux/", mname, "-b.txt"))):  
  if pin then
    print(cat(mname, " B")):
    print(b):
  end if:
  # Compute counts for stages and methods
  nstages := RowDimension(a); 
  nmeth := ColumnDimension(b);
  # Compute the numerator for R for each method
  R_Npoly := map((x)->Determinant(IdentityMatrix(nstages)-z*a+z*MatrixVectorMultiply(ConstantMatrix(1, nstages, 1), Transpose(x))), 
                 [seq(b[1..,i..i],i=1..nmeth)]);
  # Compute the denominator for R
  R_Dpoly := Determinant(IdentityMatrix(nstages)-z*a);
  # Compute roots for numerator(s) and the denominator
  R_roots := map((x)->[evalf(allvalues(RootOf(x)))], R_Npoly);
  R_poles := ifelse(depends(R_Dpoly, z), evalf(allvalues(RootOf(R_Dpoly))), NULL);
  # Write polynomials, roots, and poles out to disk.
  for i from 1 to nmeth do
    ExportMatrix(cat("RFunc/", mname, "-RNpoly", i, ".txt"), Matrix(CoefficientVector(R_Npoly[i],z))):
  end do:
  if not(R_poles=NULL) then
    ExportMatrix(cat("RFunc/", mname, "-RDpoly.txt"), Matrix(CoefficientVector(R_Dpoly,z))):
  end if:
  for i from 1 to nmeth do
    ExportMatrix(cat("RFunc/", mname, "-RNroots", i, ".txt"), Transpose(Matrix(R_roots[i]))):
  end do:
  if not(R_poles=NULL) then
    ExportMatrix(cat("RFunc/", mname, "-RNroots", i, ".txt"), Transpose(Matrix([R_poles]))):
  end if:
  R_fun := map((x)->MakeFunction(evalf(x/R_Dpoly), z), R_Npoly):
  # Set digits to something smaller to speed up remainig computation
  Digits := 10:
  interface(displayprecision=-1):
  # Create component plot objects
  if R_poles=NULL then
    symsiz:=[11, 13]:
  else
    symsiz:=[15, 20]:
  end if:
  plt_rcpx := complexplot(evalf(R_Npoly[1]/R_Dpoly), z=x_min+y_min*I..x_max+y_max*I, colorbar=false, grid=grd, labels=["",""]):
  plt_stab := map((i)->implicitplot(abs(R_fun[i](x+y*I))=1,x=x_min..x_max,y=y_min..y_max, view=vue, color=clrs[i], thickness=2), 
                  [seq(i, i=1..nmeth)]):
  plt_star := map((i)->implicitplot(abs(R_fun[i](x+y*I))=abs(exp(x)),x=x_min..x_max,y=y_min..y_max, view=vue, color=clrs[i], 
                                    coloring=["lightblue", "pink"], grid=grd, filledregions=true,thickness=2), 
                  [seq(i, i=1..nmeth)]):
  plt_root := map((i)->pointplot(map((z)->[Re(z), Im(z)], R_roots[i]), style=point, symbol=ifelse(R_poles=NULL, solidcircle, cross), 
                                 color=clrs[i], symbolsize=symsiz[1], view=vue), 
                  [seq(i, i=1..nmeth)]):
  plt_pole := pointplot(map((z)->[Re(z), Im(z)], [R_poles]), style=point, symbol=diamond, color=black, symbolsize=symsiz[1], view=vue):
  plt_plbg := pointplot(map((z)->[Re(z), Im(z)], [R_poles]), style=point, symbol=solidcircle, color=white, symbolsize=symsiz[2], view=vue):
  plt_rtbg := map((i)->pointplot(map((z)->[Re(z), Im(z)], R_roots[i]), style=point, symbol=solidcircle, color=white, 
                                 symbolsize=symsiz[2], view=vue), 
                  [seq(i, i=1..nmeth)]):
  # Create combined "main" stablity plot
  if nmeth=1 then
    if R_poles=NULL then
      cmb_stab := display(plt_rcpx, plt_stab[1], plt_rtbg[1], plt_root[1], view=vue, size=[1024,1024], 
                          title=cat("Stability for ", mname), 
                          labels=[typeset(Re), typeset(Im)], font=[Times, normal, 20], labelfont=[Times, normal, 40]):
    else
      cmb_stab := display(plt_rcpx, plt_stab[1], plt_plbg, plt_rtbg[1], plt_root[1], plt_pole, view=vue, size=[1024,1024], 
                          title=cat("Stability for ", mname), 
                          labels=[typeset(Re), typeset(Im)], font=[Times, normal, 20], labelfont=[Times, normal, 40]):
    end if:
  else
    if R_poles=NULL then
      cmb_stab := display(plt_rcpx, plt_stab[1], plt_stab[2], plt_rtbg[1], plt_rtbg[2], plt_root[1], plt_root[2], 
                          view=vue, size=[1024,1024], title=cat("Stability 1(", clrs[1], ") & 2(", clrs[2], ") for ", mname), 
                          labels=[typeset(Re), typeset(Im)], font=[Times, normal, 20], labelfont=[Times, normal, 40]):
    else
      cmb_stab := display(plt_rcpx, plt_stab[1], plt_stab[2], plt_plbg, plt_rtbg[1], plt_rtbg[2], plt_root[1], plt_root[2], plt_pole, 
                          view=vue, size=[1024,1024], title=cat("Stability 1(", clrs[1], ") & 2(", clrs[2], ") for ", mname), 
                          labels=[typeset(Re), typeset(Im)], font=[Times, normal, 20], labelfont=[Times, normal, 40]):
    end if:
  
  end if: 
  if pp then
    print(cmb_stab);
  end if:
  exportplot(JoinPath([currentdir(), "RPlot", cat(mname, "-stab.png")]), cmb_stab);
  # Create combined stablity plot
  cmb_astab := display(plt_stab, plt_root, view=vue, size=[1024,1024], 
                               title=cat("Stability for ", mname, " ", clrs[1..nmeth]), 
                               labels=[typeset(Re), typeset(Im)], 
                               font=[Times, normal, 20], labelfont=[Times, normal, 40]): 
  if pp then
    print(cmb_astab);
  end if:
  exportplot(JoinPath([currentdir(), "RPlot", cat(mname, "-astab.png")]), cmb_astab);
  # Create order star plots
  cmb_star := map((i)->display(plt_star[i], plt_plbg, plt_pole, plt_rtbg[i], plt_root[i], view=vue, size=[1024,1024], 
                               title=cat("Order Star ", ifelse(nmeth=1, "", i), ifelse(nmeth=1, "", " "), "for ", mname), 
                               labels=[typeset(Re), typeset(Im)], 
                               font=[Times, normal, 20], labelfont=[Times, normal, 40]), [seq(i, i=1..nmeth)]):
  if pp then
    for i from 1 to nmeth do
      print(cmb_star[i]);
    end do:
  end if:
  for i from 1 to nmeth do
      exportplot(JoinPath([currentdir(), "RPlot", cat(mname, "-star", i, ".png")]), cmb_star[i]);
  end do;
  # Restore floating point settings
  Digits := oldDigits:
  interface(displayprecision=oldDisplayprecision):
  # Return
  NULL:
end proc:
