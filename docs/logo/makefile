
IMAGES = MRKISS_logo_m_1000x500.png MRKISS_logo_m_500x250.png MRKISS_logo_m_250x125.png MRKISS_logo_c_1000x500.png MRKISS_logo_c_500x250.png MRKISS_logo_c_250x125.png MRKISS_logo_m_100x50.png MRKISS_logo_c_100x50.png MRKISS_logo_bg_1200x600.png MRKISS_logo_bg_120x60.png MRKISS_logo_bg_300x150.png MRKISS_logo_bg_600x300.png




MRKISS_logo.pdf : MRKISS_logo.tex
	pdflatex MRKISS_logo.tex




MRKISS_logo_m_1000x500.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=pnggray -g1000x500 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_m_1000x500.png MRKISS_logo.pdf

MRKISS_logo_m_500x250.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=pnggray -g500x250 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_m_500x250.png MRKISS_logo.pdf

MRKISS_logo_m_250x125.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=pnggray -g250x125 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_m_250x125.png MRKISS_logo.pdf

MRKISS_logo_m_100x50.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=pnggray -g100x50 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_m_100x50.png MRKISS_logo.pdf





MRKISS_logo_c_1000x500.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=png16m -g1000x500 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_c_1000x500.png MRKISS_logo.pdf

MRKISS_logo_c_500x250.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=png16m -g500x250 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_c_500x250.png MRKISS_logo.pdf

MRKISS_logo_c_250x125.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=png16m -g250x125 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_c_250x125.png MRKISS_logo.pdf

MRKISS_logo_c_100x50.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=png16m -g100x50 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_c_100x50.png MRKISS_logo.pdf





MRKISS_logo_bg_1200x600.png : MRKISS_logo.pdf
	magick logo_background.png -resize 1200x600 MRKISS_logo_bg_1000x500.png

MRKISS_logo_bg_600x300.png : MRKISS_logo.pdf
	magick logo_background.png -resize 600x300 MRKISS_logo_bg_600x300.png

MRKISS_logo_bg_300x150.png : MRKISS_logo.pdf
	magick logo_background.png -resize 300x150 MRKISS_logo_bg_300x150.png

MRKISS_logo_bg_120x60.png : MRKISS_logo.pdf
	magick logo_background.png -resize 120x60 MRKISS_logo_bg_120x60.png








view : $(IMAGES)
	nomacs $(IMAGES)

all : $(IMAGES)

clean : 
	rm -f $(IMAGES) *.pdf *.log *.aux *~
