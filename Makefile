FORMS=\
 fglquerydlg.42f \
 fglquerydlg_value.42f \
 query_dialog_test.42f

PROGMOD=\
 fglquerydlg.42m \
 query_dialog_test.42m

all: $(PROGMOD) $(FORMS)

run:: all
	fglrun query_dialog_test

%.42f: %.per
	fglform -M $<

%.42m: %.4gl
	fglcomp -M $<

clean::
	rm -f *.42?
