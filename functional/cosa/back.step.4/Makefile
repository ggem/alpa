SGEN = sgen
OS = $(shell arch.guess)
SGENFLAGS = $(PARAMS) -I$(SYNLOC)/$(OS)/lib/stk/$(STK_VER)/include
FILES = concrete.ssl abstract.ssl ids.ssl attributes.ssl \
	stage1.ssl stage2.ssl unparse.ssl hilite.ssl
EDITOR = timebound

$(EDITOR): $(FILES)
	$(SGEN) -o $@ $(SGENFLAGS) $(FILES)
