lambac: lambac.icl lambac.prj Lamba/*.icl Lamba/*.dcl Lamba/Language/*.icl Lamba/Language/*.dcl
	cpm project lambac.prj build

clean:
	rm lambac
	rm test
