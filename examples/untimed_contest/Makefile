HPCM=${HOME}/hpcm

INDICES=    ${HPCM}/user/index \
	    ${HPCM}/problem_library/index, \
	    public/=problems/, \
	    private/=problems/, \
	    demo:'include', \
	    preliminary96:'include', \
	    selection96:'include', \
	    selection97:'include'

all:
	makejudgingdir -email ${INDICES}
	chmod a+x . ..

scoreboard:
	scorefinder | scoreboard

update_scoreboard:
        sendscoreboard '"wyg2bpitbp scoreboard"<hc3@deas.harvard.edu>'

clean:
	rm -rf info solutions mail help
