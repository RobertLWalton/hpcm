program count ( input, output );

var
    paragraph : integer;
    characters : integer;
    words : integer;
    lines : integer;
    c : char;
    nonblank : boolean;
    endline : boolean;
    endparagraph : boolean;
begin
    paragraph := 1;

    while not eof do
    begin
	characters := 0;
	words := 0;
	lines := 0;
	endparagraph := false;

	while not eof and not endparagraph do
	begin

	    nonblank := false;
	    endline := false;

	    while not nonblank and not endline do
	    begin
		if eoln then endline := true
		else
		    begin
			read (c);
			nonblank := not ( c = ' ' );
			characters := characters + 1
		    end
	    end;
	        

	    if endline then
		endparagraph := true
	    else
	    begin
		lines := lines + 1;

		while not endline do
		begin

		    words := words + 1;

		    while not endline and nonblank do
		    begin
			if eoln then endline := true
			else
			    begin
				read (c);
				nonblank := not ( c = ' ' );
				characters :=
				    characters + 1
			    end
		    end;

		    while not endline and not nonblank
			do
		    begin
			if eoln then endline := true
			else
			    begin
				read (c);
				nonblank := not ( c = ' ' );
				characters :=
				    characters + 1
			    end
		    end
		end
	    end;
	    readln
	end;

	if lines > 0 then
	begin
	    writeln ( 'Paragraph ', paragraph:0, ': ',
		      lines:0, ' lines, ',
		      words:0, ' words, ',
		      characters:0, ' characters.' );
	    paragraph := paragraph + 1
	end
    end;

end;
