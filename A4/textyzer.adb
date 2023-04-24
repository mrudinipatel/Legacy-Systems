-- Name: Mrudini Patel
-- Course: CIS*3190 A4
-- Date: Saturday April 8, 2023

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure textyzer is
    -- This procedure is the 'main' procedure that calls upon the other functions.
    
    function getFilename return unbounded_string is 
        -- Gets filename and checks if it exists in local directory.
        -- If it doesn't, it keeps reprompting until valid file is entered.
        filename : unbounded_string;
    begin
        loop
            put_line("");
            put_line("Enter filename: ");
            get_line(filename);

            if exists(to_string(filename)) then
                exit;
            else
                put_line("File does not exist in local directory.");
            end if;
        end loop;

        return filename;
    end getFilename;

    function isWord (line : unbounded_string) return integer is
        -- Determines word based on whitespace present in line of text passed in.
        wordCount : integer := 1; -- Set to 1 to account for first word.
        letter : character;
    begin
        for i in 1..length(line) loop
            letter := element(line, i);

            if letter = ' ' or letter = ASCII.LF then
                wordCount := wordCount + 1;
            end if;
        end loop;

        return wordCount;
    end isWord;

    function charCount (line : unbounded_string) return integer is
        -- Increments counter if there are alphabetic letters present in line passed in.
        alpha : integer := 0;
        letter : character;
    begin
        for i in 1..length(line) loop
            letter := element(line, i);

            if (is_letter(letter)) then
                alpha := alpha + 1;
            end if;
        end loop;

        return alpha;
    end charCount;

    function sentCount (line : unbounded_string) return integer is
        -- Increments counter based on the 3 sentence punctuations (., !, ?).
        sent : integer := 0;
        letter : character;
    begin
        for i in 1..length(line) loop
            letter := element(line, i);

            if (letter = '.' or letter = '!' or letter = '?') then
                sent := sent + 1;
            end if;
        end loop;

        return sent;
    end sentCount;

    function punctCount (line : unbounded_string) return integer is
        -- Increments counter based on specified punctuations from provided pascal code.
        punct : integer := 0;
        letter : character;
    begin
        for i in 1..length(line) loop
            letter := element(line, i);

            if (letter = '.' or letter = '!' or letter = '?' or letter = ',' or letter = ':' or letter = ';') then
                punct := punct + 1;
            end if;
        end loop;

        return punct;
    end punctCount;

    function isNumber (line : unbounded_string) return integer is
        -- Increments counter if character falls in 0-9 range (its a valid digit).
        num : integer := 0;
        letter : character;
        curr_word : unbounded_string := to_unbounded_string("");
    begin
        for i in 1..length(line) loop
            letter := element(line, i);

            if letter in '0'..'9' then
                curr_word := curr_word & letter;
            else
                if length(curr_word) > 0 then
                    num := num + 1;
                    curr_word := to_unbounded_string("");
                end if;
            end if;
        end loop;

        return num;
    end isNumber;

    procedure printHist (fp : unbounded_string) is
        arr : array(1..20) of integer := (others => 0); -- Sets all indexes to 0.
        counter : integer := 0;
        letter : character;
        input : file_type;
        line : unbounded_string;
    begin
        -- Opening file again here to avoid resetting indexes to 0 each time (results in inaccurate calculation).
        -- Increment counter until space/punctuation is reached to get length of each word.
        -- Store length in that specific array index before resetting counter to get next word length.
        -- Continue until end of file is reached.
        open(input, in_file, to_string(fp));

        while not end_of_file(input) loop
            get_line(input, line);

            counter := 0;

            for i in 1..length(line) loop
                letter := element(line, i);

                if letter = ' ' or letter = ',' or letter = '.' or letter = '!' or letter = '?' then
                    if counter > 0 then
                        if counter <= 20 then
                            arr(counter) := arr(counter) + 1;
                        end if;
                    end if;

                    counter := 0;

                else
                    counter := counter + 1;
                end if;
            end loop;

            if counter > 0 then
                if counter <= 20 then
                    arr(counter) := arr(counter) + 1;
                end if;
            end if;
        end loop;

        close(input);

        -- Printing an asterik (*) for each word tallied in each index of the array, arr.
        put_line("");
        put_line("-------- Histogram - Word Length Distribution --------");

        for i in 1..20 loop
           put(i'Image & " ");

           for j in 1..arr(i) loop
              put("*");
           end loop;

           put_line("");
        end loop;
        
    end printHist;

    function analyzeText (file: unbounded_string) return string is
        input : file_type;
        line : unbounded_string;
        endMsg : unbounded_string := to_unbounded_string("");
        line_count : integer := 0;
        word_count : integer := 0;
        char_count : integer := 0;
        sent_count : integer := 0;
        punct_count : integer := 0;
        num_count : integer := 0;
        avgWordsPerSent : float := float(0);
        avgCharsPerWord : float := float(0);
    begin
        -- Open file passed into function and pass each line into the helper functions above.
        -- Store return values from helper functions into variables to be printed outside loop.
        open(input, in_file, to_string(file));

        while not end_of_file(input) loop
            get_line(input, line);
            word_count := word_count + (isWord(line));
            char_count := char_count + (charCount(line));
            sent_count := sent_count + (sentCount(line));
            line_count := line_count + 1;
            punct_count := punct_count + (punctCount(line));
            num_count := num_count + (isNumber(line));
        end loop;

        -- Calculating necessary average statistics inside float variables.
        word_count := word_count - num_count; -- Numbers are not words, so subtracting them from word_count.
        avgWordsPerSent := float(word_count) / float(sent_count);
        avgCharsPerWord := float(char_count) / float(word_count);

        put_line("");
        put_line("-------- TEXT STATISTICS --------");
        put_line("Character Count (a-z): " & Integer'image(char_count));
        put_line("Word Count: " & Integer'image(word_count));
        put_line("Sentence Count: " & Integer'image(sent_count));
        put_line("Number Count: " & Integer'image(num_count));
        put_line("Line Count: " & Integer'image(line_count));
        put_line("Punctuation Count: " & Integer'image(punct_count));
        
        put("Average letters per word:  ");
        put(avgCharsPerWord, Fore => 0, Aft => 2, Exp => 0);
        put_line("");
        put("Average words per sentence:  ");
        put(avgWordsPerSent, Fore => 0, Aft => 2, Exp => 0);
        put_line("");

        close(input);

        -- Calling on printHist procedure to print word length distribution histogram.
        put_line("");
        printHist(file);

        -- Returning ending message back to main procedure.
        put_line("");
        endMsg := to_unbounded_string("Program ended. Run again to get another file's statistics.");

        return to_string(endMsg);
    end analyzeText;
begin
    declare
        file : unbounded_string;
        retString : unbounded_string;
    begin
        -- Calling on getFilename to retrieve valid filename and passing it into analyzeText
        -- Printing returned string from analyzeText (program end message).
        file := getFilename;
        retString := to_unbounded_string(analyzeText(file));
        put_line(retString);
    end;
end textyzer;







