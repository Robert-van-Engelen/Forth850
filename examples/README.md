Forth850 Additions and Examples
===============================

With the Forth850 full version you can load a Forth source code example into
the PC-G850(V)(S) Text Editor and execute `TEXT` in Forth850 to load it into
Forth850.

With a [serial interface](https://www.hpmuseum.org/forum/thread-19431-post-169250.html#pid169250)
you can load Forth source code via the TEXT Sio Load command.  By default, the
SIO settings in TEXT Sio Format "line number: yes" adds line numbers
automatically when the file is sent (Sio Save) to a PC.

**When receiving a file on the PC-G850(V)(S) via TEXT Sio Load, letting the
PC-G850(V)(S) automatically add line numbers is not a good idea, because lines
that start with a digit will make the resulting concatenated line number
unusable! I recommend to use TEXT Sio Format `line number = yes`.  This
setting accepts files with line numbers and without line numbers.**

If you do get errors when loading, you should add line numbers to the file that
you want to send by using the `nl` utility on MacOS, Linux and Android:

    nl -ba -v10 -i5 -nln -w4 -s' ' filename.fth > numbered_filename.fth

The `numbered_filename.fth` has line numbers and can be sent via SIO.

To load Forth source code via the cassette interface CE-126P or CE-124, you can
use [PocketTools](https://www.peil-partner.de/ifhe.de/sharp/) on a Windows PC.
To use PocketTools on MacOS, Linux or Android, compile the PocketTools source
code with a C compiler:

    cd PocketTools_vvv/POCKTOOL/Sources
    make

This creates the `bas2img`, `bin2wav` and `wav2bin` utilities.

Convert a Forth source file `filename.fth` to a wav file with `bas2img` and
`bin2wav`:

    ./bas2img --pc=G850VS --type=asm -l0x408 filename.fth
    ./bin2wav --pc=G850VS filename.img

Option `--type=asm` automatically adds line numbers so you do not need to add
line numbers to the Forth source code to transfer.  Option `-l0x408` preserves
the content.  In this way you can also convert Z80 assembly source code and C
source code to wav files.

In BASIC RUN MODE on the PC-G850(V)(S), execute `BLOAD` to load via the
cassette interface.  Play the wav file, for example with `afplay filename.wav`
in MacOS terminal.  After loading, go to the Text Editor and press B for Basic
then T to select Text<-basic.  This loads a copy of the Forth source file into
the Text Editor.  The program loaded in BASIC PRO MODE can be deleted.

In Forth850 execute `TEXT` to load Forth source from the Text Editor.
