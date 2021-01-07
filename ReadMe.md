
FsWasm
======

This is a hobby project examining Web Assembly and X86/ARM 
assembly language generation.  It's really just an excuse for
me to look at these things.

This project incorporates the following code I have written
myself:

- Loading of a Web Assembly binary (.wasm) file onto the heap
  into primary data structures that correspond closely with the
  original file organisation.
  
- Re-organisation of the loaded data into "Better Wasm" which
  I have defined, and is trying to be a more convenient 
  tree-shaped organisation than the original tables.  Obviously,
  this might be contentious.
  
- Translation of the WASM function code sequences into an
  intermediate representation I am calling the "Common Register
  Machine".  This is something I have defined myself, and it
  forces the register machine to directly mimic the actions of
  WASM's stack machine on its own stack.
  
- Reductions of my verbose stack-machine style code sequences 
  into smaller sequences by removing unnecessary Push/Pop memory
  reference sequences, and some other things I noted, but at
  this time there is much more that could be done.  This uses
  peephole sliding windows.  This isn't going to introduce 
  usage of more than a couple of registers either.
  
- Translation of the Common Register Machine code into ARMv7,
  X86/32 assembly language code, as well as assembly code for
  a custom assembler I wrote myself (not included here).

- Rudimentary hosting environments written in C++ that support
  loading and executing the binary file that resulted from 
  assembling the above output.

Points:  

- FsWasm is the main program (other than the test framework)
  and is likely to be volatile.  At the time of writing, this
  translates "program-5.wasm" into ARM assembly, and writes
  it to the STDOUT.  I then capture that from the terminal
  window and paste into 3rdParty\program-5-Arm32.fasm and
  run the associated batch file to invoke the assembler.
  This updates the ".bin" file within the CppHosting\ folders
  so that when you build Debug, it will push the image file
  over SSH to the Raspberry PI.
  
- Use the Disassembly window to step into the entry point
  of the binary image in the 1GB memory region.  At the time
  of writing this does NOT correctly disassemble ARM MOVW/MOVT
  instructions, they appear as complete nonsense.
 
- Use the Memory View window to see the 1GB region, but this
  does (at the time of writing) have BAD bugs where it gives
  up showing everything there because of adjacent absent 
  memory pages.  This is VERY VERY BAD Microsoft!
 
- No WASM verification is done.  I am choosing that this is
  out of scope.
  
- Translation is not supported for more than a single .wasm
  module at present.  I could throw several into a bucket and
  statically link them as a future subproject.  Support for 
  imports is a bit patchy therefore.

- I could do better with the ARM just by using the shorter
  sized THUMB instructions.  This is a to-do, as I need to read
  up on the spec for that.
  
- ARMv8 is also a to-do, as again I need to read up.

- Currently the generated assembly language text is for the
  FASM and FASMARM assemblers, and I produce a flat binary
  with origin at address 1GB.  This loads into the "Rudimentary"
  test rigs and I am using Microsoft Visual Studio Community
  Edition to host these, and remotely debug the ARMv7 on a
  Raspberry PI 3.
  
Development Ingredients

- X86 Laptop with Windows 10
- Microsoft Visual Studio Community Edition 2019
- Microsoft F#
- Microsoft C++
- Microsoft C++ for Linux development option for remote
  build / deploy / debug over SSH!!
- Raspberry PI 3, headless  (Just raspbian with SSH enabled).

3rdParty

- FASM and FASMARM.  Get these separately and unzip.
- Unzip FASM into FsWasm\3rdParty\fasmw17325\
- Unzip FASMARM into FsWasm\3rdParty\FASMARM_win32\

