# pure-nes: NES emulator written in Haskell

[![Build Status](https://dev.azure.com/tufh3g/Build/_apis/build/status/sutajo.pure-nes?branchName=master)](https://dev.azure.com/tufh3g/Build/_build/latest?definitionId=1&branchName=master)

![Alter Ego by Shiru](showcase/shiru.png)

# Description
This project is a part of my Bachelor's thesis, which aims to explore 
the field of emulation using functional programming.

The main focus was to use efficient techniques in a readable fashion to achieve good performance.

The program is completely cross platform, built on top of GTK+ and SDL2.

[If you speak hungarian, click here if you want to read my thesis :)](documentation/thesis.pdf)

# Functionality and compatibility

The following mappers are supported: 0, 2, 3

Overall the emulation is quite accurate, except for some timing issues and edge cases regarding the PPU.

Only CPU and PPU are emulated, meaning that there is only video output. However, I'm planning on implementing the APU soon.

You can either play with a keyboard or a controller. The controls are listed in the main menu.

I tested a few games on my Ryzen 7 2700 processor and the average FPS lied between 150-170 FPS (with Ubuntu + LLVM backend).
Even an old Core 2 Duo processor is fast enough to run the emulation in real-time.

There is a GHC bug on Windows that prevents compilation with the LLVM backend. Due to the fact that the NCG backend generally
produces slower code, the emulator runs somewhat slower on Windows. 

# Build instructions for Ubuntu

# Install Gtk
```console
$ sudo apt-get install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
```

# Install SDL2
```console
$ sudo apt-get install libsdl2-dev
```

# Build the project
```console
$ stack build
```

# Start the emulator
```console
$ stack exec pure-nes
```

# (Optional): Run tests to check the accuracy of the emulator
```console
$ stack test
```
