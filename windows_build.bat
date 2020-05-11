:: You need to install Gtk and SDL2 via MSYS2 before running this script

SET PATH=C:\msys64\mingw64\bin;C:\msys64\usr\bin;%PATH%
SET PKG_CONFIG_PATH=C:\msys64\mingw64\lib\pkgconfig
SET XDG_DATA_DIRS=C:\msys64\mingw64\share
stack build --ghc-options="-split-sections"
pause