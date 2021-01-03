#!/bin/sh

dotnet publish -o ~/.local/share/solfa
if [ ! -f ~/.local/bin/solfa ]; then
  ln -s ~/.local/share/solfa/solfa ~/.local/bin/solfa
fi
