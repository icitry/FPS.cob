#!/usr/bin/env bash
set -e

MAP_PATH="${1:-./map/doom_sectors.map}"

cobc -x -free -O2 fps.cob -o fps

if [ -r /dev/tty ]; then
  ./fps "$MAP_PATH" </dev/tty | ffplay -autoexit -f image2pipe -framerate 60 -probesize 32 -vf "scale=800:600:flags=neighbor" -i -
else
  ./fps "$MAP_PATH" | ffplay -autoexit -f image2pipe -framerate 60 -probesize 32 -vf "scale=800:600:flags=neighbor" -i -
fi
