# FPS.cob

`FPS.cob` is what you get when you decide that game development is too easy nowadays.
Enjoy what can only be described as an out-of-body experience in the ways of COBOL tomfoolery.

It supports two level styles:

- `map/level1.map`: the original grid-based Wolf3D-style path.
- `map/doom_sectors.map`: a sector/linedef map with doors, different heights (so DOOM-like).

## Requirements

- `cobc`   from the GnuCOBOL project
- `ffplay` from the FFMpeg project
- `bash`

## Run

From the repo root:

```bash
bash build.sh
```

To load a specific map:

```bash
bash build.sh ./map/level1.map
bash build.sh ./map/doom_sectors.map
```

## Controls

- `W` / `S`: move forward and backward
- `A` / `D`: turn left and right
- `F` toggle full-screen
- `Space`: fire
- `Q`: quit

## Assets

Textures and sprites live under `res/`.
Maps live under `map/`.
