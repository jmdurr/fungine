# fungine

The beginning of a functional game engine written in haskell.

There is no guarantee this builds at any given time until it reaches v0.2


## UI
The ui in fungine is driven by:
  - svg
  - png

## 3d Models
Since the first game in fungine will be a RPG, it needs to support:
  - characters:
    * animation
    * armor and weapon attachment
    * emitters 
    * cloaks - animation
    * lighting effects - torches etc
    * skeletons
    * uv mapping

  - scenery:
    * lighting effects
    * 


  - there are a few creature types:
    * 2 legged
      - hop
      - walk
      - claw
      - punch
      - smash
      - sword
      - spear
      - staff
        these can all be acomplished by having base animations that are
          applied to any skeleton that matches the given type
        bones need labels
        
    * 4 legged

    * 0 legged
    * 2 legged winged
    * 4 legged winged
    * 0 legged winged