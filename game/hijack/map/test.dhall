{
  width = 240,
  height = 160,
  lock = "teiri",
  objects = [
    <
      teiri = {
        type = "teiri",
        id = ["teiri"] : Optional Text,
        x = +0,
        y = +0,
        control = <
          playable = {
            type = "playable"
          }
        | wave : {
            type : Text,
            incr : Double,
            bias : Double
          }
        >,
        pose = "walk",
        direction = "front"
      }
    | gray : {
        type : Text,
        id : Optional Text,
        x : Integer,
        y : Integer
      }
    >
  ] # ./Prelude/List/map {
    x : Integer,
    y : Integer
  } <
    teiri : {
      type : Text,
      id : Optional Text,
      x : Integer,
      y : Integer,
      control : <
        playable : {
          type : Text
        }
      | wave : {
          type : Text,
          incr : Double,
          bias : Double
        }
      >,
      pose : Text,
      direction : Text
    }
  | gray : {
      type : Text,
      id : Optional Text,
      x : Integer,
      y : Integer
    }
  > (\(p : {
    x : Integer,
    y : Integer
  }) ->
    <teiri = {
      type = "teiri",
      id = [] : Optional Text,
      x = p.x,
      y = p.y,
      control = <wave = {
        type = "wave",
        incr = 0.0245,
        bias = 0.252
      } | playable : {
        type : Text
      }>,
      pose = "walk",
      direction = "front" }
    | gray : {
        type : Text,
        id : Optional Text,
        x : Integer,
        y : Integer
      }>
  ) [
    {x = +60, y = +40},
    {x = -60, y = +40},
    {x = +60, y = -40},
    {x = -60, y = -40}
  ] # ./Prelude/List/map {
    x : Integer,
    y : Integer
  } <
    teiri : {
      type : Text,
      id : Optional Text,
      x : Integer,
      y : Integer,
      control : <
        playable : {
          type : Text
        }
      | wave : {
          type : Text,
          incr : Double,
          bias : Double
        }
      >,
      pose : Text,
      direction : Text
    }
  | gray : {
      type : Text,
      id : Optional Text,
      x : Integer,
      y : Integer
    }
  > (\(p : {
    x : Integer,
    y : Integer
  }) ->  <
    gray = {
      type = "gray",
      id = [] : Optional Text,
      x = p.x,
      y = p.y
    }
  | teiri : {
      type : Text,
      id : Optional Text,
      x : Integer,
      y : Integer,
      control : <
        playable : {
          type : Text
        }
      | wave : {
          type : Text,
          incr : Double,
          bias : Double
        }
      >,
      pose : Text,
      direction : Text
    }
  >) [
    {x = -96, y = -64},
    {x = -96, y = -48},
    {x = -96, y = -32},
    {x = -96, y = -16},
    {x = -96, y = +0},
    {x = -96, y = +16},
    {x = -96, y = +32},
    {x = -96, y = +48},
    {x = -96, y = +64},
    {x = +96, y = -64},
    {x = +96, y = -48},
    {x = +96, y = -32},
    {x = +96, y = -16},
    {x = +96, y = +0},
    {x = +96, y = +16},
    {x = +96, y = +32},
    {x = +96, y = +48},
    {x = +96, y = +64},
    {x = -80, y = -64},
    {x = -64, y = -64},
    {x = -48, y = -64},
    {x = -32, y = -64},
    {x = -16, y = -64},
    {x = +0, y = -64},
    {x = +80, y = -64},
    {x = +64, y = -64},
    {x = +48, y = -64},
    {x = +32, y = -64},
    {x = +16, y = -64},
    {x = -80, y = +64},
    {x = -64, y = +64},
    {x = -48, y = +64},
    {x = -32, y = +64},
    {x = -16, y = +64},
    {x = +0, y = +64},
    {x = +80, y = +64},
    {x = +64, y = +64},
    {x = +48, y = +64},
    {x = +32, y = +64},
    {x = +16, y = +64}
  ]
}
