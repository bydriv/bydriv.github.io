// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");

function intro(param) {
  return Promise.resolve(/* record */[
              /* i */0,
              /* x */param[0],
              /* y */param[1],
              /* pose : Walk */0,
              /* direction : Front */3
            ]);
}

function step(inputs, teiri) {
  var xshift = inputs.length === 0 ? 0 : (
      Caml_array.caml_array_get(inputs, 0).x < -0.25 ? -1 : (
          Caml_array.caml_array_get(inputs, 0).x > 0.25 ? 1 : 0
        )
    );
  var yshift = inputs.length === 0 ? 0 : (
      Caml_array.caml_array_get(inputs, 0).y < -0.25 ? -1 : (
          Caml_array.caml_array_get(inputs, 0).y > 0.25 ? 1 : 0
        )
    );
  return Promise.resolve(/* record */[
              /* i */teiri[/* i */0] + 1 | 0,
              /* x */teiri[/* x */1] + xshift | 0,
              /* y */teiri[/* y */2] + yshift | 0,
              /* pose */inputs.length === 0 || !Caml_array.caml_array_get(Caml_array.caml_array_get(inputs, 0).buttons, 4) ? /* Walk */0 : /* Hijack */1,
              /* direction */yshift < 0 ? /* Back */1 : (
                  yshift > 0 ? /* Front */3 : (
                      xshift < 0 ? /* Left */0 : (
                          xshift > 0 ? /* Right */2 : teiri[/* direction */4]
                        )
                    )
                )
            ]);
}

function x(teiri) {
  var match = teiri[/* pose */3];
  if (match) {
    return teiri[/* x */1] - 8 | 0;
  } else {
    return teiri[/* x */1];
  }
}

function y(teiri) {
  var match = teiri[/* pose */3];
  if (match) {
    return teiri[/* y */2] - 8 | 0;
  } else {
    return teiri[/* y */2];
  }
}

function string_of_direction(param) {
  switch (param) {
    case 0 : 
        return "left";
    case 1 : 
        return "back";
    case 2 : 
        return "right";
    case 3 : 
        return "front";
    
  }
}

function views(teiri) {
  return Promise.resolve(/* array */[/* `Image */[
                -795439301,
                /* tuple */[
                  x(teiri),
                  y(teiri),
                  "pixelart/teiri/" + ((
                      teiri[/* pose */3] ? "hijack" : "walk"
                    ) + ("/" + (string_of_direction(teiri[/* direction */4]) + ("/" + (String((teiri[/* i */0] / 8 | 0) % 4) + ".png")))))
                ]
              ]]);
}

var Teiri = /* module */[
  /* intro */intro,
  /* step */step,
  /* views */views
];

function intro$1(param) {
  return Promise.resolve(/* record */[
              /* x */param[0],
              /* y */param[1],
              /* width */param[2],
              /* top_height */param[3],
              /* bot_height */param[4]
            ]);
}

function step$1(param, building) {
  return Promise.resolve(building);
}

function views$1(building) {
  var views$2 = Caml_array.caml_make_vect(Caml_int32.imul(building[/* width */2] / 16 | 0, (building[/* top_height */3] + building[/* bot_height */4] | 0) / 16 | 0), /* `Image */[
        -795439301,
        /* tuple */[
          0,
          0,
          ""
        ]
      ]);
  for(var i = 0 ,i_finish = (building[/* width */2] / 16 | 0) - 1 | 0; i <= i_finish; ++i){
    for(var j = 0 ,j_finish = ((building[/* top_height */3] + building[/* bot_height */4] | 0) / 16 | 0) - 1 | 0; j <= j_finish; ++j){
      var name = i === 0 && j === 0 ? "pixelart/maptip/building/0-0.png" : (
          i === ((building[/* width */2] / 16 | 0) - 1 | 0) && j === 0 ? "pixelart/maptip/building/2-0.png" : (
              i === 0 && j === ((building[/* top_height */3] / 16 | 0) - 1 | 0) ? "pixelart/maptip/building/0-2.png" : (
                  i === ((building[/* width */2] / 16 | 0) - 1 | 0) && j === ((building[/* top_height */3] / 16 | 0) - 1 | 0) ? "pixelart/maptip/building/2-2.png" : (
                      i === 0 && j === (((building[/* top_height */3] + building[/* bot_height */4] | 0) / 16 | 0) - 1 | 0) ? "pixelart/maptip/building/0-4.png" : (
                          i === ((building[/* width */2] / 16 | 0) - 1 | 0) && j === (((building[/* top_height */3] + building[/* bot_height */4] | 0) / 16 | 0) - 1 | 0) ? "pixelart/maptip/building/2-4.png" : (
                              i === 0 && j < (building[/* top_height */3] / 16 | 0) ? "pixelart/maptip/building/0-1.png" : (
                                  i === 0 ? "pixelart/maptip/building/0-3.png" : (
                                      i === ((building[/* width */2] / 16 | 0) - 1 | 0) && j < (building[/* top_height */3] / 16 | 0) ? "pixelart/maptip/building/2-1.png" : (
                                          i === ((building[/* width */2] / 16 | 0) - 1 | 0) ? "pixelart/maptip/building/2-3.png" : (
                                              j === 0 ? "pixelart/maptip/building/1-0.png" : (
                                                  j === (((building[/* top_height */3] + building[/* bot_height */4] | 0) / 16 | 0) - 1 | 0) ? "pixelart/maptip/building/1-4.png" : (
                                                      j === ((building[/* top_height */3] / 16 | 0) - 1 | 0) ? "pixelart/maptip/building/1-2.png" : (
                                                          j < (building[/* top_height */3] / 16 | 0) ? "pixelart/maptip/building/1-1.png" : "pixelart/maptip/building/1-3.png"
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        );
      Caml_array.caml_array_set(views$2, Caml_int32.imul(j, building[/* width */2] / 16 | 0) + i | 0, /* `Image */[
            -795439301,
            /* tuple */[
              building[/* x */0] + (i << 4) | 0,
              building[/* y */1] + (j << 4) | 0,
              name
            ]
          ]);
    }
  }
  return Promise.resolve(views$2);
}

var Building = /* module */[
  /* intro */intro$1,
  /* step */step$1,
  /* views */views$1
];

function intro$2(param) {
  if (param.tag) {
    return intro$1(param[0]).then((function (building) {
                  return Promise.resolve(/* Building */Block.__(1, [building]));
                }));
  } else {
    return intro(param[0]).then((function (teiri) {
                  return Promise.resolve(/* Teiri */Block.__(0, [teiri]));
                }));
  }
}

function step$2(inputs, param) {
  if (param.tag) {
    return Promise.resolve(param[0]).then((function (building) {
                  return Promise.resolve(/* Building */Block.__(1, [building]));
                }));
  } else {
    return step(inputs, param[0]).then((function (teiri) {
                  return Promise.resolve(/* Teiri */Block.__(0, [teiri]));
                }));
  }
}

function views$2(param) {
  if (param.tag) {
    return views$1(param[0]);
  } else {
    return views(param[0]);
  }
}

var $$Object = /* module */[
  /* intro */intro$2,
  /* step */step$2,
  /* views */views$2
];

function intro$3(param) {
  return Promise.all($$Array.map(intro$2, param[/* params */0])).then((function (objects) {
                return Promise.resolve(/* record */[/* objects */objects]);
              }));
}

function step$3(inputs, map) {
  return Promise.all($$Array.map((function (obj) {
                      return step$2(inputs, obj);
                    }), map[/* objects */0])).then((function (objects) {
                return Promise.resolve(/* record */[/* objects */objects]);
              }));
}

function views$3(map) {
  return Promise.all($$Array.map(views$2, map[/* objects */0])).then((function (views) {
                return Promise.resolve($$Array.concat($$Array.to_list(views)));
              }));
}

var test = /* record */[/* params : array */[
    /* TeiriParam */Block.__(0, [/* tuple */[
          32,
          32
        ]]),
    /* BuildingParam */Block.__(1, [/* tuple */[
          64,
          16,
          80,
          80,
          128
        ]]),
    /* BuildingParam */Block.__(1, [/* tuple */[
          160,
          16,
          80,
          80,
          128
        ]])
  ]];

var $$Map = /* module */[
  /* intro */intro$3,
  /* step */step$3,
  /* views */views$3,
  /* test */test
];

function intro$4(param) {
  return intro$3(test);
}

var width = 320;

var height = 240;

var assets = /* array */[
  /* tuple */[
    "pixelart/teiri/walk.png",
    /* array */[
      /* tuple */[
        "pixelart/teiri/walk/left/0.png",
        /* tuple */[
          0,
          0,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/left/1.png",
        /* tuple */[
          16,
          0,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/left/2.png",
        /* tuple */[
          32,
          0,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/left/3.png",
        /* tuple */[
          48,
          0,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/back/0.png",
        /* tuple */[
          0,
          16,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/back/1.png",
        /* tuple */[
          16,
          16,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/back/2.png",
        /* tuple */[
          32,
          16,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/back/3.png",
        /* tuple */[
          48,
          16,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/right/0.png",
        /* tuple */[
          0,
          32,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/right/1.png",
        /* tuple */[
          16,
          32,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/right/2.png",
        /* tuple */[
          32,
          32,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/right/3.png",
        /* tuple */[
          48,
          32,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/front/0.png",
        /* tuple */[
          0,
          48,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/front/1.png",
        /* tuple */[
          16,
          48,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/front/2.png",
        /* tuple */[
          32,
          48,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/teiri/walk/front/3.png",
        /* tuple */[
          48,
          48,
          16,
          16
        ]
      ]
    ]
  ],
  /* tuple */[
    "pixelart/teiri/hijack.png",
    /* array */[
      /* tuple */[
        "pixelart/teiri/hijack/left/0.png",
        /* tuple */[
          0,
          0,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/left/1.png",
        /* tuple */[
          32,
          0,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/left/2.png",
        /* tuple */[
          64,
          0,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/left/3.png",
        /* tuple */[
          96,
          0,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/back/0.png",
        /* tuple */[
          0,
          32,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/back/1.png",
        /* tuple */[
          32,
          32,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/back/2.png",
        /* tuple */[
          64,
          32,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/back/3.png",
        /* tuple */[
          96,
          32,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/right/0.png",
        /* tuple */[
          0,
          64,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/right/1.png",
        /* tuple */[
          32,
          64,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/right/2.png",
        /* tuple */[
          64,
          64,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/right/3.png",
        /* tuple */[
          96,
          64,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/front/0.png",
        /* tuple */[
          0,
          96,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/front/1.png",
        /* tuple */[
          32,
          96,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/front/2.png",
        /* tuple */[
          64,
          96,
          32,
          32
        ]
      ],
      /* tuple */[
        "pixelart/teiri/hijack/front/3.png",
        /* tuple */[
          96,
          96,
          32,
          32
        ]
      ]
    ]
  ],
  /* tuple */[
    "pixelart/maptip/building.png",
    /* array */[
      /* tuple */[
        "pixelart/maptip/building/0-0.png",
        /* tuple */[
          0,
          0,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/1-0.png",
        /* tuple */[
          16,
          0,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/2-0.png",
        /* tuple */[
          32,
          0,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/0-1.png",
        /* tuple */[
          0,
          16,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/1-1.png",
        /* tuple */[
          16,
          16,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/2-1.png",
        /* tuple */[
          32,
          16,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/0-2.png",
        /* tuple */[
          0,
          32,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/1-2.png",
        /* tuple */[
          16,
          32,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/2-2.png",
        /* tuple */[
          32,
          32,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/0-3.png",
        /* tuple */[
          0,
          48,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/1-3.png",
        /* tuple */[
          16,
          48,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/2-3.png",
        /* tuple */[
          32,
          48,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/0-4.png",
        /* tuple */[
          0,
          64,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/1-4.png",
        /* tuple */[
          16,
          64,
          16,
          16
        ]
      ],
      /* tuple */[
        "pixelart/maptip/building/2-4.png",
        /* tuple */[
          32,
          64,
          16,
          16
        ]
      ]
    ]
  ]
];

var scale = 2;

exports.Teiri = Teiri;
exports.Building = Building;
exports.$$Object = $$Object;
exports.$$Map = $$Map;
exports.intro = intro$4;
exports.step = step$3;
exports.views = views$3;
exports.scale = scale;
exports.width = width;
exports.height = height;
exports.assets = assets;
/* No side effect */
