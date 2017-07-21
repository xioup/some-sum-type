import * as R           from "ramda"
import { create, env }  from "sanctuary"
import $                from "sanctuary-def"

import SumType          from "../index"

const S = create( { checkTypes: true, env } )
const def = $.create( { checkTypes: true, env } )

const log = R.tap( console.log )

//region Definitions
//    Point :: Type
const Point = $.Pair( $.ValidNumber, $.ValidNumber )

//    Shape :: Type
const Shape =
  SumType( 'Shape'
         , 'url'
         , [ { tag: 'Circle'
             , type: $.RecordType( { origin: Point, radius: $.ValidNumber } )
             , fns: { area: ( { radius } ) => Math.PI * radius * radius }
             }
           , { tag: 'Rectangle'
             , type: $.RecordType( { origin: Point, dimensions: Point } )
             , fns: { area: ( { dimensions : [ w, h ] } ) => w * h }
             }
           ]
         , { move: { sig: sumType => [ Point, sumType, sumType ]
                   , defaultFn:
                      ( [ dx, dy ], shape ) =>
                        R.over( R.lensProp( 'origin' )
                              , ( [ x, y ] ) =>
                                  [ x + dx, y + dy ]
                              , shape
                              )
                   }
           , area: { sig: sumType => [ sumType, $.ValidNumber ] }
           }
         )

const Offer =
  SumType( 'Offer'
         , 'url'
         , [ { tag: 'NeverSaved'
             , type: $.RecordType( { id: $.Null } )
             }
           , { tag: 'Saved'
             , type: $.RecordType( { id: $.PositiveInteger } )
             }
           , { tag: 'Offered'
             , type: $.RecordType( { offered: $.Date } )
             }
           , { tag: 'Accepted'
             , type: $.RecordType( { accepted: $.Date } )
             }
           ]
         , {}
         )

const Deal =
  SumType( 'Deal'
         , 'url'
         , [ { tag: 'NeverSaved'
             , type: $.RecordType( { id: $.Null } )
             }
           , { tag: 'Saved'
             , type: $.RecordType( { id: $.PositiveInteger } )
             }
           , { tag: 'Offered'
             , type:
                 y =>
                   typeof
                     R.find( R.o( y =>
                                    typeof y !== 'undefined'
                                    && $.Date.validate( y.offered )
                                )
                                ( R.prop( 'proposals' ) )
                                ( y )
                           )
                     !== 'undefined'
             }
           ]
         , {}
         )

const Bool =
  UntaggedSumType( 'Bool'
                 , 'url'
                 , [ true, false ]
                 )

const IntegerOrString =
  UntaggedSumType( 'IntegerOrString'
                 , 'url'
                 , [ $.Integer, $.String ]
                 )

const RegexFlags =
  UntaggedSumType( 'RegexFlags'
                 , 'url'
                 , [ $.EnumType( 'Standard', 'url', [ 'g', 'i', 'm' ] )
                   , $.EnumType( 'Experimental', 'url', [ 'u', 'y' ] )
                   ]
                 )
//endregion

// region Checks
const PointChecks =
  () =>
  (
    { '0 Point num num': Point.validate( [ 1, 2 ] ).isRight
    , '1 Point num str': Point.validate( [ 1, 'a' ] ).isRight
    , '2 Point num num num': Point.validate( [ 1, 2, 3 ] ).isRight
    }
  )

const ShapeChecks =
  () =>
  (
    { z: ','
    , '0 Shape': Shape
    , '1 Inf Circle': Shape.toShape( { origin: [ 1, 2 ], radius: 3 } )
    , '2 Inf Rectangle': Shape.toShape( { origin: [ 0, 0 ], dimensions: [ 5, 5 ] } )
    //, '3 Inf NoMatch_1': Shape.toShape( [ [ 1, 2 ] ] )
    //, '4 Inf NoMatch_2': Shape.toShape( [ [ 1, 2 ], [ true ] ] )
    }
  )

const dealIdentity =
  def( 'dealIdentity', {}, [ Deal.Deal, Deal.Deal ], x => x )
const DealChecks =
  () =>
  (
    { z: ','
    , '0 Inf DealNeverSaved': Deal.toDeal( { id: null } )
    , '1 Inf DealSaved': Deal.toDeal( { id: 1234 } )
    // , '2 Inf DealNoMatch': Deal.toDeal( { id: 'a' } )
    , '3 DealNeverSaved': Deal.NeverSaved( { id: null } )
    , '4 DealSaved': Deal.Saved( { id: 1234 } )
    //, '5 DealNoMatch': Deal.Saved( { id: 'a' } )
    , '6 dealIdentity NeverSaved bare': dealIdentity( { id: null } )
    , '7 dealIdentity Saved bare': dealIdentity( { id: 1234 } )
    // , '8 dealIdentity Reg bare': dealIdentity( { id: 'a' } ) // this breaks (as it should)
    , '9 dealIdentity NeverSaved constr': dealIdentity( Deal.toDeal( { id: null } ) )
    , '9_ dealIdentity Reg constr': dealIdentity( Deal.toDeal( { id: 1234 } ) )
    }
  )

const BoolChecks =
  () =>
  (
    { '0 BoolTrue': Bool( true )
    , '1 BoolFalse': Bool( false )
    , '2 BoolNoMatch': Bool( 1 )
    }
  )

const IntegerOrStringChecks =
  () =>
  (
    { IntegerOrStringInt: IntegerOrString( 1 )
    , IntegerOrStringString: IntegerOrString( 'a string' )
    , IntegerOrStringBoolean: IntegerOrString( true )
    }
  )

const RegexFlagsChecks =
  () =>
  (
    { G: RegexFlags( 'g' )
    , Y: RegexFlags( 'y' )
    , Z: RegexFlags( 'z' )
    }
  )

if ( typeof window !== 'undefined' ) {
  window.$ = $
  window.S = S
  window.def = def
  window.Shape = Shape
  window.circle = Shape.toShape( { origin: [ 1, 2 ], radius: 3 } )
  window.rectangle = Shape.toShape( { origin: [ 0, 0 ], dimensions: [ 5, 5 ] } )
  window.Offer = Offer
  window.Deal = Deal
  window.dealNeverSaved = Deal.toDeal( { id: null } )
}
//endregion

console.group( 'union-types' )
console.log( 'PointChecks', PointChecks() )
console.log( 'ShapeChecks', ShapeChecks() )
console.log( 'DealChecks', DealChecks() )
//console.log( 'DealNew_tag', Deal.toDeal( { id: null } ).tag )
console.log( 'BoolChecks', BoolChecks() )
console.log( 'IntegerOrStringChecks', IntegerOrStringChecks() )
console.log( 'RegexFlagsChecks', RegexFlagsChecks() )
//console.log( 'validate', Union.validate( Shape.inferShape( 1, 2, 3 ) ) )
//console.log( 'Right', S.Right(1) )
console.log( 'Real validate', $.Integer.validate( 1 ) )
console.groupEnd()

/*
R.identity( { z: ','
            , '00 PointChecks': PointChecks()
            , '05 ShapeChecks': ShapeChecks()
            , '10 DealChecks': DealChecks()
            //, '20 DealNew_tag': Deal.toDeal( { id: null } ).tag
            , '30 BoolChecks': BoolChecks()
            , '50 IntegerOrStringChecks': IntegerOrStringChecks()
            , '60 RegexFlagsChecks': RegexFlagsChecks()
            //, '80 validate': Union.validate( Shape.inferShape( 1, 2, 3 ) )
            //, '90 Right': S.Right(1)
            }
          )
*/
