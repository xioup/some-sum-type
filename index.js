import * as R          from "ramda"
import { create, env } from "sanctuary"
import $               from "sanctuary-def"

const S = create( { checkTypes: true, env } )
const def = $.create( { checkTypes: true, env } )

const log = R.tap( console.log )

const isType =
  x =>
    $.Type.validate( x ).isRight === true

const isOfTypeOrEqual =
    x => t =>
      ( isType( t ) && t.validate( x ).isRight === true )
      || ( R.is( Function, t ) && t( x ) === true )
      || t === x

const _SumType =
  ( name, url, cases, sharedFns ) =>
    { const isConstructed =
        x =>
          x[ '@@type' ] === name

      const typePredicate =
        x =>
          isConstructed( x )
          || R.reduce( ( _, kase ) =>
                         isOfTypeOrEqual( x )( kase.type ) === true
                           ? R.reduced( true )
                           : false
                     , false
                     , cases
                     )

      const _type =
        $.NullaryType( name
                     , url
                     , typePredicate
                     )

      const getValue =
        x =>
          isConstructed( x )
            ? x.value
            : x

      const _allCasesFnsOrig =
        R.reduce( ( acc, { tag, fns } ) =>
                    R.assoc( tag, fns, acc )
                , {}
                )
                ( cases )

      const _allFnNames =
        R.pipe( R.keys
              , R.reduce( ( acc, x ) =>
                            R.concat( R.keys( _allCasesFnsOrig[ x ] )
                                    , acc
                                    )
                        , []
                        )
              , R.concat( R.keys( sharedFns ) )
              , R.uniq
              )
              ( _allCasesFnsOrig )

      const _assignFn =
        fnName =>
          R.reduce( ( acc, kase ) =>
                      R.ifElse( R.is( Function )
                              , x => R.assocPath( [ kase.tag, fnName ], x, acc )
                              , _ =>
                                  { throw new TypeError( `No '${ fnName }' function defined on case '${ kase.tag }' or default.` ) }
                              )
                              ( kase.fns[ fnName ] || sharedFns[ fnName ].defaultFn )
                  , {}
                  )
                  ( cases )

      const _allCasesFns =
        R.reduce( ( acc, x ) =>
                    R.mergeDeepLeft( _assignFn( x ), acc )
                , {}
                )
                ( _allFnNames )

      const _dispatchFn =
        fnName =>
          { const sig =
              R.pathOr( R.identity, [ fnName, 'sig' ], sharedFns )( _type )
            if ( !$.test( [], $.Array( $.Type ), sig ) )
              { throw new TypeError( `Missing or invalid signature for function '${ fnName }' on '${ name }'.` ) }
            // This gets us the index of the last __input__ of our type. We use this to determine
            // whether or not to return a constructed value if the return value is of our type.
            const typeArgIndex =
              R.findLastIndex( R.equals( _type ), R.init( sig ) )
            // If we return a value of our type and the last __input__ of our type is constructed,
            // we return a constructed value, otherwise a bare value.
            const returnsOurType = R.equals( _type, R.last( sig ) )
            const fnLength = sig.length - 1
            return (
              ( ...args ) =>
                { const typeArg = R.prop( typeArgIndex, args )
                  const typeArgIsConstructed = isConstructed( typeArg )
                  const tag = R.prop( 'tag', _of( typeArg ) )
                  const fn =
                    def( fnName, {}, sig, _allCasesFns[ tag ][ fnName ] )
                  const prepArgs =
                    arg =>
                      R.equals( tag, arg.tag )
                        ? getValue( arg )
                        : arg
                  const almostThere = fn( ...R.map( prepArgs, args ) )
                  return (
                    typeArgIsConstructed
                    && returnsOurType
                      ? _of( almostThere )
                      : almostThere
                  )
                }
            )
          }

      const _makeSharedFns =
        fnName =>
          [ fnName
          , _dispatchFn( fnName )
          ]

      const _sharedFns =
        _allFnNames.map( _makeSharedFns )

      const _makeMethods =
        x =>
          R.pipe( //log,
                    R.map( y =>
                             [ y[ 0 ]
                             , ( ...z ) =>
                                 y[ 1 ]( ...R.append( _of( x ), z ) )
                             ]
                         )
                  , R.fromPairs
                  )( _sharedFns )

      //log( _sharedFns )

      // returns all tags that an input value 'has' (could have)
      const allTags =
        x =>
          R.is( Array, x.allTags )
          ? x.allTags
          : R.reduce(
              ( acc, kase ) =>
                isOfTypeOrEqual( getValue( x ) )( kase.type ) === true
                  ? R.append( kase.tag, acc )
                  : acc
              , []
              , cases
            )

      const _tagIt =
        def( 'tagIt'
           , {}
           , [ $.Object, _type, _type ]
           , ( kase, x ) =>
               (
                 { ..._makeMethods( x )
                 , name
                 , url
                 , value: getValue( x )
                 , [ 'is' + kase.tag ]: true
                 , tag: kase.tag
                 , allTags: allTags( x )
                 , '@@type': name
                 }
               )
           )

      const _of =
        x =>
          R.reduce( ( _, kase ) =>
                      isOfTypeOrEqual( getValue( x ) )( kase.type ) === true
                        ? R.reduced( _tagIt( kase )( x ) )
                        : false
                  , false
                  , cases
                  )
      const of =
        def( 'of', {}, [ $.Any, _type ], _of )

      const allCaseTags =
        R.pluck( 'tag', cases )

      const is =
        ( tagName, x ) =>
          !R.contains( tagName, allCaseTags )
          ? S.Left( `Sum Type '${ name }' does not have a tag named '${ tagName }'.` )
          : x.tag === tagName
            || R.contains( tagName, allTags( x ) )
              ? S.Right( x )
              : S.Left( `Input does not have the '${ tagName }' tag.` )

      const hasTags =
        ( tagNames, x ) =>
          R.reduce(
            ( _, tagName ) =>
              is( tagName, x ).isLeft
                ? R.reduced( S.Left( `Input doesn't contain all specified tags.` ) )
                : S.Right( x )
            , false
            , tagNames
          )

      const _makeCaseConstructors =
        kase =>
          typeof kase.valueConstructor !== 'undefined'
            ? [ kase.tag
              , kase.valueConstructor
              ]
            : [ kase.tag
              , x =>
                  isOfTypeOrEqual( x )( kase.type )
                  ? _tagIt( kase )( x )
                  : log( S.Left( 'invalid value - need to fix this in _makeCaseConstructors' ) )
              ]

      const _makeCaseTypes =
        kase =>
          [ kase.tag + 'Type'
          , $.NullaryType( kase.tag
                         , url
                         , x => is( kase.tag, x )
                         )
          ]

      return (
        { [ name ]: _type // 'this type matches constructed and inferred values'
        , [ 'to' + name ]: of
        , isConstructed
        , getValue
        , allTags
        , is
        , hasTags
        , ...R.fromPairs( cases.map( _makeCaseConstructors ) )
        , ...R.fromPairs( cases.map( _makeCaseTypes ) )
        , ...R.fromPairs( _sharedFns )
        }
      )
    }

export const SumType =
  def( 'SumType'
     , {}
     , [ $.String, $.String, $.Array( $.Object ), $.Nullable( $.Object ), $.Object ]
     , _SumType
     )

//region Tuple and UntaggedSumType
export const UntaggedSumType =
  ( name, url, cases ) =>
    x =>
      R.reduce( ( _, kase ) =>
                  isOfTypeOrEqual( x )( kase ) === true
                    ? R.reduced( x )
                    : S.Left( 'No match' )
              , false
              , cases
              )

const validateTupleEntries =
  R.reduce( ( _, [ x, type ] ) =>
              isOfTypeOrEqual( x )( type ) === true
              || R.reduced( false )
          , false
          )

const validateTuple =
  xs => types =>
    R.when( ts => ts.length === xs.length
          , R.o( validateTupleEntries )
               ( R.zip( xs ) )
          )
          ( types )

export const TupleType =
  ( name, url, types ) =>
      $.NullaryType( name
                   , url
                   , ( ...x ) =>
                       validateTuple( x )( types ) === true
                   )
//endregion
