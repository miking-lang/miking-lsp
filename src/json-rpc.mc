include "json.mc"
include "map.mc"
include "utils.mc"

type Id
con StrId : String -> Id
con IntId : Int    -> Id

type Params
con ByPosition : [ JsonValue ]           -> Params
con ByName     : [ (String, JsonValue) ] -> Params

type RpcError = { code    : Int
                , message : String
                , data    : Option -- JsonValue
                }

type RpcResult
con Success : JsonValue -> RpcResult
con Failure : RpcError  -> RpcResult

type RpcRequest = { method : String
                  , params : Option -- Params
                  , id : Option -- Id
                  }
type RpcResponse = { result : RpcResult
                   , id : Id
                   }

let paramsToJson = lam x.
  match x with ByPosition arr then JsonArray arr
  else match x with ByName obj then JsonObject obj
  else error "paramsToJson: Arg is not of type Params"

let idToJson = lam x.
  match x with StrId s then JsonString s
  else match x with IntId i then JsonInt i
  else error "idToJson: Arg is not of type Id"

let jsonrpc = ("jsonrpc", JsonString "2.0")

let requestToJson = lam x.
  let members =
    optionSnoc
      (optionSnoc
        [ jsonrpc , ("method", JsonString x.method) ]
        (optionMap (lam x. ("params", paramsToJson x)) x.params))
      (optionMap (lam x. ("id", idToJson x)) x.id)
  in
  JsonObject members

let errorToJson = lam x.
  let members =
    optionSnoc [ ("code", JsonInt x.code)
               , ("message", JsonString x.message)
               ]
               (optionMap (lam x. ("data", x)) x.data)
  in
  JsonObject members

let responseToJson = lam x.
  let resultOrError =
    match x.result with Success v then
      ("result", v)
    else match x.result with Failure err then
      ("error", errorToJson err)
    else error "responseToJson: Invalid result"
  in
  let members = [ jsonrpc
                , resultOrError
                , ("id", idToJson x.id)
                ]
  in
  JsonObject members

let jsonToParams = lam x.
  match x with JsonObject o then
    Some (ByName o)
  else match x with JsonArray a then
    Some (ByPosition a)
  else
    None ()

let jsonToId = lam x.
  match x with JsonString s then
    Some (StrId s)
  else match x with JsonInt i then
    Some (IntId i)
  else
    None ()

let getJsonInt = lam x.
  match x with JsonInt i then Some i else None ()

let getJsonString = lam x.
  match x with JsonString s then Some s else None ()

let getObjectMapping = lam x.
  match x with JsonObject arr then
    Some (lam k. mapLookupOpt eqstr k arr)
  else
    None ()

let jsonToRequest = lam x.
  let extractRequest = lam lookup.
    optionBind (optionBind (lookup "method") getJsonString) (lam method.
    optionBind (optionInvertMap jsonToParams (lookup "params")) (lam params.
    optionBind (optionInvertMap jsonToId (lookup "id")) (lam id.
    Some { method = method
         , params = params
         , id = id
         })))
  in optionBind (getObjectMapping x) extractRequest

let jsonToResult = lam x. Some (Success x)

let jsonToError = lam x.
  let extractError = lam lookup.
    optionBind (optionBind (lookup "code") getJsonInt) (lam code.
    optionBind (optionBind (lookup "message") getJsonString) (lam msg.
    Some (Failure { code = code
                  , message = msg
                  , data = lookup "data"
                  })))
  in optionBind (getObjectMapping x) extractError

let jsonToResponse = lam x.
  let extractResponse = lam lookup.
    optionBind (optionOr (optionBind (lookup "result") jsonToResult)
                         (optionBind (lookup "error") jsonToError)) (lam res.
    optionBind (optionBind (lookup "id") jsonToId) (lam id.
    Some { result = res
         , id = id
         }))
  in optionBind (getObjectMapping x) extractResponse

let processBatch = lam jsonToRpc. lam x.
  match x with JsonObject _ then
    optionMapM jsonToRpc [x]
  else match x with JsonArray arr then
    optionMapM jsonToRpc arr
  else
    None ()

mexpr

-- Requests
let testNotification = {method="notify", params=None (), id=None ()} in
let testJsonNotification = JsonObject [ jsonrpc
                                      , ("method", JsonString "notify")
                                      ]
in
utest requestToJson testNotification with testJsonNotification in
utest jsonToRequest testJsonNotification with Some testNotification in

let testRequest = {method="foo", params=None (), id=Some (IntId 42)} in
let testJsonRequest = JsonObject [ jsonrpc
                                 , ("method", JsonString "foo")
                                 , ("id", JsonInt 42)
                                 ]
in
utest requestToJson testRequest with testJsonRequest in
utest jsonToRequest testJsonRequest with Some testRequest in

let testRequestByPosition = {method="add", params=Some (ByPosition [JsonInt 3, JsonInt 10]), id=Some (StrId "myId")} in
let testJsonRequestByPosition =
  JsonObject [ jsonrpc
             , ("method", JsonString "add")
             , ("params", JsonArray [JsonInt 3, JsonInt 10])
             , ("id", JsonString "myId")
             ]
in

utest requestToJson testRequestByPosition with testJsonRequestByPosition in
utest jsonToRequest testJsonRequestByPosition with Some testRequestByPosition in

let testRequestByName = {method="myMethod", params=Some (ByName [("foo", JsonInt 5), ("bar", JsonString "hello")]), id=Some (IntId 20)} in
let testJsonRequestByName =
  JsonObject [ jsonrpc
             , ("method", JsonString "myMethod")
             , ("params", JsonObject [("foo", JsonInt 5), ("bar", JsonString "hello")])
             , ("id", JsonInt 20)
             ]
in

utest requestToJson testRequestByName with testJsonRequestByName in
utest jsonToRequest testJsonRequestByName with Some testRequestByName in

-- Incorrect data
utest jsonToRequest (JsonInt 5) with None () in
let missingMethod = JsonObject [ jsonrpc
                               , ("params", JsonArray [])
                               , ("id", JsonInt 42)
                               ]
in
utest jsonToRequest missingMethod with None () in
let boolId = JsonObject [ jsonrpc
                        , ("method", JsonString "foo")
                        , ("id", JsonBool true)
                        ]
in
utest jsonToRequest boolId with None () in

-- Responses
let testResponseSuccess = {result=Success (JsonInt 13), id=StrId "myId"} in
let testJsonResponseSuccess =
  JsonObject [ jsonrpc
             , ("result", JsonInt 13)
             , ("id", JsonString "myId")
             ]
in

utest responseToJson testResponseSuccess with testJsonResponseSuccess in
utest jsonToResponse testJsonResponseSuccess with Some testResponseSuccess in

let testResponseFailure = {result=Failure {code=negi 32700, message="bar", data=None ()}, id=IntId 42} in
let testJsonResponseFailure =
  JsonObject [ jsonrpc
             , ("error", JsonObject [ ("code", JsonInt (negi 32700))
                                    , ("message", JsonString "bar")
                                    ])
             , ("id", JsonInt 42)
             ]
in

utest responseToJson testResponseFailure with testJsonResponseFailure in
utest jsonToResponse testJsonResponseFailure with Some testResponseFailure in

let testResponseData = {result=Failure {code=negi 32700, message="bar", data=Some (JsonInt 257)}, id=IntId 42} in
let testJsonResponseData =
  JsonObject [ jsonrpc
             , ("error", JsonObject [ ("code", JsonInt (negi 32700))
                                    , ("message", JsonString "bar")
                                    , ("data", JsonInt 257)
                                    ])
             , ("id", JsonInt 42)
             ]
in

utest responseToJson testResponseData with testJsonResponseData in
utest jsonToResponse testJsonResponseData with Some testResponseData in

-- Incorrect data
let missingId =
  JsonObject [ jsonrpc
             , ("result", JsonInt 13)
             ]
in
utest jsonToResponse missingId with None () in
let missingResultAndError =
  JsonObject [ jsonrpc
             , ("id", JsonInt 42)
             ]
in
utest jsonToResponse missingResultAndError with None () in
let missingErrorCode =
  JsonObject [ jsonrpc
             , ("error", JsonObject [ ("message", JsonString "bar") ])
             , ("id", JsonInt 42)
             ]
in
utest jsonToResponse missingErrorCode with None () in
let missingErrorMessage =
  JsonObject [ jsonrpc
             , ("error", JsonObject [ ("code", JsonInt (negi 32700)) ])
             , ("id", JsonInt 42)
             ]
in
utest jsonToResponse missingErrorCode with None () in

-- processBatch
let allJsonRequests = [ testJsonNotification
                      , testJsonRequest
                      , testJsonRequestByName
                      , testJsonRequestByPosition
                      ]
in
let allRequests = [ testNotification
                  , testRequest
                  , testRequestByName
                  , testRequestByPosition
                  ]
in
utest processBatch jsonToRequest (JsonArray allJsonRequests) with Some allRequests in
let allJsonResponses = [ testJsonResponseSuccess
                       , testJsonResponseFailure
                       , testJsonResponseData
                       ]
in
let allResponses = [ testResponseSuccess
                   , testResponseFailure
                   , testResponseData
                   ]
in
utest processBatch jsonToResponse (JsonArray allJsonResponses) with Some allResponses in

utest processBatch jsonToRequest testJsonRequest with Some [testRequest] in
()
