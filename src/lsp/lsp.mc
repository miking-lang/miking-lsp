-- lsp.mc
--
-- This file contains a skeleton for an implementation of an LSP language server
--
-- serverMain is the main function to start the server: currently it reads any
-- LSP messages it receives, parses them, and then prints them back to stdout.
--
-- The intention is for serverMain to take two handlers - one for notifications
-- and one for requests, which it calls on the parsed LSP message to produce a
-- response and/or update the server state. These handlers essentially
-- constitute the language-specific part of the implementation.
--
-- In addition, the messages are currently parsed to JSON-RPC, which the LSP
-- protocol is built upon. However, it would be best to have an additional layer
-- of abstraction with types specifically for LSP, to make the implementation
-- of the handlers more clean.

include "parser.mc"
include "json.mc"
include "json-rpc.mc"
include "utils.mc"

let lexAnything = void (many (satisfy (const true) ""))

type HeaderField
con ContentLength: Int -> HeaderField
con ContentType: () -> HeaderField

let parseHeaderField = lam s.
  let len = (apr (lex_string "Length: ")
                 (fmap (lam x. ContentLength x) lex_number))
  in
  let tpe = (apr (lex_string "Type: ")
                 (fmap (lam x. ContentType x) lexAnything))
  in
  let headerField = (apr (lex_string "Content-")
                    (apl (alt len tpe)
                         end_of_input))
  in match (run_parser "" headerField s) with Success (hdr, pos) then
    Some hdr
  else
    None ()

utest parseHeaderField "Content-Length: 50" with Some (ContentLength 50)
utest parseHeaderField "Content-Type:   sh13ETRNU-upsht!!rntdTEFrty" with Some (ContentType ())
utest parseHeaderField "Content-Length:  50" with None ()
utest parseHeaderField "Content-Length: 50hej" with None ()
utest parseHeaderField " Content-Type: sh13ETRNU-upsht!!rntdTEFrty" with None ()

let matchNewline = lam line.
  match line with "\n" then
    None ()
  else match line with str ++ "\n" then
    Some (str, ())
  else error "Unexpected end of input"

let readHeaderLines = unfoldr (compose matchNewline readLine)

let readBody = lam toRead.
  recursive let f = lam len. lam acc.
    let readResult = readBytesAsString len in
    let new_acc = concat acc readResult.0 in
    if lti readResult.1 len then
      f (subi len readResult.1) new_acc
    else
      new_acc
  in
  f toRead ""

let readRequests =
  let getLength = lam x.
    match x with ContentLength i then
      Some i
    else
      None ()
  in
  -- rpcToLsp: RpcRequest -> NotificationOrRequest
  let rpcToLsp = lam x. Some x in -- Implement me!
  (optionCompose (optionMapM rpcToLsp)
  (optionCompose (processBatch jsonToRequest)
  (optionCompose (compose parseJson readBody)
  (optionCompose (optionFoldMap getLength)
  (compose       (optionMapM parseHeaderField)
                 readHeaderLines)))))

-- processRequests: (Notification -> State -> ((), State)) ->
--                  (Request -> State -> (Response, State)) ->
--                  [NotificationOrRequest] ->
--                  State ->
--                  ([Response], State)
let processRequests = lam _. lam _. lam requestLst. lam state. -- Implement me!
    (join (map (compose formatJson requestToJson) requestLst), state)

-- putResponses: [Response] -> ()
let putResponses = printLn -- Implement me!

recursive
-- handleNotification: Notification -> State -> ((), State)
-- handleRequest: Request -> State -> (Response, State)
let serverMain = lam handleNotification. lam handleRequest. lam state.
  let requests = readRequests () in
  match requests with Some r then
    let result = processRequests handleNotification handleRequest r state in
    let _ = putResponses result.0 in
    serverMain handleNotification handleRequest result.1
  else
    serverMain handleNotification handleRequest state
end
