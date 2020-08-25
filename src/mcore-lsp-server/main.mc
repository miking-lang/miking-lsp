include "../lsp/lsp.mc"

let handleNotification = lam _. ()
let handleRequest = lam _. ()

mexpr

serverMain handleNotification handleRequest ()
