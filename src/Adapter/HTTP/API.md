Get User:
  Request:
    GET /api/users

  Responce:
    Success:
      200
      { "uId": "555123"
      , "sId": "777212"
      }

Get LobbyTalbe:
  Request:
    GET /api/lobbytable

  Responce:
    Success:
      200
      { "uId": "555123"
      , "sId": "777212"
      }
  


WEB:
  get "/"
    redirect "/auth"
  
  get "/auth"
    script fetch('/api/users') -- init sessionId
      redirect "/lobby"
  
  get "/lobby"
    script waitOpponent
      redirect "/waitingroom"

    script joinGame
      redirect "/gameroom"

    script startBot
      redirect "/gameroom"



