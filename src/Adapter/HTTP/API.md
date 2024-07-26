
Login:
  Request:
    POST /api/auth/login
    { "guestUserId":"123"
    , "guestPassword":"abcDEF123"   
    }
  Responce:
    Invalid auth:
      400
      "InvalidAuth"
      { "guestUserId":"123"
      , "guestPassword":"abcDEF123"   
      }

    Success:
      200