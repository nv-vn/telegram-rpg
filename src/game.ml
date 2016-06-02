open Telegram.Api

type game_state = {
  players : User.user list
}

let games : (int, game_state) Hashtbl.t =
  Hashtbl.create 100

let start message =
  let open Message in
  let open Chat in
  let id = message.chat.id in
  Hashtbl.add games id {players = []};
  Command.SendMessage (id, "Starting new game... type /join to play", false, None, None)

let join message =
  let open Message in
  let open Chat in
  let open User in
  let id = message.chat.id in
  match message.from with
  | Some user -> begin
      let game = Hashtbl.find games id in
      Hashtbl.replace games id {players = user::game.players};
      Command.SendMessage (id, "Sucess! You're now part of the game!", false, None, None)
    end
  | None -> Command.SendMessage (id, "Couldn't add you to the game...", false, None, None)
