open Telegram.Api

type class_ = Rogue | Mage | Paladin

let class_of_string = function
  | "Rogue" -> Rogue
  | "Mage" -> Mage
  | "Paladin" -> Paladin
  | _ -> assert false

type skills = {
  strength : int;
  toughness : int;
  stealth : int;
  wisdom : int
}

type player = {
  user : User.user;
  xp : int;
  class_ : class_;
  skills : skills;
  skill_points : int
}

type game_state = {
  players : player list
}

let games : (int, game_state) Hashtbl.t =
  Hashtbl.create 10

let start message =
  let open Message in
  let open Chat in
  let id = message.chat.id in
  Hashtbl.add games id {players = []};
  Command.SendMessage (id, "Starting new game... type /join to play", false, None, None)

type player_state = WaitForClass of (class_ -> unit) | Ready

let player_states : (int, player_state) Hashtbl.t =
  Hashtbl.create 100

let join message =
  let open Message in
  let open Chat in
  let open User in
  let id = message.chat.id in
  match message.from with
  | Some user -> begin
      let game = Hashtbl.find games id in
      let on_class_picked class_ =
        let skills = {strength = 0; toughness = 0; stealth = 0; wisdom = 0} in
        let player = {user; xp = 0; class_; skills; skill_points = 0} in
        Hashtbl.replace games id {players = player::game.players} in
      Hashtbl.replace player_states (user.id) (WaitForClass on_class_picked);
      let keyboard =
        let create_button text =
          InlineKeyboardButton.create ~text ~callback_data:(Some "class_selection") () in
        ReplyMarkup.create_inline_keyboard_markup ~inline_keyboard:[List.map create_button ["Rogue"; "Mage"; "Paladin"]] () in
      Command.SendMessage (id, "Pick a class...", false, None, Some keyboard)
    end
  | None -> Command.SendMessage (id, "Couldn't add you to the game...", false, None, None)

let callback (callback : CallbackQuery.callback_query) =
  let open CallbackQuery in
  let open User in
  let open Message in
  let id = callback.from.id in
  match Hashtbl.find player_states id with
  | WaitForClass fn
    when callback.data = "class_selection" -> begin
      begin match callback.message with
        | Some {text = Some choice} -> fn (class_of_string choice)
        | _ -> ()
      end;
      Hashtbl.replace player_states id Ready;
      Command.Chain
        (Command.SendMessage (id, "Sucess! You're now part of the game!", false, None, None),
         Command.AnswerCallbackQuery (callback.id, None, false))
    end
  | _ -> Command.Nothing
