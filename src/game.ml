open Telegram.Api

let (@<<) f g x = f (g x)
let (@>>) f g x = g (f x)
let (@/>) c1 c2 = Command.Chain (c1, c2)

let create_button text =
  InlineKeyboardButton.create ~text ~callback_data:(Some text) ()

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

let skill_mod_of_string = function
  | "Strength" -> fun skill -> {skill with strength = skill.strength + 1}
  | "Toughness" -> fun skill -> {skill with toughness = skill.toughness + 1}
  | "Stealth" -> fun skill -> {skill with stealth = skill.stealth + 1}
  | "Wisdom" -> fun skill -> {skill with wisdom = skill.wisdom + 1}
  | _ -> fun skill -> skill

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

type player_state = WaitForClass of (class_ -> skills -> unit)
                  | WaitForSkills of int * (skills -> skills) * (skills -> unit)
                  | Ready

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
      let on_stats_picked class_ skills =
        let player = {user; xp = 0; class_; skills; skill_points = 0} in
        Hashtbl.replace games id {players = player::game.players} in
      Hashtbl.replace player_states (user.id) (WaitForClass on_stats_picked);
      let keyboard =
        ReplyMarkup.create_inline_keyboard_markup ~inline_keyboard:[List.map create_button ["Rogue"; "Mage"; "Paladin"]] () in
      Command.SendMessage (id, "Pick a class...", false, None, Some keyboard)
    end
  | None -> Command.SendMessage (id, "Couldn't add you to the game...", false, None, None)

let make_skill_ui chat_id =
  let keyboard =
    ReplyMarkup.create_inline_keyboard_markup
      ~inline_keyboard:[List.map create_button ["Strength"; "Toughness"; "Stealth"; "Wisdom"]] () in
  Command.SendMessage (chat_id, "Skill points remaining: 5", false, None, Some keyboard)

let callback (callback : CallbackQuery.callback_query) =
  let open CallbackQuery in
  let open User in
  let open Message in
  let chat_id =
    let open Chat in
    match callback.message with
    | Some msg -> msg.chat.id
    | None -> 0 in
  let id = callback.from.id in
  match Hashtbl.find player_states id with
  | WaitForClass fn ->
    let finish = fn (class_of_string callback.data) in
    let pstate = WaitForSkills (5, (fun skills -> skills), finish) in
    Hashtbl.replace player_states id pstate;
    Command.Chain
      (make_skill_ui chat_id,
       Command.AnswerCallbackQuery (callback.id, None, false))
  | WaitForSkills (1, fn, finish) ->
    let base = {strength = 0; toughness = 0; stealth = 0; wisdom = 0} in
    let skills = (fn @>> (skill_mod_of_string callback.data)) base in
    finish skills;
    Hashtbl.replace player_states id Ready;
    let mid = callback.message |> function Some msg -> msg.message_id | _ -> 0
    and cid = callback.message |> (function Some {chat} -> Chat.(chat.id) | _ -> 0) |> string_of_int in
    Command.SendMessage (id, "Sucess! You're now part of the game!", false, None, None)
    @/> Command.AnswerCallbackQuery (callback.id, None, false)
    @/> Command.EditMessageText (`ChatMessageId (cid, mid), "No skill points remaining", None, false, None)
  | WaitForSkills (n, fn, finish) ->
    let pstate = WaitForSkills (n - 1, fn @>> (skill_mod_of_string callback.data), finish) in
    Hashtbl.replace player_states id pstate;
    let mid = callback.message |> function Some msg -> msg.message_id | _ -> 0
    and cid = callback.message |> (function Some {chat} -> Chat.(chat.id) | _ -> 0) |> string_of_int
    and skillpoints = "Skill points remaining: " ^ string_of_int (n - 1)
    and keyboard = ReplyMarkup.create_inline_keyboard_markup
        ~inline_keyboard:[List.map create_button ["Strength"; "Toughness"; "Stealth"; "Wisdom"]] () in
    Command.EditMessageText (`ChatMessageId (cid, mid), skillpoints, None, false, Some keyboard)
    @/> Command.AnswerCallbackQuery (callback.id, None, false)
  | Ready -> Command.Nothing
