open Telegram
open TelegramDashboard

type bot_config = {
  name : string;
  token : string
}

type config = {
  bot : bot_config
}

let config =
  let path = "./rpg.toml" in
  match Toml.Parser.from_filename path with
  | `Ok toml -> begin
      let open TomlLenses in
      let bot_name = get toml (key "bot" |-- table |-- key "name" |-- string)
      and bot_token = get toml (key "bot" |-- table |-- key "token" |-- string) in
      match bot_name, bot_token with
      | (Some name, Some token) -> {bot = {name; token}}
      | _ -> assert false
    end
  | `Error _ -> assert false

module Rpg = MkDashboard (struct
    include BotDefaults

    let token = config.bot.token
    and command_postfix = Some config.bot.name

    let commands =
      let open Api.Command in
      [{name = "start"; description = "Start a new game"; enabled = true; run = Game.start};
       {name = "join"; description = "Join an existing game"; enabled = true; run = Game.join}]
  end)

let () = Rpg.run ()
