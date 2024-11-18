local home = os.getenv("HOME")
local hostname_file = io.open("/etc/hostname", "r")
local hostname = ""
if hostname_file ~= nil then
  hostname = hostname_file.read()
  hostname_file:close()
end

return {
  symlinks = {
    {
      source = home .. "/.dotfiles/config/alacritty/alacritty.toml",
      target = home .. "/.config/alacritty/alacritty.toml",
    },
    {
      source = home .. "/.dotfiles/config/direnv.toml",
      target = home .. "/.config/direnv/direnv.toml",
    },
  },
}
