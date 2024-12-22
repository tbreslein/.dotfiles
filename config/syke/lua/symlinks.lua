local home = os.getenv("HOME")
local hostname_file = io.open("/etc/hostname", "r")
local hostname = ""
if hostname_file ~= nil then
  hostname = hostname_file:read("*l")
  hostname_file:close()
end
local uname = io.popen("uname -s", "r"):read("*l")

local myconfig = home .. "/.dotfiles/config"
local userconfig = home .. "/.config"
local myscripts = home .. "/.dotfiles/scripts"
local localbin = home .. "/.local/bin"

local symlinks_strs = {
  myconfig .. "/nvim:" .. userconfig .. "/nvim",
  myconfig .. "/alacritty/alacritty.toml:" .. userconfig .. "/alacritty/alacritty.toml",
  -- myconfig .. "/alacritty/gruvbox-material.toml:" .. userconfig .. "/alacritty/colors.toml",
  myconfig
    .. "/alacritty/kanagawa-paper.toml:"
    .. userconfig
    .. "/alacritty/colors.toml",
  myconfig .. "/editorconfig:" .. home .. "/.editorconfig",
  myconfig .. "/nix.conf:" .. userconfig .. "/nix/nix.conf",
  myconfig .. "/tmux.conf:" .. userconfig .. "/tmux/tmux.conf",
  myconfig .. "/direnv.toml:" .. userconfig .. "/direnv/direnv.toml",
  myconfig .. "/starship.toml:" .. userconfig .. "/starship.toml",
  myconfig .. "/config.fish:" .. userconfig .. "/fish/config.fish",
  myconfig .. "/git:" .. userconfig .. "/git",
  myconfig .. "/luacheckrc:" .. home .. "/.luacheckrc",
  myscripts .. "/tmux_sessionizer:" .. localbin .. "/tmux_sessionizer",
  myscripts .. "/git_status:" .. localbin .. "/git_status",
  myscripts .. "/dm:" .. localbin .. "/dm",
}

local absent_symlinks = {
  myconfig .. "/bash/bash_logout:" .. home .. "/.bash_logout",
  myconfig .. "/bash/bash_profile:" .. home .. "/.profile",
  myconfig .. "/bash/bashrc:" .. home .. "/.bashrc",
  myconfig .. "/bash/inputrc:" .. home .. "/.inputrc",
}

if hostname == "kain" then
  symlinks_strs[#symlinks_strs + 1] = myconfig
    .. "/alacritty/"
    .. hostname
    .. ".toml:"
    .. userconfig
    .. "/alacritty/host.toml"
  symlinks_strs[#symlinks_strs + 1] = myconfig .. "tofi:" .. userconfig .. "/tofi/config"
  symlinks_strs[#symlinks_strs + 1] = myconfig .. "dunstrc:" .. userconfig .. "/dunst/dunstrc"
  local direct_links = {
    "/hypr/hyprland.conf",
    "/hypr/hyprlock.conf",
    "/hypr/hypridle.conf",
    "/hypr/hyprpaper.conf",
    "/waybar",
    "/electron",
    "/electron13",
    "/electron-flags.conf",
    "/electron13-flags.conf",
  }
  for _, x in ipairs(direct_links) do
    symlinks_strs[#symlinks_strs + 1] = myconfig .. x .. ":" .. userconfig .. x
  end
end

if uname == "Darwin" then
  symlinks_strs[#symlinks_strs + 1] = myconfig .. "/alacritty/darwin.toml:" .. userconfig .. "/alacritty/host.toml"
  symlinks_strs[#symlinks_strs + 1] = myconfig .. "/aerospace.toml:" .. userconfig .. "/aerospace/aerospace.toml"
end

local symlinks = {}
for _, sym_str in ipairs(symlinks_strs) do
  local t = {}
  for str in string.gmatch(sym_str, "([^:]+)") do
    table.insert(t, str)
  end
  symlinks[#symlinks + 1] = {
    source = t[1],
    target = t[2],
  }
end

for _, sym_str in ipairs(absent_symlinks) do
  symlinks[#symlinks + 1] = {
    target = sym_str,
    absent = true,
  }
end

return symlinks
