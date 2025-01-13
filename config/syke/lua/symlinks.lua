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

local symlinks = {
  { source = myconfig .. "/syke", target = userconfig .. "/syke" },
  { source = myconfig .. "/nvim", target = userconfig .. "/nvim" },
  { source = myconfig .. "/alacritty/alacritty.toml", target = userconfig .. "/alacritty/alacritty.toml" },
  -- { source = myconfig .. "/alacritty/kanagawa-paper.toml", target = userconfig .. "/alacritty/colors.toml" },
  { source = myconfig .. "/alacritty/gruvbox-material.toml", target = userconfig .. "/alacritty/colors.toml" },
  { source = myconfig .. "/editorconfig", target = home .. "/.editorconfig" },
  { source = myconfig .. "/nix.conf", target = userconfig .. "/nix/nix.conf" },
  { source = myconfig .. "/tmux.conf", target = userconfig .. "/tmux/tmux.conf" },
  { source = myconfig .. "/direnv.toml", target = userconfig .. "/direnv/direnv.toml" },
  { source = myconfig .. "/starship.toml", target = userconfig .. "/starship.toml" },
  { source = myconfig .. "/config.fish", target = userconfig .. "/fish/config.fish" },
  { source = myconfig .. "/git", target = userconfig .. "/git" },
  { source = myconfig .. "/luacheckrc", target = home .. "/.luacheckrc" },
  { source = myscripts .. "/tmux_sessionizer", target = localbin .. "/tmux_sessionizer" },
  { source = myscripts .. "/git_status", target = localbin .. "/git_status" },
  { source = myscripts .. "/dm", target = localbin .. "/dm" },
}

local host_symlinks = {}
if hostname == "kain" then
  host_symlinks = {
    { source = myconfig .. "/alacritty/" .. hostname .. ".toml", target = userconfig .. "/alacritty/host.toml" },
    { source = myconfig .. "/electron", target = userconfig .. "/electron" },
    { source = myconfig .. "/electron13", target = userconfig .. "/electron13" },
    { source = myconfig .. "/electron-flags.conf", target = userconfig .. "/electron-flags.conf" },
    { source = myconfig .. "/electron13-flags.conf", target = userconfig .. "/electron13-flags.conf" },
  }
elseif uname == "Darwin" then
  host_symlinks = {
    { source = myconfig .. "/alacritty/darwin.toml", target = userconfig .. "/alacritty/host.toml" },
    { source = myconfig .. "/alacritty/darwin.toml", target = userconfig .. "/alacritty/host.toml" },
    { source = myconfig .. "/aerospace.toml", target = userconfig .. "/aerospace/aerospace.toml" },
  }
end

for _, sl in ipairs(host_symlinks) do
  symlinks[#symlinks + 1] = sl
end

return symlinks
