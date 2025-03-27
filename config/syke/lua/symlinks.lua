local home = os.getenv("HOME")
local uname = io.popen("uname -s", "r"):read("*l")
local hostname_file = io.open("/etc/hostname", "r")
local hostname = ""
if hostname_file ~= nil then
  hostname = hostname_file:read("*l")
  hostname_file:close()
end
if hostname == "" and uname == "Darwin" then
  hostname = "darwin"
end

local myconfig = home .. "/.dotfiles/config"
local userconfig = home .. "/.config"
local myscripts = home .. "/.dotfiles/scripts"
local localbin = home .. "/.local/bin"

local symlinks = {
  { source = myconfig .. "/syke", target = userconfig .. "/syke" },
  { source = myconfig .. "/ghostty/config", target = userconfig .. "/ghostty/config" },
  { source = myconfig .. "/ghostty/" .. hostname, target = userconfig .. "/ghostty/host" },
  { source = myconfig .. "/ghostty/gruvbox-material", target = userconfig .. "/ghostty/colors" },
  { source = myconfig .. "/editorconfig", target = home .. "/.editorconfig" },
  { source = myconfig .. "/nix.conf", target = userconfig .. "/nix/nix.conf" },
  { source = myconfig .. "/tmux.conf", target = userconfig .. "/tmux/tmux.conf" },
  { source = myconfig .. "/direnv.toml", target = userconfig .. "/direnv/direnv.toml" },
  { source = myconfig .. "/starship.toml", target = userconfig .. "/starship.toml" },
  { source = myconfig .. "/zshenv", target = home .. "/.zshenv" },
  { source = myconfig .. "/zsh", target = userconfig .. "/zsh" },
  { source = myconfig .. "/git", target = userconfig .. "/git" },
  { source = myconfig .. "/nvim", target = userconfig .. "/nvim" },
  { source = myconfig .. "/emacs/emacs.org", target = home .. "/.emacs.d/emacs.org" },
  { source = myconfig .. "/emacs/init.el", target = home .. "/.emacs.d/init.el" },
  { source = myconfig .. "/emacs/early-init.el", target = home .. "/.emacs.d/early-init.el" },
  { source = myconfig .. "/zed/settings.json", target = userconfig .. "/zed/settings.json" },
  { source = myconfig .. "/zed/keymap.json", target = userconfig .. "/zed/keymap.json" },
  { source = myconfig .. "/luacheckrc", target = home .. "/.luacheckrc" },
  { source = myscripts .. "/tmux_sessionizer", target = localbin .. "/tmux_sessionizer" },
  { source = myscripts .. "/git_status", target = localbin .. "/git_status" },
  { source = myscripts .. "/dm", target = localbin .. "/dm" },
}

local host_symlinks = {}
if hostname == "kain" then
  host_symlinks = {
    { source = myconfig .. "/electron", target = userconfig .. "/electron" },
    { source = myconfig .. "/electron13", target = userconfig .. "/electron13" },
    { source = myconfig .. "/electron-flags.conf", target = userconfig .. "/electron-flags.conf" },
    { source = myconfig .. "/electron13-flags.conf", target = userconfig .. "/electron13-flags.conf" },
  }
elseif uname == "Darwin" then
  host_symlinks = {}
end

for _, sl in ipairs(host_symlinks) do
  symlinks[#symlinks + 1] = sl
end

return symlinks
