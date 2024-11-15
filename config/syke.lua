local home = os.getenv("HOME")

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
