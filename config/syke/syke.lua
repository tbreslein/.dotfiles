return {
  symlinks = require("symlinks"),
  repos = {
    {
      remote = "git@github.com:tbreslein/aoc_2024.git",
      path = "/Users/tommy/code/aoc_2024",
    },
    {
      remote = "git@github.com:tbreslein/syke.git",
      path = "/Users/tommy/code/syke",
    },
  },
  shell = {
    -- {
    --   cmd = { "ls", "/Users/tommy" },
    --   hook = { when = "before", what = "ln" },
    -- },
    -- {
    --   cmd = { "ls", "/Users/tommy/.dotfiles" },
    --   hook = { when = "after", what = "repos" },
    -- },
    -- {
    --   cmd = { "nvim", "--version" },
    --   hook = { when = "after", what = "main" },
    -- },
  },
}
