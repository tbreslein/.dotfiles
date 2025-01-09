local home = os.getenv("HOME")
return {
  symlinks = require("symlinks"),
  repos = {
    {
      remote = "git@github.com:tbreslein/aoc_2024.git",
      path = home .. "/code/aoc_2024",
    },
    {
      remote = "git@github.com:tbreslein/syke.git",
      path = home .. "/code/syke",
    },
    {
      remote = "git@github.com:tbreslein/fibis.git",
      path = home .. "/code/fibis",
    },
  },
  shell = {
    {
      cmd = { "ls", "/" },
      hook = { when = "after", what = "main" },
    },
    -- {
    --   cmd = { "nvim", "--headless", [[+Lazy! sync]], [[+TSUpdateSync]], "+qa" },
    -- },
  },
}
