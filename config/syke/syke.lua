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
  },
  shell = {
    -- {
    --   cmd = { "nvim", "--headless", [[+Lazy! sync]], [[+TSUpdateSync]], "+qa" },
    --   hook = { when = "after", what = "main" },
    -- },
  },
}
