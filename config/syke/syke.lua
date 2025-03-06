local home = os.getenv("HOME")
return {
  symlinks = require("symlinks"),
  repos = {
    {
      remote = "git@github.com:tbreslein/syke.git",
      path = home .. "/code/syke",
    },
    {
      remote = "git@github.com:tbreslein/clg.git",
      path = home .. "/code/clg",
    },
    {
      remote = "git@github.com:tbreslein/frankenrepo.git",
      path = home .. "/code/frankenrepo",
    },
    {
      remote = "git@github.com:tbreslein/shyr.git",
      path = home .. "/code/shyr",
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
