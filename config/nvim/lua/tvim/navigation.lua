later(function()
  add("aserowy/tmux.nvim")
  require("tmux").setup()

  add("ibhagwan/fzf-lua")
  local fzflua = require("fzf-lua")
  fzflua.setup({
    winopts = {
      border = vim.g.borderstyle,
      preview = { layout = "vertical" },
    },
    fzf_opts = { ["--layout"] = false },
  })
  Map("n", "<leader>ff", fzflua.files)
  Map("n", "<leader>fs", fzflua.live_grep)

  require("mini.files").setup()
  Map("n", "<leader>fp", MiniFiles.open)

  add({
    source = "ThePrimeagen/harpoon",
    checkout = "harpoon2",
    depends = { "nvim-lua/plenary.nvim" },
  })
  local harpoon = require("harpoon")
  harpoon:setup()

  vim.keymap.set("n", "<leader>a", function()
    harpoon:list():add()
  end)
  vim.keymap.set("n", "<leader>e", function()
    harpoon.ui:toggle_quick_menu(harpoon:list())
  end)
  vim.keymap.set("n", "<A-r>", function()
    harpoon:list():select(1)
  end)
  vim.keymap.set("n", "<A-e>", function()
    harpoon:list():select(2)
  end)
  vim.keymap.set("n", "<A-w>", function()
    harpoon:list():select(3)
  end)
  vim.keymap.set("n", "<A-q>", function()
    harpoon:list():select(4)
  end)
  vim.keymap.set("n", "<A-t>", function()
    harpoon:list():select(5)
  end)
end)
