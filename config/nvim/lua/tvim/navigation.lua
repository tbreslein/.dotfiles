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

  require("mini.move").setup()

  require("mini.files").setup()
  Map("n", "<leader>fp", function()
    MiniFiles.open(vim.api.nvim_buf_get_name(0))
  end)

  add("cbochs/grapple.nvim")
  require("grapple").setup()
  Map("n", "<leader>a", "<cmd>Grapple tag<cr>")
  Map("n", "<leader>e", "<cmd>Grapple toggle_tags<cr>")
  Map("n", "<A-r>", "<cmd>Grapple select index=1<cr>")
  Map("n", "<A-e>", "<cmd>Grapple select index=2<cr>")
  Map("n", "<A-w>", "<cmd>Grapple select index=3<cr>")
  Map("n", "<A-q>", "<cmd>Grapple select index=4<cr>")
end)
