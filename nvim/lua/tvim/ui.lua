-- vim.g.gruvbox_material_diagnostic_virtual_text = "colored"
-- vim.g.gruvbox_material_dim_inactive_windows = 1
-- vim.g.gruvbox_material_enable_bold = 1
-- vim.g.gruvbox_material_transparent_background = 1
-- vim.g.gruvbox_material_ui_contrast = "high"
-- vim.g.gruvbox_material_better_performance = 1
-- vim.cmd.colorscheme "gruvbox-material"

require("vague").setup { transparent = true }
vim.cmd.colorscheme "vague"

map("n", "<leader>gg", ":Git<cr>4j", "Git")
map("n", "<leader>gPP", ":Git push<cr>", "Git push")
map("n", "<leader>gPU", ":Git push --set-upstream origin<cr>", "Git push -u")
map(
  "n",
  "<leader>gPF",
  ":Git push --force-with-lease<cr>",
  "Git push --force-with-lease"
)

require("nvim-treesitter.configs").setup {
  ensure_installed = "all",
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
    disable = { "json", "zsh" },
  },
  indent = { enable = true },
  autotag = { enable = true },
}
require("treesitter-context").setup { multiline_threshold = 2 }
vim.cmd [[hi TreesitterContextBottom gui=underline]]

local toggleterm = require "toggleterm"
toggleterm.setup {}
map("n", "<leader>tt", ":ToggleTerm size=20<cr>", "toggleterm")
map("n", "<leader>te", ":TermExec cmd='!!'<cr>", "toggleterm")

local quicker = require "quicker"
quicker.setup {}
map("n", "]c", ":cnext<cr>zz", "quicklist next")
map("n", "[c", ":cprev<cr>zz", "quicklist prev")
map("n", "<leader>C", quicker.toggle, "quicklist toggle")
map("n", "<leader>L", function()
  quicker.toggle { loclist = true }
end, "loclist toggle")

require("zen-mode").setup {
  window = { width = 120 },
  plugins = {
    alacritty = { enabled = true, font = "28" },
    tmux = { enabled = true },
  },
}
map("n", "<leader>zz", require("zen-mode").toggle)

require("render-markdown").setup {}
