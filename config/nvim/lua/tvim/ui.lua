Now(function()
  Add("sainnhe/gruvbox-material")
  vim.g.gruvbox_material_enable_italic = 1
  vim.g.gruvbox_material_enable_bold = 1
  vim.g.gruvbox_material_better_performance = 1
  vim.g.gruvbox_material_ui_contrast = "high"
  vim.g.gruvbox_material_diagnostic_virtual_text = "highlighted"
  vim.g.gruvbox_material_dim_inactive_windows = 1
  vim.g.gruvbox_material_float_style = "dim"
  vim.g.gruvbox_material_transparent_background = 2
  -- vim.o.background = "light"
  -- vim.g.gruvbox_material_background = "soft"
  vim.cmd.colorscheme("gruvbox-material")

  package.preload["nvim-web-devicons"] = function()
    package.loaded["nvim-web-devicons"] = {}
    require("mini.icons").mock_nvim_web_devicons()
    return package.loaded["nvim-web-devicons"]
  end
end)

Later(function()
  Add({
    source = "nvim-treesitter/nvim-treesitter",
    depends = { "nvim-treesitter/nvim-treesitter-context" },
    hooks = {
      post_checkout = function()
        vim.cmd("TSUpdate")
      end,
    },
  })

  require("nvim-treesitter.configs").setup({
    ensure_installed = {
      "lua",
      "vimdoc",
      "rust",
      "zig",
      "c",
      "cpp",
      "python",
      "go",
      "haskell",
      "astro",
      "javascript",
      "typescript",
      "toml",
      "yaml",
      "markdown",
      "markdown_inline",
    },
    highlight = { enable = true, use_languagetree = true },
    indent = { enable = true },
  })
  require("treesitter-context").setup({ multiline_threshold = 2 })
  vim.cmd([[hi TreesitterContextBottom gui=underline]])

  Add("MeanderingProgrammer/render-markdown.nvim")
  require("render-markdown").setup({
    -- latex = { enabled = false },
    completions = { lsp = { enabled = true }, blink = { enabled = true } },
  })

  local hipatterns = require("mini.hipatterns")
  hipatterns.setup({
    highlighters = {
      fixme = { pattern = "%f[%w]()FIXME()%f[%W]", group = "MiniHipatternsFixme" },
      hack = { pattern = "%f[%w]()HACK()%f[%W]", group = "MiniHipatternsHack" },
      todo = { pattern = "%f[%w]()TODO()%f[%W]", group = "MiniHipatternsTodo" },
      note = { pattern = "%f[%w]()NOTE()%f[%W]", group = "MiniHipatternsNote" },
      perf = { pattern = "%f[%w]()PERF()%f[%W]", group = "MiniHipatternsNote" },
      warn = { pattern = "%f[%w]()WARN()%f[%W]", group = "MiniHipatternsFixme" },
      hex_color = hipatterns.gen_highlighter.hex_color(),
    },
  })
  require("mini.trailspace").setup()
  require("mini.indentscope").setup()

  Add({
    source = "NeogitOrg/neogit",
    depends = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
    },
  })
  require("neogit").setup()
  Map("n", "<leader>gg", "<cmd>Neogit<cr>")

  Add("akinsho/toggleterm.nvim")
  require("toggleterm").setup({
    size = function(term)
      if term.direction == "horizontal" then
        return 35
      elseif term.direction == "vertical" then
        return vim.o.columns * 0.25
      end
    end,
  })
  Map("n", "<leader>tt", ":ToggleTerm direction=vertical<cr>")
  Map("n", "<leader>tf", ":ToggleTerm direction=float<cr>")
  Map("n", "<leader>ts", ":ToggleTerm direction=horizontal<cr>")
  Map("t", "<esc><esc>", [[<C-\><C-n>]])
  Map({ "n", "t" }, "<c-h>", "<cmd>wincmd h<cr>")
  Map({ "n", "t" }, "<c-j>", "<cmd>wincmd j<cr>")
  Map({ "n", "t" }, "<c-k>", "<cmd>wincmd k<cr>")
  Map({ "n", "t" }, "<c-l>", "<cmd>wincmd l<cr>")

  -- Add("shortcuts/no-neck-pain.nvim")
  -- require("no-neck-pain").setup({
  --   -- width = 140,
  --   buffers = {
  --     right = { enabled = false },
  --     scratchPad = {
  --       enabled = true,
  --       location = "~/Documents/",
  --     },
  --     bo = { filetype = "md" },
  --   },
  -- })
  -- Map("n", "<leader>zz", "<cmd>NoNeckPain<cr>")
end)
