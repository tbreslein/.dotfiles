return {
  {
    "sainnhe/gruvbox-material",
    lazy = false,
    priority = 1000,
    config = function()
      vim.o.termguicolors = true
      vim.g.gruvbox_material_enable_italic = true
      vim.g.gruvbox_material_enable_bold = true
      vim.g.gruvbox_material_better_performance = true
      vim.g.gruvbox_material_ui_contrast = "high"
      vim.g.gruvbox_material_diagnostic_virtual_text = "highlighted" -- or "colored"
      vim.g.gruvbox_material_transparent_background = 2
      vim.cmd.colorscheme("gruvbox-material")
    end,
  },
  -- {
  --   "vague2k/vague.nvim",
  --   lazy = false,
  --   priority = 1000,
  --   config = function()
  --     require("vague").setup { transparent = true }
  --     vim.cmd.colorscheme "vague"
  --   end,
  -- },

  {
    "tpope/vim-fugitive",
    lazy = false, -- I need FugitiveStatusline for my statusline
    keys = {
      { "<leader>gg", ":Git<cr>4j", "Git" },
      { "<leader>gpp", ":Git push<cr>", "Git push" },
      { "<leader>gpu", ":Git push --set-upstream origin<cr>", "Git push -u" },
      {
        "<leader>gpf",
        ":Git push --force-with-lease<cr>",
        "Git push --force-with-lease",
      },
    },
  },

  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    dependencies = { "nvim-treesitter/nvim-treesitter-context" },
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = "all",
        ignore_install = { "norg" },
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
          disable = { "json" },
        },
        indent = { enable = true },
        autotag = { enable = true },
      })
      require("treesitter-context").setup({ multiline_threshold = 2 })
      vim.cmd([[hi TreesitterContextBottom gui=underline]])
      vim.cmd([[hi TreesitterContext guibg=#363738]])
    end,
  },

  {
    "akinsho/toggleterm.nvim",
    event = "VeryLazy",
    keys = {
      { "<leader>tt", ":ToggleTerm size=20<cr>", "toggleterm split" },
      { "<leader>te", ":TermExec cmd='!!'<cr>", "toggleterm !!" },
    },
    opts = {},
  },

  {
    "stevearc/quicker.nvim",
    event = "VeryLazy",
    keys = {
      { "]c", ":cnext<cr>zz", "quicklist next" },
      { "[c", ":cprev<cr>zz", "quicklist prev" },
      {
        "<leader>C",
        function()
          require("quicker").toggle()
        end,
        "quicklist toggle",
      },
    },
    opts = {},
  },

  {
    "folke/zen-mode.nvim",
    event = "VeryLazy",
    keys = {
      {
        "<leader>zz",
        function()
          require("zen-mode").toggle()
        end,
        "zen-mode toggle",
      },
    },
    opts = {
      plugins = {
        alacritty = { enabled = true, font = "28" },
        tmux = { enabled = true },
      },
    },
  },

  {
    "meanderingprogrammer/render-markdown.nvim",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
    event = "VeryLazy",
    opts = {},
  },
}
