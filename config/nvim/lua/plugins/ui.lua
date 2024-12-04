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

  "Bekaboo/dropbar.nvim",

  -- {
  --   "nvim-lualine/lualine.nvim",
  --   lazy = false,
  --   opts = {
  --     options = {
  --       component_separators = { left = "", right = "" },
  --       section_separators = { left = "", right = "" },
  --     },
  --     sections = {
  --       lualine_a = {
  --         function()
  --           return "󰣇 "
  --         end,
  --       },
  --       lualine_b = {
  --         {
  --           "filename",
  --           path = 1,
  --           shorting_target = 80,
  --         },
  --       },
  --       lualine_c = { "progress", "location", "diagnostics" },
  --       lualine_x = { "diff" },
  --       lualine_y = { "branch" },
  --       lualine_z = {},
  --     },
  --     inactive_sections = {
  --       lualine_a = {},
  --       lualine_b = {},
  --       lualine_c = { "filename", "location" },
  --       lualine_x = {},
  --       lualine_y = {},
  --       lualine_z = {},
  --     },
  --   },
  -- },

  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    dependencies = { "nvim-treesitter/nvim-treesitter-context" },
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = "all",
        ignore_install = { "norg", "hoon" },
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
      vim.filetype.add({
        pattern = { [".*/hypr/.*%.conf"] = "hyprlang" },
      })
    end,
  },
  "Apeiros-46B/uiua.vim",

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
    "meanderingprogrammer/render-markdown.nvim",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
    event = "VeryLazy",
    opts = {},
  },
}
