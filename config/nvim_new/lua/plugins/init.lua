local plugins = {
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

  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "nvim-telescope/telescope.nvim",
    },
    keys = { { "<leader>gg", ":Neogit<cr>", "Neogit" } },
    opts = {
      integrations = {
        telescope = true,
        diffview = true,
      },
      commit_editor = {
        staged_diff_split_kind = "split_above",
        spell_check = false,
      },
      mappings = {
        popup = { ["p"] = "PushPopup", ["F"] = "PullPopup" },
      },
    },
  },

  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    cmd = { "TSInstall", "TSBufEnable", "TSBufDisable", "TSModuleInfo" },
    event = { "BufReadPost", "BufNewFile" },
    dependencies = { "nvim-treesitter/nvim-treesitter-context" },
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = { "lua", "vimdoc", "rust", "python", "c", "zig", "toml", "yaml", "markdown", "markdown_inline" },
        highlight = {
          enable = true,
          -- additional_vim_regex_highlighting = false,
          use_languagetree = true,
          disable = { "json" },
        },
        indent = { enable = true },
        autotag = { enable = true },
      })
      require("treesitter-context").setup({ multiline_threshold = 2 })
      vim.cmd([[hi TreesitterContextBottom gui=underline]])
      vim.cmd([[hi TreesitterContext guibg=#363738]])
      vim.filetype.add({
        pattern = { [".*/hypr/.*%.conf"] = "hyprlang" }
      })
    end,
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
    "meanderingprogrammer/render-markdown.nvim",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      -- "nvim-tree/nvim-web-devicons",
    },
    ft = "markdown",
    opts = {},
  },

  {
    "nvim-lualine/lualine.nvim",
    lazy = false,
    opts = {
      options = {
        component_separators = { left = "", right = "" },
        section_separators = { left = "", right = "" },
      },
      winbar = {
        lualine_a = {},
        lualine_b = {{
          "filename",
          path = 1,
          shorting_target = 80,
	}},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
      },
      inwinbar = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {{
          "filename",
          path = 1,
          shorting_target = 80,
	}},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
      },
      sections = {
        lualine_a = {},
        lualine_b = {"branch", "diff", "diagnostics"},
        lualine_c = {"diagnostics"},
        lualine_x = {},
        lualine_y = {"progress"},
        lualine_z = {"location"},
        -- lualine_a = {
        --   function()
        --     return "󰣇 "
        --   end,
        -- },
        -- lualine_b = {
        --   {
        --     "filename",
        --     path = 1,
        --     shorting_target = 80,
        --   },
        -- },
        -- lualine_c = { "progress", "location", "diagnostics" },
        -- lualine_x = { "diff" },
        -- lualine_y = { "branch" },
        -- lualine_z = {},
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
      },
    },
  },
}

require("lazy").setup(plugins, {
  concurrency = 4,
  defaults = { lazy = true },
  -- install = { colorscheme = { "catppuccin" } },
  dev = { path = vim.env.NVIM_DEV },
  performance = {
    cache = { enabled = true },
    reset_packpath = true,
    rtp = {
      disabled_plugins = {
        "osc52",
        "parser",
        "gzip",
        "netrwPlugin",
        "health",
        "man",
        "matchit",
        "rplugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
        "shadafile",
        "spellfile",
      },
    },
  },
  ui = {
    border = "solid",
    title = "lazy.nvim",
    size = { width = 0.9, height = 0.9 },
  },
})
