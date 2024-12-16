local function map(mode, keys, action, desc, opts)
  vim.keymap.set(
    mode,
    keys,
    action,
    vim.tbl_extend(
      "keep",
      opts or {},
      { noremap = true, silent = true, desc = desc }
    )
  )
end

--- VIM SETTINGS
vim.loader.enable()
vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.opt.guicursor = ""
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.colorcolumn = "80"
vim.opt.signcolumn = "yes"
vim.opt.cursorline = true
vim.opt.cursorlineopt = "number"
vim.opt.termguicolors = true
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.local/share/vim/undodir"
vim.opt.undofile = true
vim.opt.autoread = true
vim.opt.laststatus = 3
vim.opt.clipboard:append { "unnamed", "unnamedplus" }
vim.opt.completeopt = { "menuone", "noselect", "noinsert" }
vim.opt.confirm = false
vim.opt.equalalways = false
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.g.loaded_node_provider = 0
vim.g.loaded_python3_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0
vim.opt.timeout = false
vim.opt.scrolloff = 5
vim.opt.sidescrolloff = 3
vim.opt.shiftwidth = 2
vim.opt.smartindent = true
vim.opt.tabstop = 2
vim.opt.expandtab = true
vim.opt.softtabstop = 2
vim.opt.breakindent = true
vim.opt.linebreak = true
vim.opt.fillchars:append { eob = " " }
vim.opt.shortmess:append "aIF"
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.mouse = "a"
vim.opt.fileencoding = "utf-8"
vim.g.winblend = 0
vim.opt.wildmenu = true
vim.opt.wildoptions:append("fuzzy")
vim.opt.pumheight = 10
vim.opt.updatetime = 400
vim.opt.cmdheight = 0
vim.g.border_style = "single"
vim.opt.statusline = "%#Normal#" .. "%="
vim.diagnostic.config {
  virtual_text = {
    prefix = "",
    suffix = "",
    format = function(diagnostic)
      return " " .. diagnostic.message .. " "
    end,
  },
  underline = { severity = { min = vim.diagnostic.severity.WARN } },
  signs = {
    text = {
      [vim.diagnostic.severity.HINT] = "󱐮",
      [vim.diagnostic.severity.ERROR] = "✘",
      [vim.diagnostic.severity.INFO] = "◉",
      [vim.diagnostic.severity.WARN] = "",
    },
  },
}

--- AUTOCOMMANDS
-- Load shada after ui-enter
local shada = vim.o.shada
vim.o.shada = ""
vim.api.nvim_create_autocmd("User", {
  pattern = "VeryLazy",
  callback = function()
    vim.o.shada = shada
    pcall(vim.cmd.rshada, { bang = true })
  end,
})

vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
  pattern = "*",
})
vim.api.nvim_create_autocmd("FocusGained", { command = "checktime" })
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "fugitive", "git", "help", "lspinfo", "man", "query", "vim" },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set(
      "n",
      "q",
      "<cmd>close<cr>",
      { buffer = event.buf, silent = true }
    )
  end,
})

--- KEYMAPS
map("n", "Q", "<nop>", "")
map("n", "<esc>", ":noh<cr>", "")
map("t", "<c-n>", "<c-\\><c-n>", "")
map("v", "P", [["_dP]], "")
map({ "n", "x", "v" }, "x", [["_x]], "")
map("n", "Y", "yg$", "")
map("n", "J", "mzJ`z", "")
map("n", "n", "nzz", "")
map("n", "N", "Nzz", "")
map("n", "*", "*zz", "")
map("n", "#", "#zz", "")
map("n", "g*", "g*zz", "")
map("n", "g#", "g#zz", "")
map("n", "<c-d>", "<c-d>zz", "")
map("n", "<c-u>", "<c-u>zz", "")
map("v", "<", "<gv", "")
map("v", ">", ">gv", "")
map("v", "J", ":m '>+1<cr>gv=gv", "")
map("v", "K", ":m '<-2<cr>gv=gv", "")
map("n", "j", "v:count == 0 ? 'gj' : 'j'", "", { expr = true })
map("n", "k", "v:count == 0 ? 'gk' : 'k'", "", { expr = true })
-- map("n", "<m-j>", "<c-w>-", "")
-- map("n", "<m-k>", "<c-w>+", "")

local lazy_path = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazy_path) then
  local lazy_url = "https://github.com/folke/lazy.nvim"
  vim.fn.system { "git", "clone", "--filter=blob:none", lazy_url, "--branch=stable", lazy_path }
end
vim.opt.rtp:prepend(lazy_path)

local plugins = {
  {
    "sainnhe/gruvbox-material",
    lazy = false,
    priority = 1000,
    config = function()
      vim.g.gruvbox_material_enable_italic = true
      vim.g.gruvbox_material_enable_bold = true
      vim.g.gruvbox_material_better_performance = true
      vim.g.gruvbox_material_ui_contrast = "high"
      vim.g.gruvbox_material_diagnostic_virtual_text = "highlighted"
      vim.g.gruvbox_material_transparent_background = 2
      vim.cmd.colorscheme("gruvbox-material")
    end,
  },

  {
    "sschleemilch/slimline.nvim",
    -- lazy = "VeryLazy",
    event = "VeryLazy",
    opts = {
      style = 'fg', -- or "fg". Whether highlights should be applied to bg or fg of components
      components = {
        left = {},
        center = {},
        right = {
          "path",
          "diagnostics",
        }
      },
      icons = {
        diagnostics = {
          ERROR = vim.diagnostic.severity.ERROR,
          WARN = vim.diagnostic.severity.WARN,
          HINT = vim.diagnostic.severity.HINT,
          INFO = vim.diagnostic.severity.INFO,
        },
      },
    }
  },

  {
    "nvim-treesitter/nvim-treesitter",
    cmd = { "TSInstall", "TSBufEnable", "TSBufDisable", "TSModuleInfo" },
    build = ":TSUpdate",
    event = { "BufReadPost", "BufNewFile" },
    config = function()
      require("nvim-treesitter.configs").setup {
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
      }
    end,
  },

  {
    "MeanderingProgrammer/render-markdown.nvim",
    ft = "markdown",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
  },

  {
    "neovim/nvim-lspconfig",
    name = "lspconfig",
    cmd = { "LspInfo", "LspInstall", "LspUninstall" },
    event = { "BufReadPost", "BufNewFile" },
    dependencies = {
      { "saghen/blink.cmp", version = "0.*" },
      "folke/lazydev.nvim",
      "rafamadriz/friendly-snippets",
    },
    config = function()
    end
  },
  { "mrcjkb/rustaceanvim", lazy = false, dependencies = { "mfussenegger/nvim-dap" } },
  {
    "williamboman/mason.nvim",
    lazy = false,
    opts = {}
  },
}

require("lazy").setup(plugins, {
  concurrency = 4,
  defaults = { lazy = true, },
  performance = {
    cache = { enabled = true, },
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
})
