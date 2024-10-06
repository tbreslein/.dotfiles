local plugins = {
  "savq/paq-nvim",

  -- deps
  "nvim-tree/nvim-web-devicons",
  "nvim-lua/plenary.nvim",
  "Bilal2453/luvit-meta",
  "nvim-neotest/nvim-nio",

  -- ui
  "sainnhe/gruvbox-material",
  "vague2k/vague.nvim",
  "folke/zen-mode.nvim",
  "tpope/vim-fugitive",
  { "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
  "nvim-treesitter/nvim-treesitter-context",
  "akinsho/toggleterm.nvim",
  "stevearc/quicker.nvim",
  "meanderingprogrammer/render-markdown.nvim", -- deps: treesitter, nvim-web-devicons

  -- navigation
  { "ThePrimeagen/harpoon", branch = "harpoon2" }, -- dep: plenary
  "nvim-telescope/telescope.nvim", -- dep: plenary, nvim-web-devicons
  "nvim-telescope/telescope-fzy-native.nvim",
  "stevearc/oil.nvim",

  -- lsp
  "neovim/nvim-lspconfig",
  "hrsh7th/nvim-cmp",
  "hrsh7th/cmp-nvim-lsp",
  "hrsh7th/cmp-path",
  "hrsh7th/cmp-buffer",
  "hrsh7th/cmp-cmdline",
  "onsails/lspkind.nvim",
  "j-hui/fidget.nvim",
  "folke/lazydev.nvim", -- dep: luvit-meta

  -- linting/formatting
  "nvimtools/none-ls.nvim",
  "nvimtools/none-ls-extras.nvim",

  -- dap
  "rcarriga/nvim-dap-ui", -- dep: nvim-nio
  "mfussenegger/nvim-dap",
  "mfussenegger/nvim-dap-python",
  "leoluz/nvim-dap-go",
}

local function load_paq()
  local path = vim.fn.stdpath "data" .. "/site/pack/paqs/start/paq-nvim"
  local is_installed = vim.fn.empty(vim.fn.glob(path)) == 0
  if not is_installed then
    vim.fn.system {
      "git",
      "clone",
      "--depth=1",
      "https://github.com/savq/paq-nvim.git",
      path,
    }
  end

  vim.cmd.packadd "paq-nvim"
  local paq = require "paq"
  if not is_installed then
    vim.notify "Installing plugins... If prompted, hit Enter to continue."
  end
  paq(plugins)
  paq.install()
end

local function headless_paq()
  vim.cmd "autocmd User PaqDoneInstall quit"
  load_paq()
end

local function sync_paq()
  vim.cmd.packadd "paq-nvim"
  local paq = require "paq"
  require "paq"(plugins)
  local group = vim.api.nvim_create_augroup("paq_group", {})
  vim.api.nvim_create_autocmd(
    "User",
    { pattern = "PaqDoneUpdate", command = "quit", group = group }
  )
  paq.clean()
  paq.install()
  paq.update()
end

return {
  headless_paq = headless_paq,
  load_paq = load_paq,
  sync_paq = sync_paq,
}
