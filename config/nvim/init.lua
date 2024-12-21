local function map(mode, keys, action, desc, opts)
  vim.keymap.set(mode, keys, action, vim.tbl_extend("keep", opts or {}, { noremap = true, silent = true, desc = desc }))
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
vim.opt.clipboard:append({ "unnamed", "unnamedplus" })
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
vim.opt.fillchars:append({ eob = " " })
vim.opt.shortmess:append("aIF")
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.mouse = "a"
vim.opt.fileencoding = "utf-8"
vim.g.winblend = 0
vim.opt.wildmenu = true
vim.opt.wildoptions:append("fuzzy")
vim.opt.pumheight = 10
vim.opt.updatetime = 400
vim.g.border_style = "single"
vim.opt.statusline = "%#Normal#" .. "%="
vim.diagnostic.config({
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
})

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
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = event.buf, silent = true })
  end,
})
vim.api.nvim_create_autocmd({ "BufWritePost" }, {
  pattern = { "*.rs" },
  callback = function()
    vim.lsp.buf.format({ async = false })
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
map("n", "]c", ":cnext<cr>", "")
map("n", "[c", ":cprev<cr>", "")

local lazy_path = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazy_path) then
  local lazy_url = "https://github.com/folke/lazy.nvim"
  vim.fn.system({ "git", "clone", "--filter=blob:none", lazy_url, "--branch=stable", lazy_path })
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
    event = "VeryLazy",
    opts = {
      style = "fg",
      components = { left = { "diagnostics" }, center = { "path" }, right = { "progress" } },
      icons = { diagnostics = { ERROR = "✘ ", WARN = " ", HINT = "󱐮 ", INFO = "◉ " } },
    },
  },

  {
    "nvim-treesitter/nvim-treesitter",
    cmd = { "TSInstall", "TSBufEnable", "TSBufDisable", "TSModuleInfo" },
    build = ":TSUpdate",
    event = { "BufReadPost", "BufNewFile" },
    dependencies = { "nvim-treesitter/nvim-treesitter-context" },
    config = function()
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
      vim.cmd([[hi TreesitterContext guibg=#363738]])
    end,
  },

  {
    "MeanderingProgrammer/render-markdown.nvim",
    ft = "markdown",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
  },

  { "aserowy/tmux.nvim",         event = "UIEnter", opts = {} },

  {
    "echasnovski/mini.nvim",
    init = function()
      package.preload["nvim-web-devicons"] = function()
        package.loaded["nvim-web-devicons"] = {}
        require("mini.icons").mock_nvim_web_devicons()
        return package.loaded["nvim-web-devicons"]
      end
    end,
    event = "VimEnter",

    config = function()
      require("mini.pick").setup()
      map("n", "<leader>ff", ":Pick files<cr>")
      map("n", "<leader>fs", ":Pick grep_live<cr>")
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
    end,
  },

  {
    "stevearc/oil.nvim",
    keys = { { "-", "<cmd>Oil<cr>", "oil" } },
    opts = {},
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
      "j-hui/fidget.nvim",
      "williamboman/mason-lspconfig.nvim",
      "williamboman/mason.nvim",
    },
    config = function()
      require("fidget").setup({})
      require("blink.cmp").setup({
        keymap = {
          preset = "default",
          ["<C-j>"] = { "select_next" },
          ["<C-k>"] = { "select_prev" },
          ["<C-l>"] = { "accept" },
          ["<C-b>"] = { "scroll_documentation_up" },
          ["<C-f>"] = { "scroll_documentation_down" },
          ["<Tab>"] = { "snippet_forward", "fallback" },
          ["<S-Tab>"] = { "snippet_backward", "fallback" },
        },
        completion = {
          list = { max_items = 200 },
          menu = { border = vim.g.border_style },
          documentation = {
            auto_show = true,
            auto_show_delay_ms = 500,
            window = {
              min_width = 10,
              max_width = 60,
              max_height = 20,
              border = vim.g.border_style,
            },
          },
        },
        signature = { enabled = true },
      })

      local lsp_capabilities = require("blink.cmp").get_lsp_capabilities()
      lsp_capabilities.textDocument.completion.completionItem = {
        documentationFormat = { "markdown", "plaintext" },
        snippetSupport = true,
        preselectSupport = true,
        insertReplaceSupport = true,
        labelDetailsSupport = true,
        deprecatedSupport = true,
        commitCharactersSupport = true,
        tagSupport = { valueSet = { 1 } },
        resolveSupport = {
          properties = {
            "documentation",
            "detail",
            "additionalTextEdits",
          },
        },
      }

      local lspconfig = require("lspconfig")
      local mason_lspconfig = require("mason-lspconfig")

      mason_lspconfig.setup({
        ensure_installed = {
          "asm_lsp",
          "bashls",
          "neocmake",
          "dockerls",
          "lua_ls",
          "marksman",
          "nil_ls",
          "ruff",
          "ts_ls",
          "astro",
        },
      })
      lspconfig["clangd"].setup({ capabilities = lsp_capabilities })
      lspconfig["zls"].setup({ capabilities = lsp_capabilities })
      lspconfig["ocamllsp"].setup({ capabilities = lsp_capabilities })

      mason_lspconfig.setup_handlers({
        function(server_name) -- default handler
          lspconfig[server_name].setup({ capabilities = lsp_capabilities })
        end,
        ["lua_ls"] = function()
          lspconfig.lua_ls.setup({
            capabilities = lsp_capabilities,
            settings = {
              Lua = {
                runtime = { version = "LuaJIT", path = vim.split(package.path, ";") },
                diagnostics = { globals = { "vim" } },
                workspace = {
                  library = {
                    [vim.fn.expand("$VIMRUNTIME/lua")] = true,
                    [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
                  },
                },
              },
            },
          })
        end,
      })

      vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = vim.g.border_style })

      lspconfig.pyright.setup({
        capabilities = lsp_capabilities,
        on_new_config = function(config, root_dir)
          local env =
              vim.trim(vim.fn.system('cd "' .. (root_dir or ".") .. '"; poetry env info --executable 2>/dev/null'))
          if string.len(env) > 0 then
            config.settings.python.pythonPath = env
          end
        end,
      })
      map("n", "gl", vim.diagnostic.open_float)
      map("n", "]d", vim.diagnostic.goto_next)
      map("n", "[d", vim.diagnostic.goto_prev)
      vim.api.nvim_create_autocmd("LspAttach", {
        desc = "LSP actions",
        callback = function(e)
          vim.bo[e.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
          map("n", "gq", vim.diagnostic.setqflist)
          map("n", "gQ", vim.diagnostic.setloclist)
          map("n", "gD", vim.lsp.buf.declaration)
          map("n", "gt", vim.lsp.buf.type_definition)
          map("n", "gi", vim.lsp.buf.implementation)
          map("n", "grr", vim.lsp.buf.references)
          map("n", "grn", vim.lsp.buf.rename)
          map("n", "gra", vim.lsp.buf.code_action)
          map("n", "<leader>hi", function()
            vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
          end)
          map({ "i", "s" }, "<c-s>", vim.lsp.buf.signature_help)

          -- default keymaps I should use:
          -- ^]: goto definition
          -- grr: references to qflist
          -- gra: code actions to qflist
          -- grn: lsp rename
          -- c-s: signature_help
        end,
      })
    end,
  },
  { "mrcjkb/rustaceanvim",       lazy = false,      dependencies = { "mfussenegger/nvim-dap" } },
  { "mrcjkb/haskell-tools.nvim", lazy = false },

  {
    "williamboman/mason.nvim",
    dependencies = {
      "WhoIsSethDaniel/mason-tool-installer.nvim",
      "jay-babu/mason-null-ls.nvim",
      "nvimtools/none-ls.nvim",
      "nvimtools/none-ls-extras.nvim",
      "nvim-lua/plenary.nvim",
    },
    lazy = false,
    config = function()
      require("mason").setup()
      require("mason-null-ls").setup({
        ensure_installed = {
          "asmfmt",
          "cmake_lint",
          "cmake_format",
          "clang_format",
          "hadolint",
          "shellcheck",
          "shellharden",
          "luacheck",
          "stylua",
          "yamllint",
          "nixpkgs_fmt",
          "ormolu",
        },
        handlers = {},
      })
      require("mason-tool-installer").setup({
        ensure_installed = {
          "codelldb",
        },
      })

      local null_ls = require("null-ls")
      local augroup = vim.api.nvim_create_augroup("LspFormatting", {})
      null_ls.setup({
        sources = {
          -- null_ls.builtins.code_actions.statix,
          -- null_ls.builtins.diagnostics.statix,
          --
          -- null_ls.builtins.formatting.alejandra,
          -- null_ls.builtins.formatting.clang_format,
          -- null_ls.builtins.formatting.ocamlformat,
          -- null_ls.builtins.formatting.shfmt,
          require("none-ls.code_actions.eslint").with({ prefer_local = "node_modules/.bin" }),
          require("none-ls.diagnostics.eslint").with({ prefer_local = "node_modules/.bin" }),
          null_ls.builtins.diagnostics.cppcheck.with({ extra_args = { "--force", "--check-level=exhaustive" } }),
          null_ls.builtins.formatting.black.with({
            prefer_local = ".venv/bin",
          }),
          null_ls.builtins.formatting.prettier.with({ prefer_local = "node_modules/.bin" }),
        },
        on_attach = function(client, bufnr)
          if client.supports_method("textDocument/formatting") then
            vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
            vim.api.nvim_create_autocmd("BufWritePre", {
              group = augroup,
              buffer = bufnr,
              callback = function()
                vim.lsp.buf.format({ bufnr = bufnr })
              end,
            })
          end
        end,
      })
    end,
  },

  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    config = function()
      local snacks = require("snacks")
      snacks.setup({
        indent = { enabled = true },
        lazygit = { enabled = true },
        notifier = { enabled = true },
        terminal = { enabled = true },
        zen = { enabled = true },
      })
      map("n", "<leader>zz", snacks.zen.zen)
      map("n", "<leader>tt", snacks.terminal.toggle)
      map("n", "<leader>nh", snacks.notifier.get_history)
      map("n", "<leader>gg", snacks.lazygit.open)
    end,
  },
}

require("lazy").setup(plugins, {
  defaults = { lazy = true },
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
})
