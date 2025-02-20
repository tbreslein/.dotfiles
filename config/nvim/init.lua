local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
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
vim.opt.clipboard:append({ "unnamed", "unnamedplus" })
vim.opt.completeopt = { "menuone", "noselect", "noinsert" }
vim.opt.fileencoding = "utf-8"
vim.opt.confirm = false
vim.opt.equalalways = false
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.timeout = false
vim.opt.scrolloff = 5
vim.opt.sidescrolloff = 3
vim.opt.shiftwidth = 2
vim.opt.smartindent = true
vim.opt.tabstop = 2
vim.opt.expandtab = true
vim.opt.breakindent = true
vim.opt.linebreak = true
vim.opt.fillchars:append({ eob = " " })
vim.opt.shortmess:append("aIF")
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.mouse = "a"
vim.opt.wildmenu = true
vim.opt.wildoptions:append("fuzzy")
vim.opt.pumheight = 10
vim.opt.updatetime = 400
vim.g.winblend = 0
vim.g.borderstyle = "single"
vim.opt.laststatus = 3
vim.opt.cmdheight = 1
vim.g.loaded_node_provider = 0
vim.g.loaded_python3_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0

function kmap(mode, keys, action, opts)
  vim.keymap.set(mode, keys, action, vim.tbl_extend("keep", opts or {}, { noremap = true, silent = true, desc = desc }))
end

kmap("n", "Q", "<nop>")
kmap("n", "<esc>", ":noh<cr>")
kmap("v", "P", [["_dP]])
kmap({ "n", "x", "v" }, "x", [["_x]])
kmap("n", "Y", "yg$")
kmap("n", "J", "mzJ`z")
kmap("n", "n", "nzz")
kmap("n", "N", "Nzz")
kmap("n", "*", "*zz")
kmap("n", "#", "#zz")
kmap("n", "g*", "g*zz")
kmap("n", "g#", "g#zz")
kmap("n", "<c-d>", "<c-d>zz")
kmap("n", "<c-u>", "<c-u>zz")
kmap("v", "<", "<gv")
kmap("v", ">", ">gv")
kmap("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true })
kmap("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true })
kmap("n", "]c", ":cnext<cr>")
kmap("n", "[c", ":cprev<cr>")

vim.api.nvim_create_autocmd("TextYankPost", {
  group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
  pattern = "*",
  callback = function()
    vim.highlight.on_yank()
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "git", "help", "lspinfo", "man", "query", "vim" },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    kmap("n", "q", "<cmd>close<cr>", { buffer = event.buf })
  end,
})

require("lazy").setup({
  spec = {
    {
      "sainnhe/gruvbox-material",
      lazy = false,
      priority = 1000,
      config = function()
        vim.g.gruvbox_material_enable_italic = 1
        vim.g.gruvbox_material_enable_bold = 1
        vim.g.gruvbox_material_better_performance = 1
        vim.g.gruvbox_material_ui_contrast = "high"
        vim.g.gruvbox_material_diagnostic_virtual_text = "highlighted"
        vim.g.gruvbox_material_transparent_background = 2
        vim.g.gruvbox_material_dim_inactive_windows = 1
        vim.g.gruvbox_material_float_style = "dim"
        vim.cmd.colorscheme("gruvbox-material")
      end,
    },
    {
      "nvim-lualine/lualine.nvim",
      opts = {
        options = {
          component_separators = { left = "", right = "" },
          section_separators = { left = "", right = "" },
        },
        sections = {
          lualine_a = {},
          lualine_b = { { "filename", path = 3 } },
          lualine_c = { { "diagnostics", symbols = { error = "✘ ", warn = " ", hint = "󱐮 ", info = "◉ " } } },
          lualine_x = { "progress" },
          lualine_y = { "location" },
          lualine_z = {},
        },
      },
    },
    {
      "MeanderingProgrammer/render-markdown.nvim",
      ft = "markdown",
      opts = { latex = { enabled = false } },
    },
    {
      "nvim-treesitter/nvim-treesitter",
      dependencies = "nvim-treesitter/nvim-treesitter-context",
      build = ":TSUpdate",
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
      end,
    },
    {
      "echasnovski/mini.nvim",
      lazy = false,
      keys = {
        {
          "<leader>fp",
          function()
            MiniFiles.open(vim.api.nvim_buf_get_name(0))
          end,
          "",
        },
      },
      config = function()
        require("mini.move").setup()
        require("mini.icons").setup()
        MiniIcons.mock_nvim_web_devicons()
        MiniIcons.tweak_lsp_kind()
        require("mini.files").setup()
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
      "folke/snacks.nvim",
      priority = 1000,
      lazy = false,
      keys = {
        {
          "<leader>ff",
          function()
            Snacks.picker.git_files()
          end,
          "",
        },
        {
          "<leader>fs",
          function()
            Snacks.picker.grep()
          end,
          "",
        },
        {
          "<leader>zz",
          function()
            Snacks.zen()
          end,
          "",
        },
        {
          "<leader>gg",
          function()
            Snacks.lazygit()
          end,
          "",
        },
      },
      config = function()
        require("snacks").setup({
          bigfile = { enabled = true },
          -- image = { enabled = true },
          indent = { enabled = true },
          input = { enabled = true },
          lazygit = { enabled = true },
          notifier = { enabled = true },
          picker = { enabled = true },
          rename = { enabled = true },
          zen = { enabled = true },
        })

        -- LSP integrated rename in mini.files
        vim.api.nvim_create_autocmd("User", {
          pattern = "MiniFilesActionRename",
          callback = function(event)
            Snacks.rename.on_rename_file(event.data.from, event.data.to)
          end,
        })

        ---@type table<number, {token:lsp.ProgressToken, msg:string, done:boolean}[]>
        local progress = vim.defaulttable()
        vim.api.nvim_create_autocmd("LspProgress", {
          ---@param ev {data: {client_id: integer, params: lsp.ProgressParams}}
          callback = function(ev)
            local client = vim.lsp.get_client_by_id(ev.data.client_id)
            local value = ev.data.params.value --[[@as {percentage?: number, title?: string, message?: string, kind: "begin" | "report" | "end"}]]
            if not client or type(value) ~= "table" then
              return
            end
            local p = progress[client.id]

            for i = 1, #p + 1 do
              if i == #p + 1 or p[i].token == ev.data.params.token then
                p[i] = {
                  token = ev.data.params.token,
                  msg = ("[%3d%%] %s%s"):format(
                    value.kind == "end" and 100 or value.percentage or 100,
                    value.title or "",
                    value.message and (" **%s**"):format(value.message) or ""
                  ),
                  done = value.kind == "end",
                }
                break
              end
            end

            local msg = {} ---@type string[]
            progress[client.id] = vim.tbl_filter(function(v)
              return table.insert(msg, v.msg) or not v.done
            end, p)

            local spinner = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
            vim.notify(table.concat(msg, "\n"), "info", {
              id = "lsp_progress",
              title = client.name,
              opts = function(notif)
                notif.icon = #progress[client.id] == 0 and " "
                  or spinner[math.floor(vim.uv.hrtime() / (1e6 * 80)) % #spinner + 1]
              end,
            })
          end,
        })
      end,
    },
    { "aserowy/tmux.nvim", events = "VeryLazy", opts = { multi_windows = true } },
    {
      "cbochs/grapple.nvim",
      events = "VeryLazy",
      opts = {},
      keys = {
        { "<leader>a", "<cmd>Grapple tag<cr>" },
        { "<leader>e", "<cmd>Grapple toggle_tags<cr>" },
        { "<A-r>", "<cmd>Grapple select index=1<cr>" },
        { "<A-e>", "<cmd>Grapple select index=2<cr>" },
        { "<A-w>", "<cmd>Grapple select index=3<cr>" },
        { "<A-q>", "<cmd>Grapple select index=4<cr>" },
      },
    },
    {
      "folke/flash.nvim",
      events = "VeryLazy",
      opts = {},
      keys = {
        {
          "s",
          mode = { "n", "x", "o" },
          function()
            require("flash").jump()
          end,
        },
      },
    },
    {
      "jay-babu/mason-null-ls.nvim",
      events = "VeryLazy",
      dependencies = {
        { "williamboman/mason.nvim", opts = {} },
        "nvimtools/none-ls.nvim",
        "nvimtools/none-ls-extras.nvim",
        "nvim-lua/plenary.nvim",
      },
      config = function()
        require("mason-null-ls").setup({
          ensure_installed = {
            "asmfmt",
            "cmake_lint",
            "cmake_format",
            "clang_format",
            "golangci-lint",
            "gofumpt",
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
        local null_ls = require("null-ls")
        local formatting_augroup = vim.api.nvim_create_augroup("LspFormatting", {})
        null_ls.setup({
          sources = {
            require("none-ls.code_actions.eslint").with({ prefer_local = "node_modules/.bin" }),
            require("none-ls.diagnostics.eslint").with({ prefer_local = "node_modules/.bin" }),
            null_ls.builtins.diagnostics.cppcheck.with({ extra_args = { "--force", "--check-level=exhaustive" } }),
            null_ls.builtins.formatting.black.with({ prefer_local = ".venv/bin" }),
            null_ls.builtins.formatting.prettier.with({ prefer_local = "node_modules/.bin" }),
            require("none-ls.formatting.rustfmt"),
          },
          on_attach = function(client, bufnr)
            if client.supports_method("textDocument/formatting") then
              vim.api.nvim_clear_autocmds({ group = formatting_augroup, buffer = bufnr })
              vim.api.nvim_create_autocmd("BufWritePre", {
                group = formatting_augroup,
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
      "neovim/nvim-lspconfig",
      events = "VeryLazy",
      dependencies = {
        { "williamboman/mason.nvim", opts = {} },
        "williamboman/mason-lspconfig.nvim",
        { "saghen/blink.cmp", version = "0.12.3", dependencies = "rafamadriz/friendly-snippets" },
      },
      config = function()
        local lspconfig = require("lspconfig")
        local mason_lspconfig = require("mason-lspconfig")
        local blink = require("blink.cmp")

        blink.setup({
          keymap = {
            preset = "default",
            ["<c-j>"] = { "select_next" },
            ["<c-k>"] = { "select_prev" },
            ["<c-l>"] = { "accept" },
            ["<c-n>"] = { "scroll_documentation_up" },
            ["<c-f>"] = { "scroll_documentation_down" },
            ["<Tab>"] = { "snippet_forward", "fallback" },
            ["<S-Tab>"] = { "snippet_backward", "fallback" },
          },
          completion = {
            accept = { auto_brackets = { enabled = false } },
            list = { max_items = 200, selection = { auto_insert = false } },
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
          sources = {
            default = { "lsp", "path", "snippets", "buffer", "markdown" },
            providers = {
              markdown = { name = "RenderMarkdown", module = "render-markdown.integ.blink" },
            },
          },
        })

        local lsp_capabilities = blink.get_lsp_capabilities()
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

        mason_lspconfig.setup({
          ensure_installed = {
            "asm_lsp",
            "bashls",
            "neocmake",
            "dockerls",
            "lua_ls",
            "marksman",
            "rnix",
            "ruff",
            "ts_ls",
            "astro",
            "gopls",
          },
        })
        lspconfig.clangd.setup({ capabilities = lsp_capabilities })
        lspconfig.zls.setup({ capabilities = lsp_capabilities })
        lspconfig.ocamllsp.setup({ capabilities = lsp_capabilities })

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

        lspconfig.basedpyright.setup({
          capabilities = lsp_capabilities,
          on_new_config = function(config, root_dir)
            local env =
              vim.trim(vim.fn.system('cd "' .. (root_dir or ".") .. '"; poetry env info --executable 2>/dev/null'))
            if string.len(env) > 0 then
              config.settings.python.pythonPath = env
            end
          end,
        })
        kmap("n", "gl", vim.diagnostic.open_float)
        kmap("n", "]d", vim.diagnostic.goto_next)
        kmap("n", "[d", vim.diagnostic.goto_prev)
        vim.api.nvim_create_autocmd("LspAttach", {
          desc = "LSP actions",
          callback = function(e)
            vim.bo[e.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
            kmap("n", "gq", vim.diagnostic.setqflist)
            kmap("n", "gQ", vim.diagnostic.setloclist)
            kmap("n", "gD", vim.lsp.buf.declaration)
            kmap("n", "gt", vim.lsp.buf.type_definition)
            kmap("n", "gi", vim.lsp.buf.implementation)
            kmap("n", "grr", vim.lsp.buf.references)
            kmap("n", "grn", vim.lsp.buf.rename)
            kmap("n", "gra", vim.lsp.buf.code_action)
            kmap("n", "<leader>hi", function()
              vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
            end)
            kmap({ "i", "s" }, "<c-s>", vim.lsp.buf.signature_help)

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
  },

  change_detection = { enabled = false },
  ui = { border = vim.g.borderstyle },
  performance = {
    rtp = {
      disabled_plugins = {
        "gzip",
        "matchit",
        "matchparen",
        "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
  readme = { enabled = false },
})
