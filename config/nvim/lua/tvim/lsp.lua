later(function()
  add("neovim/nvim-lspconfig")
  add({ source = "mrcjkb/rustaceanvim", depends = { "mfussenegger/nvim-dap" } })
  vim.g.rustaceanvim = { server = { default_settings = { ["rust-analyzer"] = { check = { command = "check" } } } } }
  add("mrcjkb/haskell-tools.nvim")
  add("williamboman/mason.nvim")
  add("williamboman/mason-lspconfig.nvim")

  add({
    source = "Saghen/blink.cmp",
    depends = { "rafamadriz/friendly-snippets" },
    checkout = "v0.12.4",
  })

  local lspconfig = require("lspconfig")
  local mason_lspconfig = require("mason-lspconfig")
  local blink = require("blink.cmp")

  require("mason").setup()

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
      local env = vim.trim(vim.fn.system('cd "' .. (root_dir or ".") .. '"; poetry env info --executable 2>/dev/null'))
      if string.len(env) > 0 then
        config.settings.python.pythonPath = env
      end
    end,
  })
  Map("n", "gl", vim.diagnostic.open_float)
  Map("n", "]d", vim.diagnostic.goto_next)
  Map("n", "[d", vim.diagnostic.goto_prev)
  vim.api.nvim_create_autocmd("LspAttach", {
    desc = "LSP actions",
    callback = function(e)
      vim.bo[e.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
      Map("n", "gq", vim.diagnostic.setqflist)
      Map("n", "gQ", vim.diagnostic.setloclist)
      Map("n", "gD", vim.lsp.buf.declaration)
      Map("n", "gt", vim.lsp.buf.type_definition)
      Map("n", "gi", vim.lsp.buf.implementation)
      Map("n", "grr", vim.lsp.buf.references)
      Map("n", "grn", vim.lsp.buf.rename)
      Map("n", "gra", vim.lsp.buf.code_action)
      Map("n", "<leader>hi", function()
        vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
      end)
      Map({ "i", "s" }, "<c-s>", vim.lsp.buf.signature_help)

      -- default keymaps I should use:
      -- ^]: goto definition
      -- grr: references to qflist
      -- gra: code actions to qflist
      -- grn: lsp rename
      -- c-s: signature_help
    end,
  })

  vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = { "*.rs" },
    callback = function()
      vim.lsp.buf.format({ async = false })
    end,
  })
end)
