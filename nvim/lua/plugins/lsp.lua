return {
  {
    "Saghen/blink.cmp",
    event = "VeryLazy",
    version = "v0.2.0",
    dependencies = {
      "neovim/nvim-lspconfig",
      "j-hui/fidget.nvim",
    },
    config = function()
      vim.diagnostic.config {
        virtual_text = {
          prefix = "",
          suffix = "",
        },
        update_in_insert = false,
        underline = true,
        severity_sort = true,
        float = {
          border = "rounded",
          source = true,
          header = "",
          prefix = "",
        },
      }
      require("fidget").setup {}

      require("blink.cmp").setup {
        keymap = {
          show = {},
          hide = {},
          accept = "<C-l>",
          select_prev = "<C-k>",
          select_next = "<C-j>",

          show_documentation = {},
          hide_documentation = {},
          scroll_documentation_up = "<C-b>",
          scroll_documentation_down = "<C-f>",

          snippet_forward = "<Tab>",
          snippet_backward = "<S-Tab>",
        },

        trigger = { signature_help = { enabled = true } },
        windows = {
          autocomplete = { draw = "reversed", border = "rounded" },
          documentation = { border = "rounded" },
          signature_help = { border = "rounded" },
        },
        highlight = { use_nvim_cmp_as_default = true },

        -- kind_icons = {
        --   Text = "󰉿",
        --   Method = "󰊕",
        --   Function = "󰊕",
        --   Constructor = "󰒓",
        --
        --   Field = "󰜢",
        --   Variable = "󰆦",
        --   Property = "󰖷",
        --
        --   Class = "󱡠",
        --   Interface = "󱡠",
        --   Struct = "󱡠",
        --   Module = "󰅩",
        --
        --   Unit = "󰪚",
        --   Value = "󰦨",
        --   Enum = "󰦨",
        --   EnumMember = "󰦨",
        --
        --   Keyword = "󰻾",
        --   Constant = "󰏿",
        --
        --   Snippet = "󱄽",
        --   Color = "󰏘",
        --   File = "󰈔",
        --   Reference = "󰬲",
        --   Folder = "󰉋",
        --   Event = "󱐋",
        --   Operator = "󰪚",
        --   TypeParameter = "󰬛",
        -- },
      }

      local lspconfig = require "lspconfig"
      local lsp_servers = {
        "bashls",
        "rust_analyzer",
        "dockerls",
        "gopls",
        "hyprls",
        "marksman",
        "ts_ls",
        "ruff",
        "clangd",
        "cmake",
        "nil_ls",
        "zls",
        "ols",
      }
      for _, s in ipairs(lsp_servers) do
        lspconfig[s].setup { capabilities = lsp_capabilities }
      end

      vim.lsp.handlers["textDocument/hover"] =
        vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" })

      lspconfig.pyright.setup {
        capabilities = lsp_capabilities,
        on_new_config = function(config, root_dir)
          local env = vim.trim(
            vim.fn.system(
              'cd "'
                .. (root_dir or ".")
                .. '"; poetry env info --executable 2>/dev/null'
            )
          )
          if string.len(env) > 0 then
            config.settings.python.pythonPath = env
          end
        end,
      }
      lspconfig.lua_ls.setup {
        capabilities = lsp_capabilities,
        cmd = { "lua-lsp" },
        settings = {
          Lua = {
            runtime = {
              version = "LuaJIT",
              path = vim.split(package.path, ";"),
            },
            diagnostics = { globals = { "vim" } },
            workspace = {
              library = {
                [vim.fn.expand "$VIMRUNTIME/lua"] = true,
                [vim.fn.expand "$VIMRUNTIME/lua/vim/lsp"] = true,
              },
            },
          },
        },
      }
      vim.keymap.set("n", "gl", vim.diagnostic.open_float)
      vim.api.nvim_create_autocmd("LspAttach", {
        desc = "LSP actions",
        callback = function(e)
          vim.bo[e.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
          vim.keymap.set("n", "gq", vim.diagnostic.setqflist)
          vim.keymap.set("n", "gQ", vim.diagnostic.setloclist)
          vim.keymap.set("n", "gD", vim.lsp.buf.declaration)
          vim.keymap.set("n", "gt", vim.lsp.buf.type_definition)
          vim.keymap.set("n", "gi", vim.lsp.buf.implementation)
          vim.keymap.set("n", "grr", vim.lsp.buf.references)
          vim.keymap.set("n", "grn", vim.lsp.buf.rename)
          vim.keymap.set("n", "gra", vim.lsp.buf.code_action)
          vim.keymap.set("n", "<leader>hi", function()
            vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
          end)
          vim.keymap.set({ "i", "s" }, "<c-s>", vim.lsp.buf.signature_help)

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
}
