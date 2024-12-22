later(function()
  -- this assumes that mason is already set up
  add({
    source = "jay-babu/mason-null-ls.nvim",
    depends = {
      "nvimtools/none-ls.nvim",
      "nvimtools/none-ls-extras.nvim",
      "nvim-lua/plenary.nvim",
    },
  })

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
  local null_ls = require("null-ls")
  local formatting_augroup = vim.api.nvim_create_augroup("LspFormatting", {})
  null_ls.setup({
    sources = {
      require("none-ls.code_actions.eslint").with({ prefer_local = "node_modules/.bin" }),
      require("none-ls.diagnostics.eslint").with({ prefer_local = "node_modules/.bin" }),
      null_ls.builtins.diagnostics.cppcheck.with({ extra_args = { "--force", "--check-level=exhaustive" } }),
      null_ls.builtins.formatting.black.with({ prefer_local = ".venv/bin" }),
      null_ls.builtins.formatting.prettier.with({ prefer_local = "node_modules/.bin" }),
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
end)
