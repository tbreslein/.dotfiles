return {
  {
    "nvimtools/none-ls.nvim",
    event = "VeryLazy",
    dependencies = { "nvimtools/none-ls-extras.nvim" },
    config = function()
      local null_ls = require("null-ls")
      local augroup = vim.api.nvim_create_augroup("LspFormatting", {})
      null_ls.setup({
        sources = {
          -- this is needed so blink does not freak out when there's nothing to
          -- display
          null_ls.builtins.completion.tags,

          null_ls.builtins.code_actions.statix,
          require("none-ls.code_actions.eslint").with({
            prefer_local = "node_modules/.bin",
          }),
          require("none-ls.diagnostics.eslint").with({
            prefer_local = "node_modules/.bin",
          }),
          null_ls.builtins.diagnostics.cppcheck.with({
            extra_args = { "--force", "--check-level=exhaustive" },
          }),
          null_ls.builtins.diagnostics.hadolint,
          null_ls.builtins.diagnostics.statix,
          null_ls.builtins.diagnostics.zsh,

          null_ls.builtins.formatting.alejandra,
          null_ls.builtins.formatting.black.with({
            prefer_local = ".venv/bin",
          }),
          null_ls.builtins.formatting.clang_format,
          null_ls.builtins.formatting.cmake_format,
          null_ls.builtins.formatting.prettier.with({
            prefer_local = "node_modules/.bin",
          }),
          null_ls.builtins.formatting.ocamlformat,
          null_ls.builtins.formatting.shfmt,
          null_ls.builtins.formatting.stylua,
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
}
