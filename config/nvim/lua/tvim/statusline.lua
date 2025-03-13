Now(function()
  Add("linrongbin16/lsp-progress.nvim")
  local lsp_progress = require('lsp-progress')
  lsp_progress.setup()

  Statusline = {}
  local filepath = function()
    local fpath = vim.fn.fnamemodify(vim.fn.expand "%", ":.")
    if fpath == "" or fpath == "." then
        return " "
    end
    return string.format(" %%<%s", fpath) .. " | "
  end

  local lsp = function()
    return lsp_progress.progress()
    -- local count = {}
    -- local levels = {
    --   errors = "Error",
    --   warnings = "Warn",
    --   info = "Info",
    --   hints = "Hint",
    -- }
    --
    -- for k, level in pairs(levels) do
    --   count[k] = vim.tbl_count(vim.diagnostic.get(0, { severity = level }))
    -- end
    --
    -- local errors = ""
    -- local warnings = ""
    -- local hints = ""
    -- local info = ""
    --
    -- if count["errors"] ~= 0 then
    --   errors = " %#LspDiagnosticsSignError# " .. count["errors"]
    -- end
    -- if count["warnings"] ~= 0 then
    --   warnings = " %#LspDiagnosticsSignWarning# " .. count["warnings"]
    -- end
    -- if count["hints"] ~= 0 then
    --   hints = " %#LspDiagnosticsSignHint# " .. count["hints"]
    -- end
    -- if count["info"] ~= 0 then
    --   info = " %#LspDiagnosticsSignInformation# " .. count["info"]
    -- end
    --
    -- return errors .. warnings .. hints .. info .. "%#Normal#"
  end

  -- local lineinfo = function()
  --   return "%P %l:%c"
  -- end

  Statusline.active = function()
    return table.concat({
      filepath(),
      lsp(),
      "%=%P %l:%c "
    })
  end

  vim.api.nvim_exec([[
    augroup Statusline
    au!
    au WinEnter,BufEnter * setlocal statusline=%!v:lua.Statusline.active()
    augroup END
  ]], false)

  vim.api.nvim_create_augroup("lualine_augroup", { clear = true })
  vim.api.nvim_create_autocmd("User", {
    group = "lualine_augroup",
    pattern = "LspProgressStatusUpdated",
    callback = function() vim.cmd('redrawstatus') end,
  })
end)
