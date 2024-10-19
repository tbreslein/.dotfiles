local components = {}

function _G._statusline_component(name)
  return components[name]()
end

function components.diagnostic_status()
  local ignore = {
    ["c"] = true, -- command mode
    ["t"] = true, -- terminal mode
  }

  local output = ""
  if not ignore[vim.api.nvim_get_mode().mode] then
    local levels = vim.diagnostic.severity
    local num_errs = #vim.diagnostic.get(0, { severity = levels.ERROR })
    local num_warns = #vim.diagnostic.get(0, { severity = levels.WARN })
    local num_info = #vim.diagnostic.get(0, { severity = levels.INFO })

    if num_errs > 0 then
      output = table.concat({
        output,
        " %#DiagnosticError#",
        tostring(num_errs),
        " ✘%* ",
      })
    end
    if num_warns > 0 then
      output = table.concat({
        output,
        " %#DiagnosticWarning#",
        tostring(num_warns),
        " ▲%* ",
      })
    end
    if num_info > 0 then
      output = table.concat({
        output,
        " %#DiagnosticInfo#",
        tostring(num_info),
        " %* ",
      })
    end
  end
  return output
end

local statusline = {
  "%F",
  "%r",
  "%m",
  " %{FugitiveStatusline()}",
  '%{%v:lua._statusline_component("diagnostic_status")%} ',
  "%=",
  " %2p%% ",
  " %3l:%-2c ",
}

vim.o.statusline = table.concat(statusline, "")
vim.api.nvim_create_autocmd("DiagnosticChanged", {
  callback = function(args)
    vim.cmd("let &stl=&stl")
  end,
})
