vim.loader.enable()
function map(mode, keys, action, desc, opts)
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

require("tvim.plugins").load_paq()
require "tvim.vimsettings"
require "tvim.keymaps"
require "tvim.ui"
require "tvim.navigation"
require "tvim.lsp"
require "tvim.nonels"
require "tvim.dap"
require "tvim.statusline"
