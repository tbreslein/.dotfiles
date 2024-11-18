-- TODO:
-- org-mode

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

require("config.vimsettings")
require("config.keymaps")
require("config.lazy")
-- require("config.statusline")
