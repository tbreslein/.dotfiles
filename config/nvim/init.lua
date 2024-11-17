vim.loader.enable()
require("opts").initial()

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

local lazy_path = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazy_path) then
  vim.fn.system { "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim", "--branch=stable", lazy_path }
end
vim.opt.rtp:prepend(lazy_path)

require("plugins")

-- require("config.vimsettings")
-- require("config.keymaps")
-- require("config.lazy")
-- require("config.statusline")
