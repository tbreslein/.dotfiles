vim.loader.enable()
vim.g.mapleader = " "
vim.g.maplocalleader = " "

local path_package = vim.fn.stdpath("data") .. "/site/"
local mini_path = path_package .. "pack/deps/start/mini.nvim"
if not vim.loop.fs_stat(mini_path) then
  vim.cmd('echo "Installing `mini.nvim`" | redraw')
  local clone_cmd = {
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/echasnovski/mini.nvim",
    mini_path,
  }
  vim.fn.system(clone_cmd)
  vim.cmd("packadd mini.nvim | helptags ALL")
  vim.cmd('echo "Installed `mini.nvim`" | redraw')
end
require("mini.deps").setup({ path = { package = path_package } })

function Map(mode, keys, action, opts)
  vim.keymap.set(mode, keys, action, vim.tbl_extend("keep", opts or {}, { noremap = true, silent = true, desc = desc }))
end

add, now, later = MiniDeps.add, MiniDeps.now, MiniDeps.later

require("tvim.vimsettings")
require("tvim.ui")
require("tvim.navigation")
require("tvim.lsp")
require("tvim.none-ls")
require("tvim.dap")
