vim.g.mapleader = " "
vim.g.maplocalleader = ","
vim.opt.guicursor = ""
vim.opt.cursorline = true
vim.opt.nu = true
vim.opt.relativenumber = true
vim.opt.colorcolumn = "80"
vim.opt.termguicolors = true
vim.opt.signcolumn = "yes"
vim.opt.smartindent = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.completeopt = { "menu", "menuone", "noselect" }
vim.opt.wildoptions:append("fuzzy")
vim.opt.foldenable = false
vim.opt.conceallevel = 0
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.local/share/vim/undodir"
vim.opt.undofile = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.mouse = "a"
vim.opt.fileencoding = "utf-8"
vim.opt.clipboard:append({ "unnamed", "unnamedplus" })
vim.opt.inccommand = "split"
vim.opt.autoread = true
vim.opt.laststatus = 0
if vim.loop.os_uname().sysname == "Darwin" then
  vim.fn.setenv("CC", "gcc-14")
  vim.fn.setenv("CXX", "g++-14")
end
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
  pattern = "*",
})
vim.api.nvim_create_autocmd("FocusGained", { command = "checktime" })
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "fugitive", "git", "help", "lspinfo", "man", "query", "vim" },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set(
      "n",
      "q",
      "<cmd>close<cr>",
      { buffer = event.buf, silent = true }
    )
  end,
})
vim.api.nvim_create_autocmd({ "BufWritePost" }, {
  pattern = { "*.rs" },
  callback = function()
    vim.lsp.buf.format({ async = false })
  end,
})
