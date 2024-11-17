local opts = {}

opts.initial = function()
  vim.g.mapleader = " "
  vim.g.maplocalleader = ","
  vim.opt.guicursor = ""
  vim.opt.laststatus = 3
  vim.opt.clipboard:append({"unnamed", "unnamedplus"})
  vim.opt.termguicolors = true
  vim.opt.fillchars:append { eob = " " }
  vim.opt.shortmess:append "aIF"
  vim.opt.cursorline = true
  vim.opt.cursorlineopt = "number"
  vim.opt.colorcolumn = "80"
  vim.opt.signcolumn = "yes"
  vim.opt.ruler = true
  vim.opt.number = true
  vim.opt.relativenumber = true
  vim.opt.breakindent = true
  vim.opt.linebreak = true
  vim.opt.swapfile = false
  vim.opt.undofile = true
  vim.opt.undodir = os.getenv("HOME") .. "/.local/share/vim/undodir"
  vim.opt.autoread = true
  --vim.opt.inccommand = "split"
  if vim.loop.os_uname().sysname == "Darwin" then
    vim.fn.setenv("CC", "gcc-14")
    vim.fn.setenv("CXX", "g++-14")
  end
end

opts.final = function()
  vim.opt.completeopt = { "menuone", "noselect", "noinsert" }
  vim.opt.wildmenu = true
  vim.opt.wildoptions:append("fuzzy")
  vim.opt.pumheight = 10
  vim.opt.ignorecase = true
  vim.opt.smartcase = true
  vim.opt.timeout = false
  vim.opt.updatetime = 400
  vim.opt.confirm = false
  vim.opt.equalalways = false
  vim.opt.splitbelow = true
  vim.opt.splitright = true
  vim.opt.scrolloff = 5
  vim.opt.mouse = "a"

  -- Indenting
  vim.opt.shiftwidth = 2
  vim.opt.smartindent = true
  vim.opt.tabstop = 2
  vim.opt.expandtab = true
  vim.opt.softtabstop = 2
  vim.opt.sidescrolloff = 2

  -- Statusline
  -- local statusline_ascii = ""
  -- vim.opt.statusline = "%#Normal#" .. statusline_ascii .. "%="

  -- Disable providers
  vim.g.loaded_node_provider = 0
  vim.g.loaded_python3_provider = 0
  vim.g.loaded_perl_provider = 0
  vim.g.loaded_ruby_provider = 0

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
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = event.buf, silent = true })
  end,
})
--vim.api.nvim_create_autocmd({ "BufWritePost" }, {
--  pattern = { "*.rs" },
--  callback = function()
--    vim.lsp.buf.format({ async = false })
--  end,
--})
end

--- Load shada after ui-enter
local shada = vim.o.shada
vim.o.shada = ""
vim.api.nvim_create_autocmd("User", {
  pattern = "VeryLazy",
  callback = function()
    vim.o.shada = shada
    pcall(vim.cmd.rshada, { bang = true })
  end,
})

vim.diagnostic.config {
  virtual_text = {
    prefix = "",
    suffix = "",
    format = function(diagnostic)
      return "󰍡 " .. diagnostic.message .. " "
    end,
  },
  underline = {
    severity = { min = vim.diagnostic.severity.WARN },
  },
  signs = {
    text = {
      [vim.diagnostic.severity.HINT] = "󱐮",
      [vim.diagnostic.severity.ERROR] = "✘",
      [vim.diagnostic.severity.INFO] = "◉",
      [vim.diagnostic.severity.WARN] = "",
    },
  },
}

return opts
