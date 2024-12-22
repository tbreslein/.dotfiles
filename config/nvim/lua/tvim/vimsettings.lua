local g = vim.g
local opt = vim.opt

now(function()
  opt.guicursor = ""
  opt.number = true
  opt.relativenumber = true
  opt.colorcolumn = "80"
  opt.signcolumn = "yes"
  opt.cursorline = true
  opt.cursorlineopt = "number"
  opt.termguicolors = true
  opt.swapfile = false
  opt.backup = false
  opt.undodir = os.getenv("HOME") .. "/.local/share/vim/undodir"
  opt.undofile = true
  opt.autoread = true
  opt.clipboard:append({ "unnamed", "unnamedplus" })
  opt.completeopt = { "menuone", "noselect", "noinsert" }
  opt.fileencoding = "utf-8"
  g.winblend = 0
  g.borderstyle = "single"
  opt.laststatus = 3
  opt.cmdheight = 1

  g.loaded_node_provider = 0
  g.loaded_python3_provider = 0
  g.loaded_perl_provider = 0
  g.loaded_ruby_provider = 0

  local shada = vim.o.shada
  vim.o.shada = ""
  vim.api.nvim_create_autocmd("User", {
    pattern = "VeryLazy",
    callback = function()
      vim.o.shada = shada
      pcall(vim.cmd.rshada, { bang = true })
    end,
  })

  vim.api.nvim_create_autocmd("TextYankPost", {
    group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
    pattern = "*",
    callback = function()
      vim.highlight.on_yank()
    end,
  })

  vim.api.nvim_create_autocmd("FocusGained", { command = "checktime" })
  vim.api.nvim_create_autocmd("FileType", {
    pattern = { "git", "help", "lspinfo", "man", "query", "vim" },
    callback = function(event)
      vim.bo[event.buf].buflisted = false
      Map("n", "q", "<cmd>close<cr>", { buffer = event.buf })
    end,
  })

  Map("n", "Q", "<nop>")
  Map("n", "<esc>", ":noh<cr>")
  Map("v", "P", [["_dP]])
  Map({ "n", "x", "v" }, "x", [["_x]])
  Map("n", "Y", "yg$")
  Map("n", "J", "mzJ`z")
  Map("n", "n", "nzz")
  Map("n", "N", "Nzz")
  Map("n", "*", "*zz")
  Map("n", "#", "#zz")
  Map("n", "g*", "g*zz")
  Map("n", "g#", "g#zz")
  Map("n", "<c-d>", "<c-d>zz")
  Map("n", "<c-u>", "<c-u>zz")
  Map("v", "<", "<gv")
  Map("v", ">", ">gv")
  Map("v", "J", ":m '>+1<cr>gv=gv")
  Map("v", "K", ":m '<-2<cr>gv=gv")
  Map("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true })
  Map("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true })
  Map("n", "]c", ":cnext<cr>")
  Map("n", "[c", ":cprev<cr>")
end)

later(function()
  opt.confirm = false
  opt.equalalways = false
  opt.splitbelow = true
  opt.splitright = true
  opt.timeout = false
  opt.scrolloff = 5
  opt.sidescrolloff = 3
  opt.shiftwidth = 2
  opt.smartindent = true
  opt.tabstop = 2
  opt.expandtab = true
  opt.breakindent = true
  opt.linebreak = true
  opt.fillchars:append({ eob = " " })
  opt.shortmess:append("aIF")
  opt.ignorecase = true
  opt.smartcase = true
  opt.mouse = "a"
  opt.wildmenu = true
  opt.wildoptions:append("fuzzy")
  opt.pumheight = 10
  opt.updatetime = 400
end)
