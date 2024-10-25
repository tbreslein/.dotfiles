map("n", "Q", "<nop>", "")
map("n", "<esc>", ":noh<cr>", "")
map("t", "jk", "<c-\\><c-n>", "")
map("i", "jk", "<esc>", "")
map({ "n", "i" }, "<c-c><c-c>", "<esc>:wq<cr>", "")

map("v", "P", [["_dP]], "")
map({ "n", "x", "v" }, "x", [["_x]], "")
map("n", "Y", "yg$", "")
map("n", "J", "mzJ`z", "")

map("n", "n", "nzz", "")
map("n", "N", "Nzz", "")
map("n", "*", "*zz", "")
map("n", "#", "#zz", "")
map("n", "g*", "g*zz", "")
map("n", "g#", "g#zz", "")
map("n", "<c-d>", "<c-d>zz", "")
map("n", "<c-u>", "<c-u>zz", "")

map("v", "<", "<gv", "")
map("v", ">", ">gv", "")
map("v", "J", ":m '>+1<cr>gv=gv", "")
map("v", "K", ":m '<-2<cr>gv=gv", "")
map("n", "j", "v:count == 0 ? 'gj' : 'j'", "", { expr = true })
map("n", "k", "v:count == 0 ? 'gk' : 'k'", "", { expr = true })

map("n", "<m-j>", "<c-w>-", "")
map("n", "<m-k>", "<c-w>+", "")