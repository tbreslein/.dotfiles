local harpoon = require "harpoon"
harpoon.setup { settings = { save_on_toggle = true } }
for i, v in ipairs { "r", "e", "w", "q", "t" } do
  map("n", "<m-" .. v .. ">", function()
    harpoon:list():select(i)
  end, "harpoon select " .. tostring(i))
end
map("n", "<leader>a", function()
  harpoon:list():add()
end, "harpoon add to list")
map("n", "<leader>e", function()
  harpoon.ui:toggle_quick_menu(harpoon:list())
end, "harpoon toggle quick menu")

require("telescope").setup {
  extensions = {
    fzy_native = {
      override_generic_sorter = false,
      override_file_sorter = true,
    },
  },
}
require("telescope").load_extension "fzy_native"
local builtin = require "telescope.builtin"
map("n", "<leader>ff", function()
  vim.fn.system "git rev-parse --is-inside-work-tree"
  if vim.v.shell_error == 0 then
    builtin.git_files()
  else
    builtin.find_files()
  end
end, "telescope files")
map("n", "<leader>fg", builtin.git_files, "telescope git_files")
map("n", "<leader>fs", builtin.live_grep, "telescope live_grep")
map("n", "<leader>gs", builtin.git_branches, "telescope git branches")

require("oil").setup {}
map("n", "-", "<cmd>Oil<cr>", "Oil")
