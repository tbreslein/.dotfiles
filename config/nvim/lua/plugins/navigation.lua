return {
  {
    "ThePrimeagen/harpoon",
    event = "VeryLazy",
    dependencies = { "nvim-lua/plenary.nvim" },
    branch = "harpoon2",
    keys = {
      {
        "<m-r>",
        function()
          require("harpoon"):list():select(1)
        end,
        "harpoon select 1",
      },
      {
        "<m-e>",
        function()
          require("harpoon"):list():select(2)
        end,
        "harpoon select 2",
      },
      {
        "<m-w>",
        function()
          require("harpoon"):list():select(3)
        end,
        "harpoon select 3",
      },
      {
        "<m-q>",
        function()
          require("harpoon"):list():select(4)
        end,
        "harpoon select 4",
      },
      {
        "<m-t>",
        function()
          require("harpoon"):list():select(5)
        end,
        "harpoon select 5",
      },
      {
        "<leader>a",
        function()
          require("harpoon"):list():add()
        end,
        "harpoon add to list",
      },
      {
        "<leader>e",
        function()
          local harpoon = require("harpoon")
          harpoon.ui:toggle_quick_menu(harpoon:list())
        end,
        "harpoon add to list",
      },
    },
    opts = { settings = { save_on_toggle = true } },
  },

  {
    "nvim-telescope/telescope.nvim",
    event = "VeryLazy",
    dependencies = {
      "nvim-telescope/telescope-fzy-native.nvim",
      "nvim-telescope/telescope-live-grep-args.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    keys = {
      {
        "<leader>ff",
        function()
          vim.fn.system("git rev-parse --is-inside-work-tree")
          if vim.v.shell_error == 0 then
            require("telescope.builtin").git_files()
          else
            require("telescope.builtin").find_files()
          end
        end,
        "telescope files",
      },
      {
        "<leader>fg",
        function()
          require("telescope.builtin").git_files()
        end,
        "telescope git files",
      },
      {
        "<leader>fs",
        function()
          require("telescope").extensions.live_grep_args.live_grep_args()
        end,
        "telescope git live_grep",
      },
      {
        "<leader>gs",
        function()
          require("telescope.builtin").git_branches()
        end,
        "telescope git branches",
      },
    },
    config = function()
      require("telescope").setup({
        extensions = {
          fzy_native = {
            override_generic_sorter = false,
            override_file_sorter = true,
          },
          live_grep_args = {
            auto_quoting = true,
            mappings = { -- extend mappings
              i = {
                ["<C-k>"] = require("telescope-live-grep-args.actions").quote_prompt(),
                ["<C-i>"] = require("telescope-live-grep-args.actions").quote_prompt({
                  postfix = " --iglob ",
                }),
              },
            },
          },
        },
      })
      require("telescope").load_extension("fzy_native")
      require("telescope").load_extension("live_grep_args")
    end,
  },

  {
    "stevearc/oil.nvim",
    keys = {
      {
        "-",
        "<cmd>Oil<cr>",
        "oil",
      },
    },
    opts = { keymaps = { ["q"] = "actions.close" } },
  },

  {
    "aserowy/tmux.nvim",
    opts = {},
  },
}
