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
          local harpoon = require "harpoon"
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
      "nvim-tree/nvim-web-devicons",
      "nvim-telescope/telescope-fzy-native.nvim",
    },
    keys = {
      {
        "<leader>ff",
        function()
          vim.fn.system "git rev-parse --is-inside-work-tree"
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
        require("telescope.builtin").git_files,
        "telescope git files",
      },
      {
        "<leader>fs",
        require("telescope.builtin").live_grep,
        "telescope git live_grep",
      },
      {
        "<leader>gs",
        require("telescope.builtin").git_branches,
        "telescope git branches",
      },
    },
    config = function()
      require("telescope").setup {
        extensions = {
          fzy_native = {
            override_generic_sorter = false,
            override_file_sorter = true,
          },
        },
      }
      require("telescope").load_extension "fzy_native"
    end,
  },

  {
    "mikavilpas/yazi.nvim",
    event = "VeryLazy",
    keys = {
      {
        "-",
        "<cmd>Yazi<cr>",
        desc = "Open yazi at the current file",
      },
      {
        "<leader>cw",
        "<cmd>Yazi cwd<cr>",
        desc = "Open the file manager in nvim's working directory",
      },
      {
        -- NOTE: this requires a version of yazi that includes
        -- https://github.com/sxyazi/yazi/pull/1305 from 2024-07-18
        "<c-up>",
        "<cmd>Yazi toggle<cr>",
        desc = "Resume the last yazi session",
      },
    },
    opts = {
      -- if you want to open yazi instead of netrw, see below for more info
      open_for_directories = false,
      keymaps = { show_help = "<f1>" },
    },
  },
}
