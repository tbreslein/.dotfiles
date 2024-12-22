later(function()
  -- this assumes that mason has already been set up
  add({
    source = "rcarriga/nvim-dap-ui",
    depends = {
      "mfussenegger/nvim-dap",
      "nvim-neotest/nvim-nio",
      "williamboman/mason.nvim",
      "jay-babu/mason-nvim-dap.nvim",
      "leoluz/nvim-dap-go",
      "mfussenegger/nvim-dap-python",
    },
  })

  require("mason-nvim-dap").setup({
    ensure_installed = {
      "codelldb",
      "delve",
    },
  })

  local dap, dapui = require("dap"), require("dapui")
  dapui.setup({
    layouts = {
      {
        elements = {
          { id = "scopes", size = 0.5 },
          { id = "breakpoints", size = 0.5 },
          { id = "stacks", size = 0.5 },
          { id = "watches", size = 0.5 },
        },
        position = "left",
        size = 40,
      },
      {
        elements = {
          { id = "repl", size = 0.5 },
          { id = "console", size = 0.5 },
        },
        position = "bottom",
        size = 10,
      },
    },
  })

  dap.listeners.before.attach.dapui_config = function()
    dapui.open()
  end
  dap.listeners.before.launch.dapui_config = function()
    dapui.open()
  end
  dap.listeners.before.event_terminated.dapui_config = function()
    dapui.close()
  end
  dap.listeners.before.event_exited.dapui_config = function()
    dapui.close()
  end

  require("dap-python").setup("python")
  require("dap-go").setup({})

  Map("n", "<leader>dt", function()
    dapui.toggle()
  end)
  Map("n", "<leader>db", function()
    dap.toggle_breakpoint()
  end)
  Map("n", "<leader>dl", function()
    dap.continue()
  end)
  Map("n", "<leader>dj", function()
    dap.step_into()
  end)
  Map("n", "<leader>dk", function()
    dap.step_over()
  end)
  Map("n", "<leader>dt", function()
    if vim.bo.filetype == "python" then
      require("dap-python").test_method()
    elseif vim.bo.filetype == "go" then
      require("dap-go").debug_test()
    end
  end)
end)
