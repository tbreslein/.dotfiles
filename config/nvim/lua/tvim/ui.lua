Now(function()
  Add({source = "zenbones-theme/zenbones.nvim", depends = {"rktjmp/lush.nvim"}})
  vim.opt.termguicolors = true
  vim.o.background = "dark"
  vim.g.zenbones_darken_comments = 45
  vim.cmd.colorscheme("zenwritten")
  -- vim.cmd.colorscheme("neobones")

  -- Add("sainnhe/gruvbox-material")
  -- vim.g.gruvbox_material_enable_italic = 1
  -- vim.g.gruvbox_material_enable_bold = 1
  -- vim.g.gruvbox_material_better_performance = 1
  -- vim.g.gruvbox_material_ui_contrast = "high"
  -- vim.g.gruvbox_material_diagnostic_virtual_text = "highlighted"
  -- vim.g.gruvbox_material_dim_inactive_windows = 1
  -- vim.g.gruvbox_material_float_style = "dim"
  -- vim.g.gruvbox_material_transparent_background = 2
  -- -- vim.o.background = "light"
  -- -- vim.g.gruvbox_material_background = "soft"
  -- vim.cmd.colorscheme("gruvbox-material")

  package.preload["nvim-web-devicons"] = function()
    package.loaded["nvim-web-devicons"] = {}
    require("mini.icons").mock_nvim_web_devicons()
    return package.loaded["nvim-web-devicons"]
  end
end)

Later(function()
  Add({
    source = "nvim-treesitter/nvim-treesitter",
    depends = { "nvim-treesitter/nvim-treesitter-context" },
    hooks = {
      post_checkout = function()
        vim.cmd("TSUpdate")
      end,
    },
  })

  require("nvim-treesitter.configs").setup({
    ensure_installed = {
      "lua",
      "vimdoc",
      "rust",
      "zig",
      "c",
      "cpp",
      "python",
      "go",
      "haskell",
      "astro",
      "javascript",
      "typescript",
      "toml",
      "yaml",
      "markdown",
      "markdown_inline",
    },
    highlight = { enable = true, use_languagetree = true },
    indent = { enable = true },
  })
  require("treesitter-context").setup({ multiline_threshold = 2 })
  vim.cmd([[hi TreesitterContextBottom gui=underline]])

  -- Add("MeanderingProgrammer/render-markdown.nvim")
  -- require("render-markdown").setup({ latex = { enabled = false } })

  local hipatterns = require("mini.hipatterns")
  hipatterns.setup({
    highlighters = {
      fixme = { pattern = "%f[%w]()FIXME()%f[%W]", group = "MiniHipatternsFixme" },
      hack = { pattern = "%f[%w]()HACK()%f[%W]", group = "MiniHipatternsHack" },
      todo = { pattern = "%f[%w]()TODO()%f[%W]", group = "MiniHipatternsTodo" },
      note = { pattern = "%f[%w]()NOTE()%f[%W]", group = "MiniHipatternsNote" },
      perf = { pattern = "%f[%w]()PERF()%f[%W]", group = "MiniHipatternsNote" },
      warn = { pattern = "%f[%w]()WARN()%f[%W]", group = "MiniHipatternsFixme" },
      hex_color = hipatterns.gen_highlighter.hex_color(),
    },
  })
  require("mini.trailspace").setup()
  require("mini.indentscope").setup()

  -- -- snacks lsp progress
  -- local progress = vim.defaulttable()
  -- vim.api.nvim_create_autocmd("LspProgress", {
  --   callback = function(ev)
  --     local client = vim.lsp.get_client_by_id(ev.data.client_id)
  --     local value = ev.data.params.value
  --     if not client or type(value) ~= "table" then
  --       return
  --     end
  --     local p = progress[client.id]
  --
  --     for i = 1, #p + 1 do
  --       if i == #p + 1 or p[i].token == ev.data.params.token then
  --         p[i] = {
  --           token = ev.data.params.token,
  --           msg = ("[%3d%%] %s%s"):format(
  --             value.kind == "end" and 100 or value.percentage or 100,
  --             value.title or "",
  --             value.message and (" **%s**"):format(value.message) or ""
  --           ),
  --           done = value.kind == "end",
  --         }
  --         break
  --       end
  --     end
  --
  --     local msg = {}
  --     progress[client.id] = vim.tbl_filter(function(v)
  --       return table.insert(msg, v.msg) or not v.done
  --     end, p)
  --
  --     local spinner = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
  --     vim.notify(table.concat(msg, "\n"), "info", {
  --       id = "lsp_progress",
  --       title = client.name,
  --       opts = function(notif)
  --         notif.icon = #progress[client.id] == 0 and " "
  --           or spinner[math.floor(vim.uv.hrtime() / (1e6 * 80)) % #spinner + 1]
  --       end,
  --     })
  --   end,
  -- })
end)

