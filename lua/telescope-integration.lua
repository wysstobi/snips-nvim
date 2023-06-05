
local pickers      = require "telescope.pickers"
local finders      = require "telescope.finders"
local conf         = require("telescope.config").values
local actions      = require "telescope.actions"
local action_state = require "telescope.actions.state"
local previewers   = require "telescope.previewers"

local attach_mappings = function (prompt_bufnr, _)
  actions.select_default:replace(function ()
    actions.close(prompt_bufnr)
    local selection = action_state.get_selected_entry()
    vim.cmd('HandleTelescopeSelection ' .. selection[1])

    return selection[1]
  end)

  return true
end

local marker = function(entry)
  return {
    value = entry,
    display = entry[1] .. "display",
    ordinal = entry[1],
  }
end

local function getKeysFromTable(snippetTable)
  local keyTable = {}
  for key, _ in pairs(snippetTable) do
    table.insert(keyTable, key)
    print(key)
  end
  return keyTable
end

function Run(table)
  local keys = getKeysFromTable(table)
  local opts = {
    prompt_title = "Search",
    layout_strategy = "horizontal",
    layout_config = { height = 0.95 },
    finder = finders.new_table(keys),
    sorter = conf.generic_sorter(),
    attach_mappings = attach_mappings,
    entry_maker = marker,
    previewer = previewers.new_buffer_previewer{
      title = "Snippet Preview",
      define_preview = function (self, entry, _)
        vim.api.nvim_buf_set_option(self.state.bufnr, 'filetype', table[entry[1]]["filetype"][1])
        vim.api.nvim_buf_set_lines(self.state.bufnr, 0, -1, false, table[entry[1]]["content"])
      end
    }}
    local names = pickers.new(opts)
    names:find()
  end
