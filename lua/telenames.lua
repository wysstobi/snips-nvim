
local pickers = require "telescope.pickers"
local finders = require "telescope.finders"
local conf = require("telescope.config").values
local actions = require "telescope.actions"
local action_state = require "telescope.actions.state"


function MyFunction (tobi)
   print("helo telenames" .. tobi)
   return "helo from MyFunction (telenames)"
end

--local s = vim.api.nvim_command_output('echo FprodTeam()')

-- t={}
-- for i in string.gmatch(s, "%P+") do
-- 		table.insert(t,i)
-- end

local attach_mappings = function (prompt_bufnr, map)
		actions.select_default:replace(function ()
				actions.close(prompt_bufnr)
				local selection = action_state.get_selected_entry()
				--vim.api.nvim_put({ selection[1] }, "", false, true)
               vim.cmd('HandleTeleScopeSelection ' .. selection[1])

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

-- local previewer = Previewer:new(opts)



--
--print(vim.inspect(t))
function run(table)
        local opts = {
                prompt_title = "best guys ever",
                layout_strategy = "center",
                finder = finders.new_table(table),
                sorter = conf.generic_sorter(),
                attach_mappings = attach_mappings,
                entry_maker = marker,
        }
        local names = pickers.new(opts)
        names:find()
end


