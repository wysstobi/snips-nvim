
local pickers = require "telescope.pickers"
local finders = require "telescope.finders"
local conf = require("telescope.config").values
local actions = require "telescope.actions"
local action_state = require "telescope.actions.state"
local previewers = require "telescope.previewers"


local attach_mappings = function (prompt_bufnr, map)
		actions.select_default:replace(function ()
				actions.close(prompt_bufnr)
				local selection = action_state.get_selected_entry()
				--vim.api.nvim_put({ selection[1] }, "", false, true)
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

local function printTable( t )
 
    local printTable_cache = {}
 
    local function sub_printTable( t, indent )
 
        if ( printTable_cache[tostring(t)] ) then
            print( indent .. "*" .. tostring(t) )
        else
            printTable_cache[tostring(t)] = true
            if ( type( t ) == "table" ) then
                for pos,val in pairs( t ) do
                    if ( type(val) == "table" ) then
                        print( indent .. "[" .. pos .. "] => " .. tostring( t ).. " {" )
                        sub_printTable( val, indent .. string.rep( " ", string.len(pos)+8 ) )
                        print( indent .. string.rep( " ", string.len(pos)+6 ) .. "}" )
                    elseif ( type(val) == "string" ) then
                        print( indent .. "[" .. pos .. '] => "' .. val .. '"' )
                    else
                        print( indent .. "[" .. pos .. "] => " .. tostring(val) )
                    end
                end
            else
                print( indent..tostring(t) )
            end
        end
    end
 
    if ( type(t) == "table" ) then
        print( tostring(t) .. " {" )
        sub_printTable( t, "  " )
        print( "}" )
    else
        sub_printTable( t, "  " )
    end
end

local function getKeysFromTable(snippetTable)
local keyTable = {}
  for key,value in pairs(snippetTable) do
    table.insert(keyTable, key)
    print(key)
  end
  return keyTable
end

function run(table)
       local keys = getKeysFromTable(table)
       local opts = {
               prompt_title = "best guys ever",
               layout_strategy = "horizontal",
               layout_config = { height = 0.95 },
               finder = finders.new_table(keys),
               sorter = conf.generic_sorter(),
               attach_mappings = attach_mappings,
               entry_maker = marker,
               previewer = previewers.new_buffer_previewer{
                     title = "My preview",
                     define_preview = function (self, entry, status)
                             vim.api.nvim_buf_set_lines(self.state.bufnr, 0, -1, false, table[entry[1]])
                     end
               }}
       local names = pickers.new(opts)
       names:find()
end
