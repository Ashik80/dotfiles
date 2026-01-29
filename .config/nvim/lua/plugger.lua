local M = {}

-- Get the path where plugins will be installed
local function get_plugin_path()
	local data_path = vim.fn.stdpath('data')
	return data_path .. '/plugger'
end

-- Extract plugin name from GitHub URL
local function get_plugin_name(url)
	return url:match('([^/]+)$'):gsub('%.git$', '')
end

-- Clone or update a plugin
local function ensure_plugin(url)
	local plugin_path = get_plugin_path()
	local plugin_name = get_plugin_name(url)
	local install_path = plugin_path .. '/' .. plugin_name

	-- Create plugin directory if it doesn't exist
	if vim.fn.isdirectory(plugin_path) == 0 then
		vim.fn.mkdir(plugin_path, 'p')
	end

	-- Clone if not exists
	if vim.fn.isdirectory(install_path) == 0 then
		print('Installing ' .. plugin_name .. '...')
		local result = vim.fn.system({
			'git', 'clone', '--depth=1', url, install_path
		})

		if vim.v.shell_error ~= 0 then
			print('Error installing ' .. plugin_name)
			return false
		end
		print('Installed ' .. plugin_name)
	end

	return install_path
end

-- Add plugin to runtimepath
local function load_plugin(path)
	vim.opt.runtimepath:append(path)
end

-- Main setup function
function M.setup(plugins)
	local plugin_path = get_plugin_path()

	-- Ensure all plugins are installed
	for _, url in ipairs(plugins) do
		local path = ensure_plugin(url)
		if path then
			load_plugin(path)
		end
	end

	-- Load all plugin configurations
	vim.cmd('packloadall!')

	-- Run helptags for documentation
	vim.cmd('silent! helptags ALL')
end

-- Update all plugins
function M.update()
	local plugin_path = get_plugin_path()

	if vim.fn.isdirectory(plugin_path) == 0 then
		print('No plugins installed')
		return
	end

	local plugins = vim.fn.readdir(plugin_path)
	for _, plugin_name in ipairs(plugins) do
		local path = plugin_path .. '/' .. plugin_name
		if vim.fn.isdirectory(path .. '/.git') == 1 then
			print('Updating ' .. plugin_name .. '...')
			vim.fn.system({
				'git', '-C', path, 'pull', '--depth=1'
			})

			if vim.v.shell_error == 0 then
				print('Updated ' .. plugin_name)
			else
				print('Error updating ' .. plugin_name)
			end
		end
	end
end

-- Clean unused plugins
function M.clean(active_plugins)
	local plugin_path = get_plugin_path()

	if vim.fn.isdirectory(plugin_path) == 0 then
		return
	end

	-- Get list of active plugin names
	local active_names = {}
	for _, url in ipairs(active_plugins) do
		active_names[get_plugin_name(url)] = true
	end

	-- Remove plugins not in active list
	local installed = vim.fn.readdir(plugin_path)
	for _, plugin_name in ipairs(installed) do
		if not active_names[plugin_name] then
			local path = plugin_path .. '/' .. plugin_name
			print('Removing ' .. plugin_name .. '...')
			vim.fn.delete(path, 'rf')
		end
	end
end

return M
