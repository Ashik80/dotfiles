local root = vim.fn.fnamemodify(arg[0], ':p:h:h')
local lines = vim.fn.readfile(root .. '/init.lua')
local source = table.concat(lines, '\n')
local section = source:match('%-%- Emoji selector\n(.-)\n%-%- Fuzzy finder')
assert(section, 'emoji selector section missing')
local callback, exit, started, closed, deleted
local fake = {
    fn = { expand = function() return '/home/test/dotfiles/projscripts/emoji-selector' end,
        termopen = function(cmd, opts) started = cmd; exit = opts.on_exit end },
    cmd = function(cmd) assert(cmd == 'startinsert') end,
    api = {
        nvim_win_is_valid = function(win) return win == 2 end,
        nvim_win_close = function(win) closed = win end,
        nvim_buf_is_valid = function(buf) return buf == 1 end,
        nvim_buf_delete = function(buf) deleted = buf end,
    },
    keymap = { set = function(_, key, fn) assert(key == '<leader>fe'); callback = fn end },
}
local chunk = assert(loadstring(section))
setfenv(chunk, setmetatable({ vim = fake, create_window = function() return 1, 2 end }, { __index = _G }))
chunk()
callback()
assert(started[1] == '/home/test/dotfiles/projscripts/emoji-selector', 'wrong script')
exit()
assert(closed == 2 and deleted == 1, 'terminal was not cleaned up')
print('emoji selector test passed')
