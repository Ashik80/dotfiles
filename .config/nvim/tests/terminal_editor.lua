local root = vim.fn.fnamemodify(arg[0], ':p:h:h')
local parent = vim.fn.jobstart({ vim.v.progpath, '--embed', '--headless', '-u', 'NONE' }, { rpc = true })
local function remote(code, ...)
    return vim.rpcrequest(parent, 'nvim_exec_lua', code, { ... })
end
local clients, files = {}, {}
local repo = vim.fn.tempname()
local function check(condition, message)
    assert(condition, message)
end
local ok, err = pcall(function()
    remote("vim.opt.runtimepath:prepend(...); vim.g.mapleader = ' '", root)
    local socket = remote("return vim.fn.serverstart()")
    local function edit(name)
        local path = vim.fn.tempname() .. ' ' .. name
        table.insert(files, path)
        vim.fn.writefile({ 'old prompt' }, path)
        local output = {}
        local client = vim.fn.jobstart({ root .. '/editor.lua', path }, {
            env = { NVIM = socket },
            on_stderr = function(_, lines) vim.list_extend(output, lines) end,
        })
        table.insert(clients, client)
        check(vim.wait(3000, function()
            return remote('return vim.api.nvim_buf_get_name(0)') == path
                or vim.fn.jobwait({ client }, 0)[1] ~= -1
        end), 'editor did not open')
        check(remote('return vim.api.nvim_buf_get_name(0)') == path, 'prompt not opened: ' .. table.concat(output, '\n'))
        check(vim.fn.jobwait({ client }, 0)[1] == -1, 'client must wait for editing')
        return client, path
    end

    local client, path = edit('prompt.txt')
    remote("vim.api.nvim_buf_set_lines(0, 0, -1, false, { 'new prompt', 'second line' }); vim.cmd('EditorDone')")
    check(vim.fn.jobwait({ client }, 3000)[1] == 0, 'save must succeed')
    check(vim.deep_equal(vim.fn.readfile(path), { 'new prompt', 'second line' }), 'prompt was not saved')
    check(remote('return #vim.api.nvim_list_wins()') == 1, 'original window must survive')

    client, path = edit('cancel.txt')
    remote("vim.api.nvim_buf_set_lines(0, 0, -1, false, { 'discard me' }); vim.cmd('EditorCancel')")
    check(vim.fn.jobwait({ client }, 3000)[1] == 1, 'cancel must fail the editor command')
    check(vim.fn.readfile(path)[1] == 'old prompt', 'cancel changed the file')

    client = edit('wq.txt')
    remote("vim.cmd('wq')")
    check(vim.fn.jobwait({ client }, 3000)[1] == 0, ':wq must finish without quitting the parent')
    check(remote('return #vim.api.nvim_list_wins()') == 1, 'parent exited after :wq')

    client = edit('quit.txt')
    remote("vim.api.nvim_buf_set_lines(0, 0, -1, false, { 'unsaved' }); vim.cmd('q!')")
    check(vim.fn.jobwait({ client }, 3000)[1] == 1, 'discarded changes must fail')

    client, path = edit('write-error.txt')
    check(not remote("vim.api.nvim_buf_set_lines(0, 0, -1, false, { 'unsaved' }); vim.bo.readonly = true; return pcall(vim.cmd, 'EditorDone')"),
        'read-only write should fail')
    check(vim.fn.jobwait({ client }, 200)[1] == -1, 'failed write must keep waiting')
    remote("vim.bo.readonly = false; vim.cmd('EditorCancel')")
    check(vim.fn.jobwait({ client }, 3000)[1] == 1, 'failed write could not be cancelled')

    local function git(...)
        local args = { 'git', '-C', repo }
        vim.list_extend(args, { ... })
        local output = vim.system(args, { text = true }):wait()
        check(output.code == 0, output.stderr)
        return vim.trim(output.stdout)
    end
    vim.fn.mkdir(repo)
    git('init', '-q')
    git('config', 'user.name', 'Editor Test')
    git('config', 'user.email', 'editor-test@example.invalid')
    local function commit(cancel)
        local job = vim.fn.jobstart({ 'git', '-C', repo, 'commit', '--allow-empty', '-q' }, {
            env = { NVIM = socket, GIT_EDITOR = root .. '/editor.lua' },
        })
        table.insert(clients, job)
        check(vim.wait(3000, function()
            return remote('return vim.api.nvim_buf_get_name(0)') == repo .. '/.git/COMMIT_EDITMSG'
        end), 'Git did not open its message in the parent')
        remote("vim.api.nvim_buf_set_lines(0, 0, -1, false, { 'Native editor commit' }); vim.cmd(...)",
            cancel and 'EditorCancel' or 'EditorDone')
        check(vim.fn.jobwait({ job }, 3000)[1] == (cancel and 1 or 0), 'unexpected Git commit result')
    end
    commit(false)
    check(git('log', '-1', '--format=%s') == 'Native editor commit', 'Git did not use the saved message')
    commit(true)
    check(git('rev-list', '--count', 'HEAD') == '1', 'cancel created a commit')

    client = edit('disconnect.txt')
    vim.fn.jobstop(parent)
    check(vim.fn.jobwait({ client }, 3000)[1] == 1, 'parent exit must fail, not hang')
end)
for _, client in ipairs(clients) do pcall(vim.fn.jobstop, client) end
pcall(vim.fn.jobstop, parent)
for _, path in ipairs(files) do vim.fn.delete(path) end
vim.fn.delete(repo, 'rf')
if not ok then
    io.stderr:write(tostring(err) .. '\n')
    vim.cmd('cquit 1')
end
print('terminal editor tests passed')
