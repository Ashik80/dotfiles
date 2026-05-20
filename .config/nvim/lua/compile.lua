local state = {
    buf = nil,
    win = nil,
    compile_command = "",
}

function set_compile_command(opts)
    state.compile_command = table.concat(opts.fargs, " ")
end
vim.api.nvim_create_user_command("SetCompileCommand", set_compile_command, { nargs = "+" })

function compile()
    if state.compile_command == "" then
        print("No compile command set")
        return
    end
    if state.win and vim.api.nvim_win_is_valid(state.win) then
        vim.api.nvim_win_close(state.win, true)
    end
    if state.buf and vim.api.nvim_buf_is_valid(state.buf) then
        vim.api.nvim_buf_delete(state.buf, { force = true })
    end
    local buf = vim.api.nvim_create_buf(false, true)
    local win = vim.api.nvim_open_win(buf, true, {
        split = "below",
    })
    vim.api.nvim_win_set_buf(win, buf)
    state.buf = buf
    state.win = win
    local outputfile = "/tmp/compile.log"
    local command = state.compile_command .. " > " .. outputfile .. " 2>&1"
    vim.fn.termopen(command, {
        on_exit = function(job_id, code, event)
            vim.cmd("edit "..outputfile)
            if code == 0 then
                print("Compiled: "..command)
            else
                print("Failed: "..command)
            end
        end
    })
end
vim.api.nvim_create_user_command("Compile", compile, {})
