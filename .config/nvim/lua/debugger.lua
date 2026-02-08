local dap = require('dap')
local dapui = require('dapui')
local home = os.getenv("HOME")
dapui.setup()
dap.adapters["pwa-node"] = {
    type = "server",
    host = "localhost",
    port = "${port}",
    executable = {
        command = "node",
        args = {home.."/make-builds/js-debug/src/dapDebugServer.js", "${port}"},
    }
}
dap.configurations.javascript = {
    {
        type = "pwa-node",
        request = "launch",
        name = "Launch file",
        program = "${file}",
        cwd = "${workspaceFolder}",
    },
    {
        type = "pwa-node",
        request = "launch",
        name = "Launch API (npm start)",
        runtimeExecutable = "npm",
        runtimeArgs = { "run", "start" },
        cwd = "${workspaceFolder}",
        console = "integratedTerminal",
    },
}
dap.configurations.typescript = dap.configurations.javascript
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
vim.keymap.set('n', '<leader>db', dap.toggle_breakpoint)
vim.keymap.set('n', '<leader>dc', dap.continue)
vim.keymap.set('n', '<leader>ds', dap.step_over)
vim.keymap.set('n', '<leader>dr', dap.clear_breakpoints)
vim.keymap.set('n', '<leader>dt', dap.terminate)
