local function clipboard_command(copy)
    local wayland = vim.env.WAYLAND_DISPLAY
    if not wayland or wayland == "" then
        wayland = "wayland-0"
    end
    local socket = (vim.env.XDG_RUNTIME_DIR or "") .. "/" .. wayland
    if vim.fn.executable(copy and "wl-copy" or "wl-paste") == 1
        and ((vim.env.WAYLAND_DISPLAY or "") ~= "" or vim.uv.fs_stat(socket)) then
        return copy and { "wl-copy" } or { "wl-paste", "--no-newline" }, { WAYLAND_DISPLAY = wayland }
    elseif (vim.env.DISPLAY or "") ~= "" and vim.fn.executable("xclip") == 1 then
        return { "xclip", copy and "-i" or "-o", "-selection", "clipboard" }
    elseif not copy and vim.env.TMUX then
        return { "sh", "-c", "tmux refresh-client -l && sleep 0.05 && tmux save-buffer -" }
    end
end

local function copy_clipboard(lines)
    -- Shared panes must reach both the desktop and OSC 52-capable clients.
    if (vim.env.TMUX or "") ~= "" then
        local encoded = vim.base64.encode(table.concat(lines, "\n"))
        vim.api.nvim_ui_send("\027Ptmux;\027\027]52;c;" .. encoded .. "\007\027\\")
    else
        require("vim.ui.clipboard.osc52").copy("+")(lines)
    end
    local command, env = clipboard_command(true)
    if command then
        vim.system(command, { stdin = table.concat(lines, "\n"), env = env, text = true }, function(result)
            if result.code ~= 0 then
                vim.schedule(function()
                    vim.notify("Clipboard copy failed: " .. result.stderr, vim.log.levels.WARN)
                end)
            end
        end)
    end
end

local function paste_clipboard()
    local command, env = clipboard_command(false)
    if not command then
        return {}
    end
    local result = vim.system(command, { env = env, text = true }):wait()
    if result.code ~= 0 then
        vim.notify("Clipboard paste failed: " .. result.stderr, vim.log.levels.WARN)
        return {}
    end
    return vim.split(result.stdout, "\n", { plain = true })
end

vim.g.clipboard = {
    name = "Desktop and OSC 52",
    copy = { ["+"] = copy_clipboard, ["*"] = copy_clipboard },
    paste = { ["+"] = paste_clipboard, ["*"] = paste_clipboard },
    cache_enabled = 0,
}
