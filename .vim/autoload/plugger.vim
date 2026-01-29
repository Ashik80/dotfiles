" Simple Vim Plugin Manager (Autoload version)
" Place this in ~/.vim/autoload/plugger.vim

" Get the path where plugins will be installed
function! s:GetPluginPath()
    if has('nvim')
        return stdpath('data') . '/plugger'
    else
        return expand('~/.vim/plugger')
    endif
endfunction

" Extract plugin name from GitHub URL
function! s:GetPluginName(url)
    let name = matchstr(a:url, '[^/]\+$')
    let name = substitute(name, '\.git$', '', '')
    return name
endfunction

" Clone or update a plugin
function! s:EnsurePlugin(url)
    let plugin_path = s:GetPluginPath()
    let plugin_name = s:GetPluginName(a:url)
    let install_path = plugin_path . '/' . plugin_name

    " Create plugin directory if it doesn't exist
    if !isdirectory(plugin_path)
        call mkdir(plugin_path, 'p')
    endif

    " Clone if not exists
    if !isdirectory(install_path)
        echo 'Installing ' . plugin_name . '...'
        let result = system('git clone --depth=1 ' . shellescape(a:url) . ' ' . shellescape(install_path))

        if v:shell_error != 0
            echo 'Error installing ' . plugin_name
            return ''
        endif
        echo 'Installed ' . plugin_name
    endif

    return install_path
endfunction

" Add plugin to runtimepath
function! s:LoadPlugin(path)
    if !empty(a:path)
        execute 'set runtimepath+=' . a:path
    endif
endfunction

" Main setup function
function! plugger#setup(plugins)
    " Ensure all plugins are installed and loaded
    for url in a:plugins
        let path = s:EnsurePlugin(url)
        call s:LoadPlugin(path)
    endfor

    " Load all plugin configurations
    packloadall!

    " Run helptags for documentation
    silent! helptags ALL
endfunction

" Update all plugins
function! plugger#update()
    let plugin_path = s:GetPluginPath()

    if !isdirectory(plugin_path)
        echo 'No plugins installed'
        return
    endif

    let plugins = split(globpath(plugin_path, '*'), '\n')
    for plugin_dir in plugins
        let plugin_name = fnamemodify(plugin_dir, ':t')
        if isdirectory(plugin_dir . '/.git')
            echo 'Updating ' . plugin_name . '...'
            let result = system('git -C ' . shellescape(plugin_dir) . ' pull --depth=1')

            if v:shell_error == 0
                echo 'Updated ' . plugin_name
            else
                echo 'Error updating ' . plugin_name
            endif
        endif
    endfor
endfunction

" Clean unused plugins
function! plugger#clean(active_plugins)
    let plugin_path = s:GetPluginPath()

    if !isdirectory(plugin_path)
        return
    endif

    " Get list of active plugin names
    let active_names = {}
    for url in a:active_plugins
        let name = s:GetPluginName(url)
        let active_names[name] = 1
    endfor

    " Remove plugins not in active list
    let installed = split(globpath(plugin_path, '*'), '\n')
    for plugin_dir in installed
        let plugin_name = fnamemodify(plugin_dir, ':t')
        if !has_key(active_names, plugin_name)
            echo 'Removing ' . plugin_name . '...'
            call delete(plugin_dir, 'rf')
        endif
    endfor
endfunction
