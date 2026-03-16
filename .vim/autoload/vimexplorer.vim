" autoload/vimexplorer.vim
" Core logic for VimExplorer

" ── Script-local state ─────────────────────────────────────────────────────
" Each explorer buffer keeps its state in a dict keyed by bufnr
let s:state = {}

" ── Public: Open ───────────────────────────────────────────────────────────
function! vimexplorer#Open(path) abort
  " Resolve the directory to open
  if a:path ==# ''
    " Default: directory of the current file, or cwd if no file
    let l:dir = expand('%:p:h')
    if l:dir ==# '' || !isdirectory(l:dir)
      let l:dir = getcwd()
    endif
  else
    let l:dir = fnamemodify(a:path, ':p')
    " Strip trailing slash (except root)
    if l:dir[-1:] ==# '/' && l:dir !=# '/'
      let l:dir = l:dir[:-2]
    endif
  endif

  if !isdirectory(l:dir)
    echohl ErrorMsg | echo 'VimExplorer: not a directory: ' . l:dir | echohl None
    return
  endif

  " Check if we already have a buffer for this dir
  let l:bufname = 'vimexplorer://' . l:dir
  let l:existing = bufnr(l:bufname)

  if l:existing != -1
    " Re-use existing buffer
    execute 'buffer ' . l:existing
  else
    " Create a new scratch buffer
    execute 'edit ' . fnameescape(l:bufname)
  endif

  call s:SetupBuffer(l:dir)
  call s:Render(bufnr('%'))
endfunction

" ── Public: Save (apply edits) ─────────────────────────────────────────────
function! vimexplorer#Save() abort
  let l:bufnr = bufnr('%')
  if !has_key(s:state, l:bufnr)
    echoerr 'VimExplorer: no state for buffer ' . l:bufnr
    return
  endif

  let l:st = s:state[l:bufnr]
  let l:dir = l:st.dir

  " Parse current buffer lines into a list of filenames
  let l:new_names = s:ParseLines(getline(1, '$'))

  " Original names when the buffer was last rendered
  let l:old_names = l:st.names

  " ── Deletions: in old but not in new ──────────────────────────────────
  " Strip trailing slashes for comparison only — new_names may have them
  " (user-typed dirs end with /), old_names never do.
  let l:new_names_bare = map(copy(l:new_names), 'substitute(v:val, "/$", "", "")')

  let l:to_delete = []
  for l:name in l:old_names
    if index(l:new_names_bare, l:name) ==# -1
      call add(l:to_delete, l:name)
    endif
  endfor

  " ── Renames: positional match (same index, different name) ────────────
  " We detect renames by pairing entries that no longer exist on the
  " new side with new entries that did not exist on the old side,
  " in the order they appear.
  let l:old_only = []
  for l:name in l:old_names
    if index(l:new_names_bare, l:name) ==# -1
      call add(l:old_only, l:name)
    endif
  endfor

  let l:new_only = []
  for l:name in l:new_names
    " Use the raw name (with slash if present) so creation logic can
    " detect directories. Skip bare names that already exist in old.
    let l:bare = substitute(l:name, '/$', '', '')
    if l:bare !=# '' && index(l:old_names, l:bare) ==# -1
      call add(l:new_only, l:name)
    endif
  endfor

  " Pair old-only and new-only as renames (index-matched)
  let l:to_rename = []
  let l:new_files = []
  let l:ri = 0
  for l:i in range(len(l:new_only))
    if l:ri < len(l:old_only)
      call add(l:to_rename, [l:old_only[l:ri], l:new_only[l:i]])
      let l:ri += 1
    else
      " More new names than old-only → these are new files to create
      call add(l:new_files, l:new_only[l:i])
    endif
  endfor

  " Any remaining old_only entries that were not matched → deletions
  while l:ri < len(l:old_only)
    if index(l:to_delete, l:old_only[l:ri]) ==# -1
      call add(l:to_delete, l:old_only[l:ri])
    endif
    let l:ri += 1
  endwhile

  " ── Confirm deletions ─────────────────────────────────────────────────
  if !empty(l:to_delete)
    let l:msg = 'VimExplorer: delete ' . len(l:to_delete) . ' item(s)? [y/N] '
    let l:ans = input(l:msg)
    if l:ans !~? '^y'
      echo "\nAborted."
      " Re-render to restore buffer contents
      call s:Render(l:bufnr)
      return
    endif
    echo ''
  endif

  " ── Apply renames ─────────────────────────────────────────────────────
  for [l:old, l:new] in l:to_rename
    if l:new =~# '/'
      echohl ErrorMsg
      echo 'VimExplorer: rename across directories not supported: ' . l:new
      echohl None
      continue
    endif
    let l:src = l:dir . '/' . l:old
    let l:dst = l:dir . '/' . l:new
    if filereadable(l:dst) || isdirectory(l:dst)
      echohl ErrorMsg
      echo 'VimExplorer: target already exists: ' . l:dst
      echohl None
      continue
    endif
    if rename(l:src, l:dst) !=# 0
      echohl ErrorMsg
      echo 'VimExplorer: rename failed: ' . l:src . ' -> ' . l:dst
      echohl None
    else
      echo 'Renamed: ' . l:old . ' → ' . l:new
    endif
  endfor

  " ── Apply deletions ───────────────────────────────────────────────────
  for l:name in l:to_delete
    let l:target = l:dir . '/' . l:name
    if isdirectory(l:target)
      " Use rmdir -r (via system) because Vim's delete() with 'd' is not
      " always recursive on all systems
      let l:out = system('rm -rf ' . shellescape(l:target))
      if v:shell_error !=# 0
        echohl ErrorMsg | echo 'VimExplorer: failed to remove dir: ' . l:target | echohl None
      else
        echo 'Deleted dir: ' . l:name
      endif
    else
      if delete(l:target) !=# 0
        echohl ErrorMsg | echo 'VimExplorer: failed to delete: ' . l:target | echohl None
      else
        echo 'Deleted: ' . l:name
      endif
    endif
  endfor

  " ── Create new files ─────────────────────────────────────────────────
  for l:name in l:new_files
    if l:name =~# '/$'
      " Trailing slash → create directory
      let l:dname = l:dir . '/' . l:name[:-2]
      if mkdir(l:dname, 'p') ==# 0
        echohl ErrorMsg | echo 'VimExplorer: failed to create dir: ' . l:dname | echohl None
      else
        echo 'Created dir: ' . l:name
      endif
    else
      let l:fpath = l:dir . '/' . l:name
      " Check if a file with this name exists somewhere in the original
      " listing's parent directories — i.e. the user copied it from
      " another open explorer buffer. We do this by searching all known
      " explorer buffers for a directory containing this filename.
      let l:src_path = s:FindInOtherBuffers(l:name, l:dir)
      if l:src_path !=# ''
        " Copy: read source and write to destination
        let l:content = readfile(l:src_path, 'b')
        if writefile(l:content, l:fpath, 'b') !=# 0
          echohl ErrorMsg | echo 'VimExplorer: failed to copy to: ' . l:fpath | echohl None
        else
          echo 'Copied: ' . l:src_path . ' → ' . l:fpath
        endif
      else
        " Brand-new file: create empty
        if writefile([], l:fpath) !=# 0
          echohl ErrorMsg | echo 'VimExplorer: failed to create file: ' . l:fpath | echohl None
        else
          echo 'Created: ' . l:name
        endif
      endif
    endif
  endfor

  " ── Re-render ─────────────────────────────────────────────────────────
  call s:Render(l:bufnr)
  setlocal nomodified
endfunction

" ── Public: Refresh ────────────────────────────────────────────────────────
function! vimexplorer#Refresh() abort
  let l:bufnr = bufnr('%')
  if has_key(s:state, l:bufnr)
    call s:Render(l:bufnr)
  endif
endfunction

" ── Public: Enter (open file or directory under cursor) ────────────────────
function! vimexplorer#Enter() abort
  let l:bufnr = bufnr('%')
  if !has_key(s:state, l:bufnr)
    return
  endif
  let l:dir = s:state[l:bufnr].dir
  let l:name = s:NameUnderCursor()
  if l:name ==# ''
    return
  endif

  " Strip detail prefix if present
  let l:target = l:dir . '/' . l:name

  if isdirectory(l:target)
    call vimexplorer#Open(l:target)
  elseif filereadable(l:target)
    execute 'edit ' . fnameescape(l:target)
  else
    echohl WarningMsg
    echo 'VimExplorer: not found: ' . l:target
    echohl None
  endif
endfunction

" ── Public: Go up one directory ────────────────────────────────────────────
function! vimexplorer#Up() abort
  let l:bufnr = bufnr('%')
  if !has_key(s:state, l:bufnr)
    return
  endif
  let l:dir = s:state[l:bufnr].dir
  let l:parent = fnamemodify(l:dir, ':h')
  if l:parent ==# l:dir
    echo 'VimExplorer: already at root'
    return
  endif
  call vimexplorer#Open(l:parent)
endfunction

" ── Public: Toggle hidden files ────────────────────────────────────────────
function! vimexplorer#ToggleHidden() abort
  let l:bufnr = bufnr('%')
  if !has_key(s:state, l:bufnr)
    return
  endif
  let l:st = s:state[l:bufnr]
  let l:st.show_hidden = !l:st.show_hidden
  call s:Render(l:bufnr)
endfunction

" ── Public: Toggle detail (size/perms prefix) ──────────────────────────────
function! vimexplorer#ToggleDetail() abort
  let l:bufnr = bufnr('%')
  if !has_key(s:state, l:bufnr)
    return
  endif
  let l:st = s:state[l:bufnr]
  let l:st.detail = !l:st.detail
  call s:Render(l:bufnr)
endfunction

" ── Public: Open file in a vertical split ──────────────────────────────────
function! vimexplorer#OpenSplit(vertical) abort
  let l:bufnr = bufnr('%')
  if !has_key(s:state, l:bufnr)
    return
  endif
  let l:dir = s:state[l:bufnr].dir
  let l:name = s:NameUnderCursor()
  if l:name ==# '' || isdirectory(l:dir . '/' . l:name)
    return
  endif
  let l:target = l:dir . '/' . l:name
  if a:vertical
    execute 'vsplit ' . fnameescape(l:target)
  else
    execute 'split ' . fnameescape(l:target)
  endif
endfunction

" ── Private: Set up the explorer buffer ────────────────────────────────────
function! s:SetupBuffer(dir) abort
  let l:bufnr = bufnr('%')

  " Buffer-local settings
  setlocal buftype=acwrite      " We handle writes ourselves via BufWriteCmd
  setlocal bufhidden=hide
  setlocal noswapfile
  setlocal filetype=vimexplorer
  setlocal nolist
  setlocal nowrap
  setlocal nonumber
  setlocal norelativenumber
  setlocal nobuflisted
  setlocal statusline=%!vimexplorer#StatusLine()

  " Initialise or update state for this buffer
  if !has_key(s:state, l:bufnr)
    let s:state[l:bufnr] = {}
  endif
  let s:state[l:bufnr].dir         = a:dir
  let s:state[l:bufnr].names       = []
  let s:state[l:bufnr].show_hidden   = get(s:state[l:bufnr], 'show_hidden',   g:vimexplorer_show_hidden)
  let s:state[l:bufnr].detail        = get(s:state[l:bufnr], 'detail',        g:vimexplorer_detail)
  let s:state[l:bufnr].show_header   = get(s:state[l:bufnr], 'show_header',   g:vimexplorer_show_header)

  " Key mappings (buffer-local)
  nnoremap <buffer> <silent> <CR>   :call vimexplorer#Enter()<CR>
  nnoremap <buffer> <silent> -      :call vimexplorer#Up()<CR>
  nnoremap <buffer> <silent> <BS>   :call vimexplorer#Up()<CR>
  nnoremap <buffer> <silent> gh     :call vimexplorer#ToggleHidden()<CR>
  nnoremap <buffer> <silent> gd     :call vimexplorer#ToggleDetail()<CR>
  nnoremap <buffer> <silent> <C-v>  :call vimexplorer#OpenSplit(1)<CR>
  nnoremap <buffer> <silent> <C-s>  :call vimexplorer#OpenSplit(0)<CR>
  nnoremap <buffer> <silent> R      :call vimexplorer#Refresh()<CR>
  nnoremap <buffer> <silent> q      :bdelete<CR>
  nnoremap <buffer> <silent> ?      :call vimexplorer#Help()<CR>
endfunction

" ── Private: Render the buffer contents ────────────────────────────────────
function! s:Render(bufnr) abort
  if !has_key(s:state, a:bufnr)
    return
  endif

  let l:st      = s:state[a:bufnr]
  let l:dir     = l:st.dir
  let l:detail  = l:st.detail
  let l:show_h  = l:st.show_hidden

  " Read directory entries
  let l:entries = s:ReadDir(l:dir, l:show_h)

  " Build the display lines and store the canonical name list
  let l:lines = []
  let l:names = []

  " Header comment (not editable — users should not modify it, but it is
  " just a comment and will be ignored during save parsing)
  if l:st.show_header
    call add(l:lines, '" ' . l:dir)
    call add(l:lines, '" [<CR>] open  [-] up  [gh] hidden  [gd] detail  [?] help')
    call add(l:lines, '')
  endif

  for l:e in l:entries
    let l:display = s:FormatEntry(l:e, l:detail)
    call add(l:lines, l:display)
    call add(l:names, l:e.name)
  endfor

  " Write into buffer without triggering autocommands
  let l:ma = &l:modifiable
  setlocal modifiable
  silent call deletebufline(a:bufnr, 1, '$')
  silent call setbufline(a:bufnr, 1, l:lines)
  setlocal nomodified
  if !l:ma
    setlocal nomodifiable
  endif

  " Save canonical name list (for diffing on save)
  let l:st.names = l:names
endfunction

" ── Private: Read and sort directory entries ───────────────────────────────
function! s:ReadDir(dir, show_hidden) abort
  let l:all = glob(a:dir . '/*', 0, 1)
  if a:show_hidden
    let l:hidden = glob(a:dir . '/.*', 0, 1)
    " Remove . and ..
    let l:hidden = filter(l:hidden, 'v:val !~# "/\\.\\.$" && v:val !~# "/\\.$"')
    let l:all = l:hidden + l:all
  endif

  let l:entries = []
  for l:path in l:all
    let l:name = fnamemodify(l:path, ':t')
    if l:name ==# ''
      continue
    endif
    let l:is_dir = isdirectory(l:path)
    let l:entry  = {
          \ 'name':   l:name,
          \ 'path':   l:path,
          \ 'is_dir': l:is_dir,
          \ 'size':   l:is_dir ? '' : s:HumanSize(getfsize(l:path)),
          \ 'perms':  s:FilePerms(l:path),
          \ }
    call add(l:entries, l:entry)
  endfor

  " Sort: dirs first, then alphabetical (case-insensitive)
  call sort(l:entries, function('s:CompareEntries'))
  return l:entries
endfunction

" ── Private: Format a single entry for display ─────────────────────────────
function! s:FormatEntry(entry, detail) abort
  let l:suffix = a:entry.is_dir ? '/' : ''
  let l:name   = a:entry.name . l:suffix

  if a:detail
    let l:size  = printf('%6s', a:entry.size)
    let l:perms = a:entry.perms
    return l:perms . '  ' . l:size . '  ' . l:name
  else
    return l:name
  endif
endfunction

" ── Private: Get name under cursor (strips detail prefix) ──────────────────
function! s:NameUnderCursor() abort
  let l:line = getline('.')
  " Skip header comment lines and blank lines
  if l:line =~# '^"' || l:line =~# '^\s*$'
    return ''
  endif

  let l:bufnr = bufnr('%')
  let l:detail = get(get(s:state, l:bufnr, {}), 'detail', 0)

  if l:detail
    " Format: "rwxr-xr-x   1.2K  filename"
    " Strip leading perms (10 chars) + 2 spaces + size (6 chars) + 2 spaces
    let l:name = substitute(l:line, '^\S\{10\}\s\+\S*\s\+', '', '')
  else
    let l:name = l:line
  endif

  " Strip trailing slash (dirs)
  let l:name = substitute(l:name, '/$', '', '')
  return trim(l:name)
endfunction

" ── Private: Parse buffer lines into name list (for Save) ──────────────────
function! s:ParseLines(lines) abort
  let l:names = []
  for l:line in a:lines
    " Skip header comment lines and blanks
    if l:line =~# '^"' || l:line =~# '^\s*$'
      continue
    endif

    let l:bufnr = bufnr('%')
    let l:detail = get(get(s:state, l:bufnr, {}), 'detail', 0)

    if l:detail
      let l:name = substitute(l:line, '^\S\{10\}\s\+\S*\s\+', '', '')
    else
      let l:name = l:line
    endif

    " Do NOT strip the trailing slash here — Save() uses it to detect
    " that the user wants a directory created, not a file.
    let l:name = trim(l:name)
    if l:name !=# ''
      call add(l:names, l:name)
    endif
  endfor
  return l:names
endfunction

" ── Private: Sort comparator: dirs first, then alphabetical ────────────────
function! s:CompareEntries(a, b) abort
  if a:a.is_dir && !a:b.is_dir | return -1 | endif
  if !a:a.is_dir && a:b.is_dir | return  1 | endif
  let l:la = tolower(a:a.name)
  let l:lb = tolower(a:b.name)
  return l:la ==# l:lb ? 0 : l:la <# l:lb ? -1 : 1
endfunction

" ── Private: Human-readable file size ──────────────────────────────────────
function! s:HumanSize(bytes) abort
  if a:bytes < 0
    return '?'
  elseif a:bytes < 1024
    return a:bytes . 'B'
  elseif a:bytes < 1024 * 1024
    return (a:bytes / 1024) . 'K'
  elseif a:bytes < 1024 * 1024 * 1024
    return (a:bytes / 1024 / 1024) . 'M'
  else
    return (a:bytes / 1024 / 1024 / 1024) . 'G'
  endif
endfunction

" ── Private: File permissions string ───────────────────────────────────────
function! s:FilePerms(path) abort
  " Use getfperm() — returns e.g. "rwxr-xr-x"
  let l:perms = getfperm(a:path)
  if l:perms ==# ''
    return '----------'
  endif
  " Prefix with d for directories, - for files, l for links
  if isdirectory(a:path)
    return 'd' . l:perms
  elseif getftype(a:path) ==# 'link'
    return 'l' . l:perms
  else
    return '-' . l:perms
  endif
endfunction

" ── Public: Status line helper ─────────────────────────────────────────────
function! vimexplorer#StatusLine() abort
  let l:bufnr = bufnr('%')
  if !has_key(s:state, l:bufnr)
    return ''
  endif
  let l:st  = s:state[l:bufnr]
  let l:h   = l:st.show_hidden ? '[H]' : ''
  let l:d   = l:st.detail      ? '[D]' : ''
  return ' VimExplorer  ' . l:st.dir . '  ' . l:h . l:d
endfunction

" ── Private: Find a file by name in any other open explorer buffer ─────────
" Returns the full path if found, or '' if not found or same directory.
function! s:FindInOtherBuffers(name, current_dir) abort
  for [l:bufnr, l:st] in items(s:state)
    if l:st.dir ==# a:current_dir
      continue
    endif
    let l:candidate = l:st.dir . '/' . a:name
    if filereadable(l:candidate)
      return l:candidate
    endif
  endfor
  return ''
endfunction

" ── Public: Show help ──────────────────────────────────────────────────────
function! vimexplorer#Help() abort
  echo ''
  echo 'VimExplorer key bindings'
  echo '────────────────────────'
  echo '<CR>    Open file / enter directory'
  echo '-       Go up one directory'
  echo '<BS>    Go up one directory'
  echo 'gh      Toggle hidden files'
  echo 'gd      Toggle detail mode (size + perms)'
  echo '<C-v>   Open file in vertical split'
  echo '<C-s>   Open file in horizontal split'
  echo 'R       Refresh listing'
  echo ':w      Apply pending renames / deletions / creations'
  echo 'q       Close explorer'
  echo '?       Show this help'
  echo ''
  echo 'Editing:'
  echo '  Rename  - edit the filename on its line'
  echo '  Delete  - delete the line'
  echo '  Create  - add a new line (end name with / for directory)'
endfunction
