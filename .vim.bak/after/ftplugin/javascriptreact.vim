runtime! ftplugin/javascript.vim
runtime! snippet_helpers.vim

" snippets mappings
" function component (rfc)
iabbrev rfc import React from "react";<CR><CR>export default function ComponentName() {<CR>return (<CR><div>hello</div><CR>)<CR>}<Esc>gg?ComponentName<CR>viw

" arrow component (rafc)
iabbrev rafc import React from "react";<CR><CR>const ComponentName = () => {<CR>return (<CR><div>hello</div><CR>);<CR>};<CR><CR>export default ComponentName;<Esc>gg?ComponentName<CR>viw

iabbrev img <img src="" alt="" /><ESC>F<f"a
