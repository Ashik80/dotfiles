setlocal tabstop=2 shiftwidth=2

command! FormatPrettier execute 'silent !npx prettier -w %'

nmap <leader>fp :FormatPrettier<CR>
