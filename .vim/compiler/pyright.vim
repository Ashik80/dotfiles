let g:compiler_name = 'pyright'

CompilerSet makeprg=pyright\ %\ \\\|\ grep\ '.*:[0-9]*:[0-9]*'\ \\\|\ sed\ 's/^\s*//'
CompilerSet errorformat=%f:%l:%c\ %m
