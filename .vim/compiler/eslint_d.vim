let g:compiler_name = 'eslint_d'

CompilerSet makeprg=eslint_d\ --format\ compact\ %\ \\\|\ grep\ '.*:\\sline\\s[0-9]*'
CompilerSet errorformat=%f:\ line\ %l\\,\ col\ %c\\,\ %m
