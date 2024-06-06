let g:compiler_name = 'ruster'

CompilerSet makeprg=cargo\ clippy\ 2>&1\ \\\|\ sed\ '{N;\ s/warning\\(.*\\)\\n\\s*-->\\s*\\(.*\\)/\\2:\ warning\\1/g};\ {N;\ s/error\\(.*\\)\\n\\s*-->\\s*\\(.*\\)/\\2:\ error\\1/g}'\ \\\|\ grep\ -v\ '^\\s'\ \\\|\ grep\ '.*:[0-9]*:[0-9]*:'
CompilerSet errorformat=%f:%l:%c:\ %m
