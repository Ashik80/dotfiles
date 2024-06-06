let g:compiler_name = 'tscn'

CompilerSet makeprg=tsc\ --incremental\ --noEmit\ \\\|\ grep\ '.*([0-9]*,[0-9]*):'
CompilerSet errorformat=%f(%l\\,%c):%m
