augroup FormatWithBlack
	au! BufWritePost * silent execute '!black %'
augroup END
