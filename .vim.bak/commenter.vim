" comment visually selected block
function! CommentBlock(comment_string)
    execute "norm `<^"
    let l:starting_line = line("'<")
    let l:starting_col = col(".")
    let l:end_line = line("'>")

    execute l:starting_line.",".l:end_line."norm ".l:starting_col."|i".a:comment_string." "
endfunction

function! CommentLine(comment_string)
    execute "norm I".a:comment_string." "
endfunction
