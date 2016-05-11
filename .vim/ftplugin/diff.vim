" See also: vimdiffext <http://www.vim.org/scripts/script.php?script_id=5309>

let s:hunk_pattern = '@@ -\d\+\(,\d\+\)\? +\d\+\(,\d\+\)\? @@'
let s:head_pattern = '\m^--- .\+\n+++ .\+\n'.s:hunk_pattern

function! s:DiffOpen() abort
    if match(getline('.'), '\m^diff .\+$') > -1
        call search(s:head_pattern, 'W')
    endif
    let ln = search(s:head_pattern, 'Wbc')
    if ln > 0
        let p1 = substitute(substitute(getline(ln), '\v^--- "?(.{-})"?(\t.*)?$', '\=submatch(1)', ''), '\m\\"', '"', 'g')
        let p2 = substitute(substitute(getline(ln+1), '\v^\+\+\+ "?(.{-})"?(\t.*)?$', '\=submatch(1)', ''), '\m\\"', '"', 'g')
        exec 'tabnew '.fnameescape(p1)
        exec 'vertical rightbelow diffsplit '.fnameescape(p2)
        wincmd p
    endif
endfunction
command! DiffOpen call s:DiffOpen()
noremap <buffer> <silent> <leader>dc :DiffOpen<CR>

exec "noremap <buffer> <silent> <nowait> [ :call search('".s:head_pattern."', 'Wb') <Bar> normal! 0<CR>"
exec "noremap <buffer> <silent> <nowait> ] :call search('".s:head_pattern."', 'W') <Bar> normal! 0<CR>"

exec "noremap <buffer> <silent> { :call search('".s:hunk_pattern."', 'Wb')<CR>"
exec "noremap <buffer> <silent> } :call search('".s:hunk_pattern."', 'W')<CR>"
