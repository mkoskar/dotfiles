" vim: fdm=marker

syn region diffFoldFile start=/^diff\>/ skip=/^[-+I=*@<>!# 0-9]/ end=/^.*/me=s-1,he=s-1,re=s-1 fold transparent contains=ALL keepend
syn region diffFoldFileFragment start=/^[*@0-9]/ end=/^[*@0-9]/me=s-1,he=s-1,re=s-1 fold transparent contains=ALL containedin=diffFoldFile keepend
