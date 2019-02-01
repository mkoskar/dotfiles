# vim: ft=muttrc

save-hook . $my_base/%O

macro index,pager \e1 <change-folder>$spoolfile<enter> 'go to spoolfile mailbox'
macro index,pager \e2 <change-folder>$record<enter> 'go to record mailbox'
macro index,pager \e3 <change-folder>$postponed<enter> 'go to postponed mailbox'
macro index,pager \e4 <change-folder>$my_mbarchived<enter> 'go to archived mailbox'
macro index,pager \e0 <change-folder>$my_mbtrash<enter> 'go to trash mailbox'

macro index \& <tag-prefix><copy-message>$my_mbarchived<enter><previous-entry><sync-mailbox> 'archive messages'
macro pager \& <copy-message>$my_mbarchived<enter><previous-entry><sync-mailbox> 'archive message'

macro index,pager,browser y <change-folder>?<change-dir>^U$my_base<enter> "browse account's folder"
