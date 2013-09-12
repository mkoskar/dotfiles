-- ~/.imapfilter/config.lua

-- options
options.timeout = 60
options.subscribe = true

-- accounts
mgm = IMAP {
    server = '',
    username = '',
    password = '',
    ssl = 'ssl3',
}

-- mgm
context = mgm.INBOX
--context = mgm.INBOX:is_recent()
--context = mgm.INBOX:is_unseen()

mgm['INBOX/jira']:is_older(90):delete_messages()
