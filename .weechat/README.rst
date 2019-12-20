weechat
=======

::

    /secure passphrase <input-passphrase>
    /secure set <server> <input-password-here>

::

    /print # Bars
    /print # ----------------------------------------

    /bar del -all

    /bar add input1 root bottom 1 0 [mode_indicator],[input_prompt]+(away),[input_search],[input_paste],input_text
    /bar set input1 priority 1000
    /bar del input
    /bar set input1 name input

    /bar add status root bottom 1 0 [time],{tabs},[buffer_count],[buffer_plugin],buffer_number+:+buffer_name+(buffer_modes)+{buffer_nicklist_count}+buffer_zoom+buffer_filter,[lag],completion,scroll,aspell_suggest
    /bar set status color_bg 237
    /bar set status color_fg white
    /bar set status priority 500

    /bar add title window,${inactive} top 1 0 >,buffer_name,>,buffer_title
    /bar set title color_bg 237
    /bar set title priority 500

    /bar add title_active window,${active} top 1 0  >,buffer_name,>,buffer_title
    /bar set title_active color_bg 237
    /bar set title_active color_fg *white
    /bar set title_active priority 500

    /bar default nicklist
    /bar set nicklist conditions ${nicklist}

    /bar add buflist root left 24 1 buflist

::

    /print # Triggers
    /print # ----------------------------------------
    /print # Proper quoting of trigger's arguments is critical!
    /print # See also <https://github.com/weechat/weechat/wiki/Triggers>.

    /trigger default -yes

    /trigger addreplace url_color modifier weechat_print
    /trigger set url_color regex ;\S+://\S+;${color:yellow}${re:0}${color:reset};

    /trigger addreplace last_nick print irc.*;notify_message
    /trigger set last_nick conditions ${tg_displayed} && ${type} == channel && ${tg_tag_nick} != ${nick}
    /trigger set last_nick command /buffer set localvar_set_last_nick ${tg_tag_nick}

    /trigger addreplace complete_last_nick command_run "/input complete_next"
    /trigger set complete_last_nick conditions ${type} == channel && ${buffer.input_buffer_length} == 0 && ${last_nick}
    /trigger set complete_last_nick command /input insert ${last_nick}
