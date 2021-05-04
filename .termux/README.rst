Termux
======

::

    $ pkg upgrade

    $ pkg install openssh
    $ passwd
    $ sshd

    $ pkg install \
        aspell-en \
        autossh \
        bash-completion \
        bsdtar \
        file \
        fzf \
        git \
        htop \
        linux-man-pages \
        make \
        man \
        mpv \
        ncurses-utils \
        neovim \
        netcat \
        openssl-tool \
        parallel \
        proot \
        ranger \
        ripgrep \
        socat \
        termux-api \
        tmux \
        tree \
        vim \
        weechat \
        weechat-perl-plugin \
        weechat-python-plugin \
        zsh

    $ rm -rf .* *
    $ git clone --recursive <dotfiles> .
