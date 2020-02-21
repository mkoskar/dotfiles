.termux
=======

::

    $ apt update
    $ apt full-upgrade
    $ apt install openssh

    $ passwd
    $ sshd
    $ truncate -s 0 $PREFIX/etc/motd

    $ apt install \
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
        ncurses-utils \
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
    $ git clone --recurse-submodules <dotfiles> .
