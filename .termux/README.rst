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
        make \
        man \
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

    ~/.secrets

    ~/.ssh
        * config
        * config.tunnels
        * ssh-agent.presets
        * <keys>

    ~/.weechat
        * sec.conf

    $SYSPREFIX/etc/ssh
        * sshd_config
        * <keys>
