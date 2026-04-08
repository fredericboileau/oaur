FROM archlinux:base-devel

# System update and devtools
RUN pacman -Syu --noconfirm --needed devtools opam git

# Unprivileged user (devtools/makechrootpkg refuse to run as root)
ARG USERNAME=archie
RUN useradd -m -s /bin/bash $USERNAME

# Passwordless sudo for chroot-related commands
RUN echo "$USERNAME ALL=(root) NOPASSWD: SETENV: /usr/bin/makechrootpkg" > /etc/sudoers.d/$USERNAME && \
    echo "$USERNAME ALL=(root) NOPASSWD: /usr/bin/mkarchroot, /usr/bin/arch-nspawn, /usr/bin/install" >> /etc/sudoers.d/$USERNAME && \
    chmod 0440 /etc/sudoers.d/$USERNAME

# Locales
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen && locale-gen
ENV LANG=en_US.UTF-8

# Set up aurutils-style pacman/makepkg configs
RUN install -dm755 /etc/aurutils && \
    cp /usr/share/devtools/pacman.conf.d/extra.conf /etc/aurutils/pacman-x86_64.conf && \
    cp /usr/share/devtools/makepkg.conf.d/x86_64.conf /etc/aurutils/makepkg-x86_64.conf

USER $USERNAME
WORKDIR /home/$USERNAME

# Install deps from dune-project
COPY --chown=$USERNAME dune-project .
RUN opam init --disable-sandboxing --bare -y && \
    opam switch create . ocaml-base-compiler --no-install && \
    opam exec -- dune pkg lock && \
    opam install --deps-only -y .

# Source opam env in login shell
RUN echo 'eval $(opam env)' >> /home/$USERNAME/.bashrc

# Copy source and build
COPY --chown=$USERNAME . /home/$USERNAME/oaur
WORKDIR /home/$USERNAME/oaur
RUN opam exec -- dune build

CMD ["/bin/bash", "--login"]
