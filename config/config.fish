if status is-interactive
    # Commands to run in interactive sessions can go here
end
if test -d /opt/homebrew
    /opt/homebrew/bin/brew shellenv | source
end
if test -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
    source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
end
