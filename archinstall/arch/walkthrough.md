# walkthrough through my installation

- archinstall with minimal desktop
- install vim
- edit /etc/pacman.conf
  - uncomment `Color`, `VerbosePkgLists` and `ParallelDownloads = 5`
- edit /boot/loader/loader.conf and set timeout to 0
- install yay
- install rustup and nix
- install egl-wayland
- install nvidia and nvidia-utils: `sudo pacman -S nvidia-open-dkms nvidia-utils nvidia-settings lib32-nvidia-utils`
- create and edit /etc/modprobe.d/nvidia_drm.conf:
  - `options nvidia_drm modeset=1`
- rebuild initramfs `sudo mkinitcpio -P` and reboot
- startup ufw:
  - `sudo ufw enable`
  - `sudo default deny`
  - `sudo allow syncthing`
- enable greetd: `sudo systemctl enable greetd`
