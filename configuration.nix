{ config, pkgs, lib, ... }:

{

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 443 19999 3000 ];
  };

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };

  users.users."root".openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAACNzaC1lZDI1NTE5AAAAIPwjDcLZ5jrK3XL5dF7eVb+9wPWqUBLf4NCuRzmRiteT steven@DESKTOP-4IURNHR"
  ];

  services.netdata = {
    enable = true;
  };

  environment = {
    variables = {
      EDITOR = "nvim";
      LC_ALL = config.i18n.defaultLocale;
      TERM = "xterm-256color";
    };
  };

  time.timeZone = "Europe/Amsterdam";

  i18n.defaultLocale = "en_GB.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "uk";
  };

  environment.systemPackages = with pkgs; [
    neovim
    wget
  ];
  programs.bash.shellInit = ''
    TERM=xterm
  '';
}
