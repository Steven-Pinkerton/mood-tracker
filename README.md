# mood-tracker

This is a web-application for tracking moods. Run bin/run to start the program.

## Development


...

To run CI locally,

```
nix --option sandbox false build .#check
```

## Deployment

To deploy this code to a remote machine. 

- Edit the flake.nix file so that the hostname is set to the IP address of the machine you wish to deploy to.
- Run the following command `nix run github:serokell/deploy-rs -- -s`