# Seatbelt - Linux

## TODO - Uncategorized
* Get IP
* Get DNS
* Check running processes
* Cron jobs
    * World writable cron jobs
* Listening ports
* Running services
* GUID/SUID binaries
* Systemd timers
* Firewall
* find / -name password
* PATH
* ENV


## TODO - Categorized

### Files/Permissions
* Get hostname
* Interesting .files
    * ~/.history
* Check if /etc/{sudoers,passwd,shadow,group} is writable
* Read /etc/{sudoers,passwd,shadow,group}
    * Filter
* /var/mail
* /var/www/html
* Check .ssh keys
* /home/ permissions


### Commands
* id
* Check `who`
* uname -a
* Running services
    * Systemd - systemctl list-units -t service --full --all --plain --no-legend
* Screen/tmux sessions
    *  `tmux list-sessions`
