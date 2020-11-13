set -gx PATH "/usr/local/bin" $PATH
set -gx PATH "$HOME/.cargo/bin" $PATH
set -gx PATH "/usr/local/share/arcanist/bin" $PATH
set -gx EDITOR vim

# FZF uses ripgrep
set -gx FZF_DEFAULT_COMMAND  'rg --files --no-ignore-vcs --hidden'

# vim == nvim
alias vim nvim

# Some ssh configuration
fish_ssh_agent 
ssh-add ~/.ssh/id_rsa_work 2> /dev/null
ssh-add ~/.ssh/id_rsa_git 2> /dev/null
ssh-add ~/.ssh/home_pc_rsa 2> /dev/null

# Fish add llvm and rust
set -g fish_user_paths "/usr/local/opt/llvm/bin" $fish_user_paths
set -g fish_user_paths "/home/mattdarcangelo/.cargo/bin" $fish_user_paths
