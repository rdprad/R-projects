# shinyR

to compile the most recent R projects in one place.
for a new setup in local machine, follow this instruction: http://r-pkgs.had.co.nz/git.html

# Initial set up
If you’ve never used Git or GitHub before, start by installing Git and creating a GitHub account. Then, link the two together:

Install Git:

Windows: http://git-scm.com/download/win.
OS X: http://git-scm.com/download/mac.
Debian/Ubuntu: sudo apt-get install git-core.
Other Linux distros: http://git-scm.com/download/linux.
Tell Git your name and email address. These are used to label each commit so that when you start collaborating with others, it’s clear who made each change. In the shell, run:

git config --global user.name "YOUR FULL NAME"
git config --global user.email "YOUR EMAIL ADDRESS"
(You can check if you’re set up correctly by running git config --global --list.)

Create an account on GitHub, https://github.com (the free plan is fine). Use the same email address as above.

If needed, generate a SSH key. SSH keys allow you to securely communicate with websites without a password. There are two parts to an SSH key: one public, one private. People with your public key can securely encrypt data that can only be read by someone with your private key.

From R, you can check if you already have an SSH key-pair
