#!/bin/sh

sudo rm -r configuration/GenesisFiles

tmux kill-session -t 0

sudo rm -r db-0
sudo rm -r db-1
sudo rm -r db-2
