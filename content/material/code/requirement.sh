#!/bin/bash
set -e

sudo apt-get update

# Get pip for Python 3
sudo apt-get install -y python3-pip

# Required for pcapy
sudo apt-get install -y libpcap-dev

# Install Python libraries
pip3 install pcapy dpkt requests picamera dropbox schedule

# Install other packages
sudo apt-get install -y sqlite3 iw libraspberrypi-bin
