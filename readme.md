
# TR linux

This is the source repository for the linux port of N6TR's
DOS contest logging program TRlog.

## Building TR linux
I assume you will be running TR in an xterm under X11. There is
an unsupported console only mode and corresponding Makefile in the repository.
You need the following two compilers installed

* Gnu C compiler gcc
* Free pascal compiler fpc

My shack computer runs [Slackware linux](http://www.slackware.com/).
All of the required libraries and headers are part of the standard full
Slackware installation. For other distributions development packages
for these libraries often need to be installed.

* libusb-1.0
* libieee1284
* libcurl
* libasound
* libsndfile

For Slackware you can find a slackbuild script for free pascal
on [Slackbuilds](https://slackbuilds.org/).

Most other distributions will have readily available packages for
all the dependencies. I'll give a detailed description below on
how I built the code on a Raspberry Pi Zero W running Raspberry Pi OS
starting from a fresh
installation.

# Documentation
The documentation is written in latex. On my shack computer
I use texlive which is
available for Slackware
on [Slackbuilds](https://slackbuilds.org/).


# Typical build

Choose a convenient directory and execute the following commands
as a normal (not root) user:

```
   git clone https://github.com/trlinux/trlinux.git
   cd trlinux
   make
```

Once you have cloned the archive, you can build the latest version by
```
   cd trlinux
   git pull
   make
```

If you don't have latex installed, you can build just the programs trlog
and post. After cloning the repository:

```
   cd trlinux
   cd src
   make
   make install
   make -f Makefile.post
```

# Raspberry pi example

Here are all the steps I used to
install trlinux on a headless
Raspberry Pi Zero W starting from a fresh SD card.

## Raspberry Pi initial set up

* Setup the SD card as directed at
[raspberrypi.org](https://www.raspberrypi.org). I used the Raspberry Pi OS
with desktop and recommended software. At this time (August 2021), this
is [2021-05-07-raspios-buster-armhf-full.zip](https://downloads.raspberrypi.org/raspios_full_armhf/images/raspios_full_armhf-2021-05-28/2021-05-07-raspios-buster-armhf-full.zip).
I used the dd command method to copy the image to the SD card.

* I added the
[ssh and wpa_suppliant.conf](https://www.raspberrypi.org/documentation/computers/configuration.html#setting-up-a-headless-raspberry-pi)
files to the boot partition so the Pi Zero W would
come up on my shack wireless network so I could ssh into it from my
shack computer.

* Boot the Pi Zero W and do whatever housekeeping you would normally do,
such as change passwords, adjust ssh settings, add users, etc.

* The end result you need is to be logged in to the Raspberry PI
with a command line prompt
in a terminal, and have a network connection.

## Installing all dependencies

On the Pi Zero W, I first updated all the original software:
```
   sudo apt update
   sudo apt upgrade
```
If this updates the kernel and kernel modules, you will need to reboot.

Next I installed some required and optional software:
```
   sudo apt install libusb-1.0-0-dev
   sudo apt install libieee1284-3-dev
   sudo apt install libcurl4-openssl-dev
   sudo apt install libsndfile1-dev
   sudo apt install libasound-dev
   sudo apt install xterm
   sudo apt install ncat
   sudo apt install texlive
   sudo apt install xpdf
   sudo apt install autotools-dev
   sudo apt install autoconf
   sudo apt install libtool
```

While the free pascal version from apt will compile trlinux, it has
a bug that manifests itself
in the code to list the serial and parallel ports. This is fixed
in the latest version 3.2.2.
I therefore installed the version (3.2.2) for the
Raspberry Pi from sourceforge

Choose a convenient directory and execute
```
wget https://sourceforge.net/projects/freepascal/files/Linux/3.2.2/fpc-3.2.2.arm-linux-raspberry-2.tar
tar -xf fpc-3.2.2.arm-linux-raspberry-2.tar
cd fpc-3.2.2.arm-linux
sudo ./install.sh
```
Follow the prompts.

I needed to tell the free pascal compiler where to find the gcc libraries.
*I never needed to do this on the Intel architectures. This will
probably be fixed eventually and will be unneeded.*
The search directory below will change if the compiler version changes.
Generally the last digit (here 8) is the major compiler version.
```
echo "#INCLUDE /etc/fpc.cfg" > ~/.fpc.cfg
echo "-Fl/usr/lib/gcc/arm-linux-gnueabihf/8" >> ~/.fpc.cfg
```

Hamlib is optional but recommended; trlinux has internal support for
Elecraft, Kenwood, and most Icom rigs. However to share
rig control with other programs, hamlib's rigctld works well.
I installed the current bleeding edge
hamlib by following the directions
[here](https://rigpi.net/help/installing-the-latest-hamlib.html).
Choose a convenient directory and execute:
```
   git clone https://github.com/Hamlib/Hamlib.git
   cd Hamlib
   ./bootstrap
   ./configure
   make
   sudo make install
   sudo ldconfig
```
To be precise, the Hamlib commit shown by git log for this
build was
a0672e4f7e600f4ad9b887c11880baa6e9ab4052 but that shouldn't matter.

Finally, to build trlinux, execute the usual:
```
   git clone https://github.com/trlinux/trlinux.git
   cd trlinux
   make
```

At this point, the codes and directory structure are the same as in the
release tar balls. Follow the documentation in the trlinux/doc directory
to properly set up an xterm and interface to your rig.
