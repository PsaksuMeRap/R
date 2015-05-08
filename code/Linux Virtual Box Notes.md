# How to Setup various Linux Distros in Virtual Box

## Ubuntu
1. Install Ubuntu on Virtual Box
2. Install "Guest Additions" http://www.virtualbox.org/manual/ch04.html#sharedfolders
3. Setup Shared Folders https://www.virtualbox.org/manual/ch04.html#sharedfolders

## Fedora
1.  Download Fedora-Live-Desktop-x86_64-20-1.iso
2.  Set up Virtual Desktop for 2 GB RAM and 8 GB HD space
3.  Set "Shared Clipboard" to bidirectional
4.  Set "Drag-n-Drop" to bidirectional
5.  Add shared folder /Users/dlviar as auto-mount
6.  Start the VM
7.  Point the VM to the Fedora iso
8.  Install Fedora.  
    gcc and kernel-devel was not included with the generic desktop install so,
    To install Guest Additions, you need to follow: https://ask.fedoraproject.org/en/question/24349/how-do-i-run-yum-complete-transaction-as-a-root-for-updates/
    https://www.centos.org/forums/viewtopic.php?t=5603
9.  su -c "yum install -y gcc kernel-devel"
10. Put in root password.
11. su - c "yum update"
12. NOTE:  To get out of the screen saver, hit ESC
13. restart
14. Run the Virtual Box Desktop Additions
15. Now add the user to the VirtualBox Group vboxsf:
    su -c "usermod -a -G vboxsf dan"
16. Restart
17. Once all this was done, I was able to save a file from Fedora into the Mac host
    I was also able to copy (ctrl+C) from Fedora and paste (Command+V) into the Mac.

## OpenSUSE
1.  Download openSUSE-13.1-DVD-x86_64.iso
2.  Set up Virtual Desktop for 2 GB RAM and 20 GB HD space
3.  Set "Shared Clipboard" to bidirectional
4.  Set "Drag-n-Drop" to bidirectional
5.  Add shared folder /Users/dlviar as auto-mount
6.  Start the VM
7.  Point the VM to the openSUSE iso
8.  Install openSUSE
    Now follow the instructions from https://www.virtualbox.org/manual/ch04.html#idp55231856
    Here's what I did...
9.  
	
9.  In order to fully update your guest system, open a terminal and run
    zypper update
    as root.  (Note:  This takes a very long time!)
10. Install the make tool and the GNU C compiler using
    zypper install make gcc
11. Reboot your guest system in order to activate the updates.
12. Find out which kernel you are running using
    uname -a
    An example would be 2.6.31.12-0.2-default which refers to the "default" kernel. 
13. Then install the correct kernel development package. In the above example this would be
    zypper install kernel-default-devel
14. Make sure that your running kernel (uname -a) and the kernel packages you have installed (rpm -qa kernel\*) have the exact same version number. 
15. Proceed with the installation as described below:
    In your home drive /home/dan make a directory path /mount/cdrom
    mount the "Guest Additions" with the following command:
    sudo mount -t iso9660 /dev/cdrom /home/dan/mount/cdrom
Change to the directory where your CD-ROM drive is mounted and execute as root:

sh ./VBoxLinuxAdditions.run



    gcc and kernel-devel was not included with the generic desktop install so,
    To install Guest Additions, you need to follow: https://ask.fedoraproject.org/en/question/24349/how-do-i-run$
    https://www.centos.org/forums/viewtopic.php?t=5603
9.  su -c "yum install -y gcc kernel-devel"
10. Put in root password.
11. su - c "yum update"
12. NOTE:  To get out of the screen saver, hit ESC
13. restart
14. Run the Virtual Box Desktop Additions
15. Now add the user to the VirtualBox Group vboxsf:
    su -c "usermod -a -G vboxsf dan"
16. Restart
17. Once all this was done, I was able to save a file from Fedora into the Mac host
2. 

## CentOS
1. Download CentOS-6.5-x86_64-bin-DVD1.iso
2.  Set up Virtual Desktop for 2 GB RAM and 20 GB HD space
3.  Set "Shared Clipboard" to bidirectional
4.  Set "Drag-n-Drop" to bidirectional
5.  Add shared folder /Users/dlviar as auto-mount
6.  Start the VM
7.  Point the VM to the iso
8.  Intall CentOS
    I didn't have internet when I started
    following  http://superuser.com/questions/611179/installed-minimal-centos-6-4-in-virtualbox-but-have-no-internet
    Tried:
    Looks like your eth0 is not set up.

    su -c "cat /etc/sysconfig/network |grep -i network"

    This should return: NETWORKING=yes - If it does not, then change it to yes.
    
    vi /etc/sysconfig/network-scripts/ifcfg-eth0
    Now the whole file should look like this:

```
DEVICE="eth0"
HWADDR="08:00:27:07:9e:57"
NM_CONTROLLED="NO"
ONBOOT="YES"
BOOTPROTO="dhcp"
```
    Save and close. Now, lets restart the network service to reload with these settings:

    su -c "service network restart"

9.  In order to fully update your guest system, open a terminal and run
    su -c "yum update"
yum update
as root.
Install the GNU C compiler and the kernel development packages using

yum install gcc

su -c "install gcc"

followed by

yum install kernel-devel
For Oracle UEK kernels, use

yum install kernel-uek-devel
to install the UEK kernel headers.

Reboot your guest system in order to activate the updates and then proceed as described above.

Proceed with the installation as described below:
    In your home drive /home/dan make a directory path /mount/cdrom
    mount the "Guest Additions" with the following command:
    sudo mount -t iso9660 /dev/cdrom /home/dan/mount/cdrom
Change to the directory where your CD-ROM drive is mounted and execute as root:

sh ./VBoxLinuxAdditions.run

Now add the user to the VirtualBox Group vboxsf:
    su -c "usermod -a -G vboxsf dan"
16. Restart
17. Once all this was done, I was able to save a file from Fedora into the Mac host


In case Oracle Linux does not find the required packages, you either have to install them from a different source (e.g. DVD) or use Oracle's public Yum server located at http://public-yum.oracle.com.

 






> Written with [StackEdit](https://stackedit.io/).