#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/fcntl.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <netinet/ip6.h>
#include <linux/if.h>
#include <linux/if_bridge.h>
#include <errno.h>
#include <err.h>
#include <stdlib.h>

extern unsigned int if_nametoindex (const char *__ifname);
extern char *if_indextoname (unsigned int __ifindex, char *__ifname);

int br_socket_fd = -1;

static int
br_init(void)
{
    if ((br_socket_fd = socket(AF_LOCAL, SOCK_STREAM, 0)) < 0)
        return errno;
    return 0;
}

static int
br_add_interface(const char *bridge, const char *dev)
{
    struct ifreq ifr;
    int err;
    int ifindex = if_nametoindex(dev);

    if (ifindex == 0) 
        return ENODEV;
        
    strncpy(ifr.ifr_name, bridge, IFNAMSIZ);
#ifdef SIOCBRADDIF
    ifr.ifr_ifindex = ifindex;
    err = ioctl(br_socket_fd, SIOCBRADDIF, &ifr);
    if (err < 0)
#endif
        {
            unsigned long args[4] = { BRCTL_ADD_IF, ifindex, 0, 0 };
                                          
            ifr.ifr_data = (char *) args;
            err = ioctl(br_socket_fd, SIOCDEVPRIVATE, &ifr);
        }

    return err < 0 ? errno : 0;
}

static int chflags(char *dev, __u32 flags, __u32 mask)
{
    struct ifreq ifr;
    int fd;
    int err;

    strncpy(ifr.ifr_name, dev, IFNAMSIZ);
    fd = br_socket_fd;
    if (fd < 0)
        return -1;
    err = ioctl(fd, SIOCGIFFLAGS, &ifr);
    if (err) {
        perror("SIOCGIFFLAGS");
        close(fd);
        return -1;
    }
    if ((ifr.ifr_flags^flags)&mask) {
        ifr.ifr_flags &= ~mask;
        ifr.ifr_flags |= mask&flags;
        err = ioctl(fd, SIOCSIFFLAGS, &ifr);
        if (err)
            perror("SIOCSIFFLAGS");
    }
    close(fd);
    return err;

}

int
main(int argc, char **argv)
{
    char *hotplug_vif = getenv("vif");

    if (!hotplug_vif)
      errx(1, "vif not defined");
 
    if(br_socket_fd == -1) {
        br_init();
    }
    br_add_interface("br0", hotplug_vif);
    __u32 mask=0;
    __u32 flags=0;
    mask |= IFF_UP;
    flags |= IFF_UP;
    chflags(hotplug_vif, flags, mask);
    return 0;
}
