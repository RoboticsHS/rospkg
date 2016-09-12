# ROS package system information

[![Build Status](https://travis-ci.org/RoboticsHS/rospkg.svg?branch=master)](https://travis-ci.org/RoboticsHS/rospkg)
[![Build status](https://ci.appveyor.com/api/projects/status/tomd0oe3oikya0xb?svg=true)](https://ci.appveyor.com/project/akru/rospkg)

## Install

    $ git clone https://github.com/RoboticsHS/rospkg
    $ cd rospkg
    $ stack build

## Fun

### List packages

    $ time ./.stack-work/install/x86_64-linux/lts-6.14/7.10.3/bin/rospkg list > /dev/null
    0,09s user 0,04s system 195% cpu 0,070 total

    $ time rospack list > /dev/null
    0,30s user 0,28s system 99% cpu 0,585 total

### Find package

    $ time ./.stack-work/install/x86_64-linux/lts-6.14/7.10.3/bin/rospkg find geometry_msgs > /dev/null
    0,01s user 0,00s system 84% cpu 0,011 total

    $ time rospack find geometry_msgs > /dev/null 
    0,02s user 0,01s system 95% cpu 0,025 total
