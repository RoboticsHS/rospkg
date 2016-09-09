-- |
-- Module      :  Robotics.ROS.Pkg
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- The Robot Operating System (ROS) is a set of software libraries and tools
-- that help you build robot applications. From drivers to state-of-the-art
-- algorithms, and with powerful developer tools, ROS has what you need for
-- your next robotics project. And it's all open source.
--
-- Packages are the main unit for organizing software in ROS. A package may
-- contain ROS runtime processes (nodes), a ROS-dependent library, datasets,
-- configuration files, or anything else that is usefully organized together.
-- Packages are the most atomic build item and release item in ROS. Meaning
-- that the most granular thing you can build and release is a package.
--
-- This package provide utility and library for fast search ROS packages,
-- collect information from @package.xml@ files and some additional power,
-- e.g. message files finder.
--
module Robotics.ROS.Pkg (
  -- * Package search
    module Robotics.ROS.Pkg.Finder
  -- * Messages search
  , module Robotics.ROS.Pkg.Msgs
  -- * Package info types 
  , PackageMeta(..)
  , Package(..)
  , PkgName
  ) where

import Robotics.ROS.Pkg.Finder
import Robotics.ROS.Pkg.Types
import Robotics.ROS.Pkg.Msgs
