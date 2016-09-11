# ROS message generator binaries 

[![Build Status](https://travis-ci.org/RoboticsHS/rosmsg-bin.svg?branch=master)](https://travis-ci.org/RoboticsHS/rosmsg-bin)

> This use [rosmsg](https://github.com/RoboticsHS/rosmsg) and [rospkg](https://github.com/RoboticsHS/rospkg) libraries for browsing and generate Haskell native package from ROS message package.

## Install

    $ git clone https://github.com/RoboticsHS/rosmsg-bin
    $ cd rosmsg-bin
    $ stack build

## Fun

### Single package without deps

    $ ./.stack-work/install/x86_64-linux/lts-6.16/7.10.3/bin/genhs std_msgs
    $ cd std-msgs 
    $ stack build

    $ stack ghci
    > import Data.Default
    > let m = def :: Int64MultiArray 
    > m
    Int64MultiArray {_layout = MultiArrayLayout {_dim = ROSArray {unArray = []}, _data_offset = 0}, __data = ROSArray {unArray = []}}
    > getType m
    "std_msgs/Int64MultiArray"
    > getSource m
    "MultiArrayLayout layout\nint64[] data"
    > getDigest m
    54865aa6c65be0448113a2afc6a49270

### Package with deps

    $ ./.stack-work/install/x86_64-linux/lts-6.16/7.10.3/bin/rospkg deps sensor_msgs
    Build depend:
    ["geometry_msgs","message_generation","std_msgs"]
    Run depend:
    ["geometry_msgs","message_runtime","std_msgs"

    $ ./.stack-work/install/x86_64-linux/lts-6.16/7.10.3/bin/genhs std_msgs
    $ ./.stack-work/install/x86_64-linux/lts-6.16/7.10.3/bin/genhs geometry_msgs 
    $ ./.stack-work/install/x86_64-linux/lts-6.16/7.10.3/bin/genhs sensor_msgs
    $ cd sensor_msgs 
    $ stack build

    $ stack ghci
    > let m = def :: CameraInfo
    CameraInfo {_header = Header {_seq = 0, _stamp = (0,0), _frame_id = ""}, _height = 0, _width = 0, _distortion_model = "", _d = ROSArray {unArray = []}, _k = ROSFixedArray {unFixedArray = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]}, _r = ROSFixedArray {unFixedArray = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]}, _p = ROSFixedArray {unFixedArray = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]}, _binning_x = 0, _binning_y = 0, _roi = RegionOfInterest {_x_offset = 0, _y_offset = 0, _height = 0, _width = 0, _do_rectify = False}}
    > getDigest m
    c9a58c1b0b154e0e6da7578cb991d214

### Play with Lens

    > import Lens.Family2

    > import Robotics.ROS.Msg.Geometry_msgs.Vector3 as V
    > let v = def :: Vector3
    > view V.x v
    0.0
    > over V.x (+ 1) v
    Vector3 {_x = 1.0, _y = 0.0, _z = 0.0}

    > import Robotics.ROS.Msg.Geometry_msgs.PoseWithCovarianceStamped as PCS
    > import Robotics.ROS.Msg.Geometry_msgs.PoseWithCovariance as PC
    > import Robotics.ROS.Msg.Geometry_msgs.Pose as Pose

    > let p = def :: PoseWithCovarianceStamped
    > view (PCS.pose . PC.covariance ) p
    ROSFixedArray {unFixedArray = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]}

    > PCS.pose . PC.pose . Pose.orientation . w .~ 1 $ p
    PoseWithCovarianceStamped {_header = Header {_seq = 0, _stamp = (0,0), _frame_id = ""}, _pose = PoseWithCovariance {_pose = Pose {_position = Point {_x = 0.0, _y = 0.0, _z = 0.0}, _orientation = Quaternion {_x = 0.0, _y = 0.0, _z = 0.0, _w = 1.0}}, _covariance = ROSFixedArray {unFixedArray = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]}}}

    > PCS.header . frame_id  .~ "base_link" $ p
    PoseWithCovarianceStamped {_header = Header {_seq = 0, _stamp = (0,0), _frame_id = "base_link"}, _pose = PoseWithCovariance {_pose = Pose {_position = Point {_x = 0.0, _y = 0.0, _z = 0.0}, _orientation = Quaternion {_x = 0.0, _y = 0.0, _z = 0.0, _w = 0.0}}, _covariance = ROSFixedArray {unFixedArray = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]}}}
