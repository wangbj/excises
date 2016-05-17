* Convert large binary files

spec: https://www.topcoder.com/challenge-details/30053925/?type=develop

output3 need serious optimization.

Practiced this program for ieteraee I/O. guess conduit isn't very idiomatic, and has large dependencies such as ``mono-traversable``,``resourcet``, etc. Some of them have comple types, such as ``ResourceT``, ``MonadResource``. Lots of API to remember. Next time will try ``Pipes`` instead.
