import os, oids, tables

when defined(windows):
  proc dup(fd: cint): cint {.importc: "_dup", header: "<io.h>".}
  proc dup2(fd1, fd2: cint): cint {.importc: "_dup2", header: "<io.h>".}
else:
  import posix

proc getTempFile(): string =
  return getTempDir() / $genOid() & ".tmp"

template captureOutput*(code: untyped): (string, ref Exception) =
  var backupFds = newTable[File, cint]()

  let
    streams = [stdout, stderr]
    fname = getTempFile()
    tempFile = open(fname, fmReadWrite)
    tempHandle = tempFile.getFileHandle()

  for stream in streams:
    let handle = stream.getFileHandle()

    let save = dup(handle)
    backupFds[stream] = save

    discard dup2(tempHandle, handle)

  var exc: ref Exception
  var output = ""

  try:
    code

    tempFile.setFilePos(0)
    output = tempFile.readAll()
    tempFile.close()

    (output, exc)
  except Exception as e:
    exc = e

    tempFile.setFilePos(0)
    output = tempFile.readAll()
    tempFile.close()

    (output, exc)
  finally:
    for stream in streams:
      let handle = stream.getFileHandle()
      let save = backupFds[stream]
      discard dup2(save, handle)

    try:
      os.removeFile(fname)
    except:
      discard

