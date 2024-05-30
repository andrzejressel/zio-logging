package zio.logging.internal

import zio.Trace
import zio.internal.stacktracer.Tracer

import java.lang.StackWalker.StackFrame
import scala.jdk.CollectionConverters._

object StackFrameFinder {

  def findUserTrace(loggerImplPackage: String): Option[Trace] = {
    StackWalker.getInstance()
      .walk(s => findUserStackFrame(loggerImplPackage, s.iterator().asScala))
      .map(stackFrameToTracer)
  }

  private def stackFrameToTracer(stackFrame: StackFrame): Trace = {
    Trace(
      stackFrame.getClassName + "." + stackFrame.getMethodName,
      stackFrame.getFileName,
      stackFrame.getLineNumber
    )
  }

  // Stacktrace should look like this

  // ZIO framework
  // User code
  // Logger code
  // ZIO logging code

  // First we wait for logger code to appear and then we get first frame that does not belong there
  private def findUserStackFrame(loggerImplPackage: String, stackFrames: Iterator[StackFrame]): Option[StackFrame] = {
    var inLoggerCode = false

    for (stackFrame <- stackFrames) {

      if (stackFrame.getClassName.startsWith(loggerImplPackage) && !inLoggerCode) {
          inLoggerCode = true
      }
      if (!stackFrame.getClassName.startsWith(loggerImplPackage) && inLoggerCode) {
        return Some(stackFrame)
      }

    }

    None
  }

}
