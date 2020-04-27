package com.peschke

import javax.swing.JComponent

package object fractals {
  implicit class ToolTipHelper[C <: JComponent](val c: C) extends AnyVal {
    def setMultiLineToolTip(tip: String): Unit = {
      if (!tip.contains("\n")) c.setToolTipText(tip)
      else c.setToolTipText {
        s"<html>$tip</html>".replaceAllLiterally("\n", "<br/>")
      }
    }
  }
}
