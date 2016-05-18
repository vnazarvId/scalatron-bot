package com.grandivory.scalatron.bot.util

case class Matrix(width:Int, height:Int) {
  private val internal: Array[Double] = new Array[Double](width * height)

  def set(x:Int, y:Int, v:Double) = { internal(x*width+y) = v }

  def addValue(x:Int, y:Int, v:Double) = {
    set(x,y,v)
  }
}