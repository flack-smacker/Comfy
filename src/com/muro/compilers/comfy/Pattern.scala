/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy

object TokenPattern {
  val identifier = "[a-z]"
  val keyword = "(if)|(print)|(while)|(int)|(string)|(boolean)|(true)|(false)"
  val operator = "(==)|(!=)|\\+"
  val digit = "[0-9]"
}
