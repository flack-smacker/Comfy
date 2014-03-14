/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package com.muro.compilers.comfy

import Tag._

class Token(t: Tag, a: String) {
    
    /**
     * Each token contains a tag. The tag is an enumeration type describing the
     * token's type (e.g.: assignOp, boolOp, or plusOp).
     */
    var tag: Tag = t
    
    /**
     * Each token can also store an optional attribute. An example would be
     * a Token representing a boolean operation. The tag value would be 
     * "T_boolOp" and the attribute would be either "!=" or "==".
     */
    var attr: String = a
}
