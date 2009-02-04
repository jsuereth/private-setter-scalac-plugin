package org.scala_lang.privateSetter.internal

import scala.tools.nsc
import nsc._
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class privateSetterPlugin(val global : Global) extends Plugin {

  val name = "privateSetter"
  val description = "allows vars to have private setters"
  val components = List[PluginComponent](VarAccessChanger)

  
  /** Plugin component to complete dependency analysis after a build and write out dependnecies for next build */
  private object VarAccessChanger extends PluginComponent {
    val global = privateSetterPlugin.this.global
    //TODO - Figure out how to run VERY early
    val runsAfter = "parser"
    val phaseName = "make-setters-private-if-needed"
    def newPhase(prev: Phase) = new MakeSettersPrivatePhase(prev)
    def name = phaseName
        
    /** The actual phase the removes units from being compiled that are up-to-date */
    class MakeSettersPrivatePhase(prev: Phase) extends Phase(prev) {
      override def name = VarAccessChanger.this.name
        import global._
      override def run {
      
	       for {unit <- global.currentRun.units;
	           node @ DefDef(mods,name,tparams,vparams,tpt,_) <- unit.body
	            if name.toString.endsWith("_$eq") //TODO - only var like setters? for now this is fine...
	            annotation <- mods.annotations
	            if annotation.tpe.safeToString == classOf[privateSetter].getName
	            if !mods.isPrivate
	       } {
		     //node.setType
	         //TODO - Make the method private...
             
      
	      }

      }
      /** Selector to remove annotated var setters */
      object AnnotatedSetter {
        def unapply(node : Tree) : Option[DefDef] = {
          def hasPrivateSetterAnnotation(annotations : List[Annotation]) : Boolean = {
            for {
                annotation <- annotations
	            if annotation.tpe.safeToString == classOf[privateSetter].getName
            } {
              return true
            }
            false
          }
          
          node match {
            case x @ DefDef(mods,name,tparams,vparams,tpt,_)  =>
              if(name.toString.endsWith("_$eq") && hasPrivateSetterAnnotation(mods.annotations)){
                Some(x)
              } else {
                None
              }
            case _ => None
          }
        }
      }
      
      def transformTree(top : Tree) = {
        top match {
          
          case x => x
        }
      }
      
    }
  }
}
