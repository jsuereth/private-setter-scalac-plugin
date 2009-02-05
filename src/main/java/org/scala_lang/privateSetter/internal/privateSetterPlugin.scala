package org.scala_lang.privateSetter.internal

import scala.tools.nsc
import nsc._
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
/**
 * Our privatesetter plugin class that contributes a new phase to the compiler.
 */
class privateSetterPlugin(val global : Global) extends Plugin {

  val name = "privateSetter"
  val description = "allows vars to have private setters"
  val components = List[PluginComponent](VarAccessChanger)

  
  /** Plugin component to complete dependency analysis after a build and write out dependnecies for next build */
  private object VarAccessChanger extends PluginComponent {
    val global = privateSetterPlugin.this.global
    val runsAfter = "typer"
    val phaseName = "private-setters"
    def newPhase(prev: Phase) = new MakeSettersPrivatePhase(prev)
    def name = phaseName
        
    /** The actual phase the removes units from being compiled that are up-to-date */
    class MakeSettersPrivatePhase(prev: Phase) extends Phase(prev) {
      
      override def name = VarAccessChanger.this.name
      import global._
      /**
       * Called when our plugin is running.
       */
      override def run {
	       for (unit <- global.currentRun.units; if !unit.isJava) {
             unit.body = TreeTransformer.transform(unit.body)      
	      }

      }
      /** 
       * Selector to remove annotated var setters 
       */
      object AnnotatedSetterShouldBePrivate {
        /** 
         * Pattern matcher for privateSetter annotated setter methods 
         */
        def unapply(node : Tree) : Option[DefDef] = {
          /** 
           * Helper method to determine if a public setter method has the privateSetter annotation. 
           */
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
            case x @ DefDef(mods,name,_,_,_,_) if name.toString.endsWith("_$eq") =>
              if(hasPrivateSetterAnnotation(mods.annotations)){
                Some(x)
              } else {
                None
              }
            case _ => None
          }
        }
      }
      
      /** 
       * This object transforms the AST of a compilation unit apply our PRIVATE morphosis for vars.
       */
      object TreeTransformer extends Transformer {
        import symtab.Flags._
        /**
         * This method transforms individual nodes of the tree.
         */
        override def transform(tree: Tree) = tree match {
	        case AnnotatedSetterShouldBePrivate(d @ DefDef(mods,name,tparams,vparams,tpt,impl)) => 
	          //TODO - do we need to call transform lower than this? perhaps
	          //mods.isPublic
              Console.println("Making: " + d + " private!")
              val tree = copy.DefDef(d, mods | PRIVATE,name,tparams,vparams,tpt,transform(impl))
              tree.symbol.setFlag(PRIVATE)              
              tree
	        case t => 	       
	          super.transform(t)
	      }
      }
      
    }
  }
}
