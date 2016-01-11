/*
 * This file is part of Parallel Dynamic Programming Automation Plugin Prototype (PDPAPP).
 *
 * PDPAPP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * PDPAPP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with PDPAPP.  If not, see <http://www.gnu.org/licenses/>.
 */
package de.unikl.reitzig.paralleldynprog.automation

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.ast.TreeDSL

/**
 * Transforms suitable recursive methods annotated by `@DynamicProgramming` into
 * efficient, parallel code. There are some restrictions to its recognition power;
 * see the examples in the thusly named subpackage for some examples.
 * @author Raphael Reitzig, 2012
 */
class DynamicProgrammingPlugin(val global: Global) extends Plugin {
  val name = "dynprog"
  val description = "Translates annotated dynamic programming recursions into efficient code"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent with Transform with TreeDSL {
    import global._
    import definitions._

    val global: DynamicProgrammingPlugin.this.global.type = DynamicProgrammingPlugin.this.global
    val runsAfter = List[String]("parser")
    override val runsRightAfter = Some("parser")
    val phaseName = DynamicProgrammingPlugin.this.name
    def newTransformer(unit: CompilationUnit) = new DPTransformer(unit)

    /**
     * Tree transformer that finds methods annotated for optimisation and performs the rewriting.
     */
    class DPTransformer(unit : CompilationUnit) extends Transformer {

      override def transform(tree : Tree) : Tree = {
        // Do whatever the super class does
        val tree1 = super.transform(tree)

        tree1 match {
          // Change only methods annotated with @DynamicProgramming
          case DefDef(mods @ Modifiers(_, _, List(Apply(annot @ Select(New(Ident(anname)), _), anpar)) ,_),
                      name, tparams, vparams, tpt, rhs) => {
              if ( anname.toString() == "DynamicProgramming" ) {
                // Check wether parameters conform to our specification
                if ( vparams.size != 2 || vparams(0).size == 0 || vparams(1).size == 0 ) {
                  unit.warning(tree.pos, "Dynamic programming rewriting expects two non-empty parameter lists.")
                  tree1
                }
                else if ( vparams(1).size != 2 ) {
                  unit.warning(vparams(1)(0).pos, "Dynamic programming rewriting expects two parameters in the second parameter list.")
                  tree1
                }
                else if ( vparams(1)(0).tpt.toString != "Int" ) {
                  unit.warning(vparams(1)(0).pos, "Dynamic programming rewriting expects parameters of type Int in the second parameter list.")
                  tree1
                }
                else if ( vparams(1)(1).tpt.toString != "Int" ) {
                  unit.warning(vparams(1)(1).pos, "Dynamic programming rewriting expects parameters of type Int in the second parameter list.")
                  tree1
                }
                else if ( vparams(1)(0).rhs == EmptyTree ) {
                  unit.warning(vparams(1)(0).pos, "Dynamic programming rewriting requires index parameters to have default values.")
                  tree1
                }
                else if ( vparams(1)(1).rhs == EmptyTree ) {
                  unit.warning(vparams(1)(1).pos, "Dynamic programming rewriting requires index parameters to have default values.")
                  tree1
                }
                else {
                  // Select implementation based on annotations (and heuristics)
                  val impl = (anpar match {
                    case List(Ident(cname)) => Case(cname.toString()) match {
                      case Detect => detectCase(rhs, name.toString(), (vparams(1) map { _.name.toString()}))
                      case c => c
                    }
                    case _ => detectCase(rhs, name.toString(), (vparams(1) map { _.name.toString()}))
                  }) match {
                    case Case1  => Some("BlockCheck")
                    case Case2  => Some("RowSplit")
                    case Case3  => Some("RowFill")
                    case Detect => {
                      unit.warning(tree.pos, "Could not detect applicable case for dynamic programming rewriting.")
                      None
                    }
                  }

                  // Rewrite the method
                  impl match {
                    case Some(i) => {
                      parameterCheck(unit, rhs, name.toString(), vparams(0) map { _.name.toString })
                      rewrite(tree1, i)
                    }
                    case None => tree1
                  }
                }
              }
              else {
                tree1
              }
          }
          case _ => tree1
        }
      }

      /**
       * Rewrites the annotated method
       */
      def rewrite(original : Tree, solver : String) : Tree = original match { case DefDef(mods, name, tparams, vparams, tpt, rhs) => {
        // TODO use Option[T] only in Case1; the others can use T directly

        // 1. Create Array declaration
        val optionT = AppliedTypeTree(Ident(newTypeName("Option")), List(tpt))
        val arrayFill = Select(Ident(newTermName("Array")), newTermName("fill"))
        val arrayDef = ValDef(Modifiers(0), newTermName("array"), TypeTree(),
                              Apply(
                                Apply(
                                  TypeApply(arrayFill, List(optionT)),
                                  List(Apply(Select(vparams(1)(0).rhs, newTermName("$plus")), List(Literal(1))),
                                       Apply(Select(vparams(1)(1).rhs, newTermName("$plus")), List(Literal(1)))
                                      )
                                ),
                                List(Ident(newTermName("None")))
                              )
                             )

        // 3. Create cell function declaration
        // 3.1 Remove markers
        val withoutMarkers = MarkerRemover.transform(rhs)
        // 3.2 Rewrite recursive calls
        val rewrittenRecursion = new RecursionRewriter(name.toString(), Ident(newTermName("array"))).transform(withoutMarkers)
        // 3.3 Declare as function object
        val i = ValDef(vparams(1)(0).mods, vparams(1)(0).name, vparams(1)(0).tpt, EmptyTree)
        val j = ValDef(vparams(1)(1).mods, vparams(1)(1).name, vparams(1)(1).tpt, EmptyTree)
        val fcttype = AppliedTypeTree(Select(Ident(newTermName("scala")), newTypeName("Function2")),
                                      List(Ident(newTypeName("Int")), Ident(newTypeName("Int")), tpt))
        val cellfctDef = ValDef(Modifiers(0), newTermName("f"), fcttype, Function(List(i,j), rewrittenRecursion))

        // 4. Create call to implementation
        val implCall = Apply(TypeApply(Select(Select(Select(Select(Select(Ident(newTermName("de")),
                                      newTermName("unikl")),
                                        newTermName("reitzig")),
                                          newTermName("paralleldynprog")),
                                            newTermName("runtime")),
                                    newTermName(solver)), List(tpt)),
                             List(Ident("array"), Ident("f")))

        // 5. Create result retrieval/return
        val resultAccess = Select(Apply(Apply(Ident(newTermName("array")),
                                              List(vparams(1)(0).rhs)),
                                        List(vparams(1)(1).rhs)),
                                  newTermName("get"))

        // Assemble
        val newRhs = Block(List(arrayDef, cellfctDef, implCall), resultAccess)
        val result = DefDef(mods, name, tparams, List(vparams.head, List()), tpt, newRhs)

        //println("new method: " + result)
        unit.comment(original.pos, "test")
        println("Dynamic Programming method " + name + " rewritten to use " + solver + ".")
        result
      }}

      /**
       * Detects which case applies to the method with the given right hand side and name
       */
      def detectCase(methodBlock : Tree, methodName : String, params : List[String]) : Case = {
        val areas = { val ac = new AreaCollector(unit, methodName,  params); ac.transform(methodBlock); ac.areas }

        if ( areas subsetOf Set(UL, U, UR) ) {
          Case2 // Give Case 2 precedence as it performs better
        }
        else if ( areas subsetOf Set(L, UL, U) ) {
          Case1
        }
        else if ( areas subsetOf Set(L, UL, U, UR) ) {
          Case3
        }
        else {
          Detect
        }
      }
    }

    /*
     * From here on down, we have helpers that implement partial functionality of above plugin phase
     */

    /**
     * Checks wether all recursive calls have the same "real" parameters as the main call;
     * otherwise mayhem might ensue. Throws a warning for all violations.
     * @param unit Reference to the compilation unit (for warnings)
     * @param code The rewritten method's right hand side
     * @param params The names of the index parameters (in order)
     */
    def parameterCheck(unit : CompilationUnit, code : Tree, method_name : String, params : List[String]) {
      for ( recursiveCall @ Apply(Apply(Ident(name), args1), args2) <- code;
             if method_name == name.toString() ) {
        if ( (args1 map { _.toString }) != params ) {
          unit.warning(recursiveCall.pos, "Dynamic programming recursive call has different parameters than main call.")
        }
      }
    }

    /**
     * Collects all areas recursive calls in the code fragment passed to `transform` hit.
     * @param unit Reference to the compilation unit (for warnings)
     * @param method_name The name of the rewritten method
     * @param params The names of the index parameters (in order)
     */
    class AreaCollector(val unit : CompilationUnit, val method_name : String, val params : List[String]) extends Transformer {
      private val builder = Set.newBuilder[Area]
      /**
       * @return all areas found. Contains `Other` if recursive calls could not be analysed.
       */
      def areas = builder.result()

      override def transform(code : Tree) : Tree = {
        code match {
          case Block(statements, call @ Apply(Apply(Ident(name), args1), args2)) => {
            if ( method_name == name.toString() ) {
              val labels = statements.map { c => c match {
                case i @ Ident(n) => if ( Seq("L", "UL", "U", "UR") contains n.toString() ) { Some(Area(n.toString)) }
                                     else { None }
                case _ => None
              }}.flatten

              if ( labels.size > 0 ) {
                builder ++= labels
              }
              else {
                val areas = detectAreas(call.pos, args2)
                if ( areas contains Other ) {
                  unit.warning(call.pos, "Could not determine dependency areas of this recursive call.")
                }
                builder ++= areas
              }
            }
            else {
              super.transform(code)
            }
          }
          case call @ Apply(Apply(Ident(name), args1), args2) => {
            if ( method_name == name.toString() ) {
              val areas = detectAreas(call.pos, args2)
              if ( areas contains Other ) {
                  unit.warning(call.pos, "Could not determine dependency areas of this recursive call.")
                }
              builder ++= areas
            }
          }
          case t @ _ => super.transform(t)
        }

        code
      }

      /**
       * Given method call parameters, tried to decide what areas those can touch.
       * If it can not decide, the returned set will contain Other.
       * @param call The position of the considered recursive call (for warnings)
       * @param concparams The terms provided as parameters to a recursive call
       * @return The set of areas the provided terms can hit (from L,UL,U,UR). If
       *         the heuristic can not decide, it returns {Other}.
       */
      def detectAreas(call : Position, concparams : List[Tree]) : Set[Area] = {
        // Detect manipulations of parameters of the form `i - c` and `i + c` with `c` integer literals
        concparams match {
          case List(Ident(n1), Ident(n2)) => {
            if ( n1.toString == params(0) && n2.toString == params(1) ) {
              unit.warning(call, "Infinite recursion detected!")
            }
            Set(Other)
          }
          case List(Ident(n1), Apply(Select(n2, op), List(c))) => {
            if ( n1.toString() == params(0) && n2.toString() == params(1) && (c.toString matches "-?\\d+") &&
                 (    (op.toString() == "$minus"  && c.toString.toInt > 0)
                   || (op.toString() == "$plus"   && c.toString.toInt < 0) ) ) {
              Set(L)
            }
            else {
              Set(Other)
            }
          }
          case List(Apply(Select(n1, op), List(c)), Ident(n2) ) => {
            if ( n1.toString == params(0) && n2.toString == params(1) && (c.toString matches "-?\\d+") &&
                 (    (op.toString == "$minus"  && c.toString.toInt > 0)
                   || (op.toString == "$plus"   && c.toString.toInt < 0) ) ) {
              Set(U)
            }
            else {
              Set(Other)
            }
          }
          case  List(Apply(Select(n1, op1), List(c1)), Apply(Select(n2, op2), List(c2))) => {
            if ( n1.toString == params(0) && n2.toString == params(1) &&
                 (c1.toString matches "\\d+") &&  (c2.toString matches "-?\\d+") &&
                 (    (op1.toString() == "$minus"  && c1.toString.toInt > 0)
                   || (op1.toString() == "$plus"   && c1.toString.toInt < 0) ) ) {
              // All valid and in above half; decide which one of UL or UR it is!
              if (    (op2.toString == "$minus"  && c2.toString.toInt > 0)
                   || (op2.toString == "$plus"   && c2.toString.toInt < 0) ) {
                Set(UL)
              }
              else if (    (op2.toString == "$minus"  && c2.toString.toInt < 0)
                        || (op2.toString == "$plus"   && c2.toString.toInt > 0) ) {
                Set(UR)
              }
              else {
                Set(Other)
              }
            }
            else {
              Set(Other)
            }
          }
          case _ => Set(Other)
        }
      }
    }

    /**
     * Removes all recursive call markers used for case detection.
     */
    object MarkerRemover extends Transformer {
      override def transform(tree : Tree) : Tree = {
        // Do whatever the super class does
        val tree1 = super.transform(tree)

        tree1 match {
          case Block(statements, result) => {
            val nonLabel = statements.map { c => c match {
              case i @ Ident(n) => if ( Seq("L", "UL", "U", "UR") contains n.toString() ) { None } else { Some(i) }
              case _ => Some(c)
            }}.flatten

            if ( nonLabel.size == 0 ) {
              result
            }
            else {
              treeCopy.Block(tree1, nonLabel, result)
            }
          }
          case _ => tree1
        }
      }
    }

    /**
     * Replaces all calls to the specified method with accesses to the specified array,
     * dropping the method's first parameter list.
     * @param method_name The name of the rewritten method
     * @param array The tree representing the created array's identifier
     */
    class RecursionRewriter(val method_name : String, val array : Tree) extends Transformer {
      override def transform(tree : Tree) : Tree = {
        // Do whatever the super class does
        val tree1 = super.transform(tree)

        tree1 match {
          case Apply(Apply(Ident(name), args1), args2) => {
            if ( method_name == name.toString() ) {
              Select(Apply(Apply(array, args2 take 1), args2 drop 1), newTermName("get"))
            }
            else {
              tree1
            }
          }
          case _ => tree1
        }
      }
    }
  }
}
