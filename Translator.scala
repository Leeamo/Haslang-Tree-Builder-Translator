/**
 * HasLang to SEC translator.
 *
 * Copyright 2021, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package haslang

/**
 * Translator from HasLang source programs to SEC target programs.
 */
object Translator {

    import SECTree._
    import HasLangTree._
    import scala.collection.mutable.ListBuffer

    /**
     * Return a frame that represents the SEC instructions for a FunLang program.
     */
    def translate (program : Program) : Frame = {

        // An instruction buffer for accumulating the program instructions
        val programInstrBuffer = new ListBuffer[Instr] ()

        /**
         * Translate the program by translating its expression.
         */
        val expInstrs = translateExpression (program.exp)
        programInstrBuffer.appendAll (expInstrs)
        programInstrBuffer.append (IPrint ())

        // Gather the program's instructions and return them
        programInstrBuffer.result ()

    }

    /**
     * Translate an expression and return a list of the instructions that
     * form the translation.
     */
    def translateExpression (exp : Exp) : Frame = {

        // An instruction buffer for accumulating the expression instructions
        val expInstrBuffer = new ListBuffer[Instr] ()

        /**
         * Generate an instruction by appending it to the instruction buffer.
         */
        def gen (instr : Instr) {
            expInstrBuffer.append (instr)
        }

        /**
         * Generate a sequence of instructions by appending them all to the
         * instruction buffer.
         */
        def genall (frame : Frame) {
            expInstrBuffer.appendAll (frame)
        }

        /**
         * Generate code to make a closure (argName => body).
         */
        def genMkClosure (argName : String, body : Exp) {
            val bodyInstrs = translateExpression (body)
            gen (IClosure (argName, bodyInstrs :+ IPopEnv ()))
        }

        /**
          * helper function to translate listExp 
          * added by Liam H
          *
          * @param exps
          * @return
          */
        def translateListVector (exps : Vector[haslang.HasLangTree.Exp]) { 

            exps match {
                case (Vector()) => {
                    gen(INil())
                }

                case (h +: t) => {
                    genall(translateExpression(h))
                    translateListVector(t)
                    gen(ICons())
                }

            }
        }

        
        

        exp match {

        case IdnUse (value) =>
            gen (IVar (value))

        case IntExp (value) =>
            gen (IInt (value))

        case BoolExp (value) =>
            gen (IBool (value))

        case PlusExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (IAdd ())

        case MinusExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (ISub ())

        case SlashExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (IDiv ())

        case StarExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (IMul ())

        case LessExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (ILess ())

        case EqualExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (IEqual ())

        case LamExp (IdnDef (name, _), body) =>
            genMkClosure(name, body)

        case ConsExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (ICons ())


        case IfExp (cond, thenExp, elseExp) =>
            genall (translateExpression(cond))
            gen (IBranch(translateExpression(thenExp),translateExpression(elseExp)))
        
        case ListExp (exps) => translateListVector(exps)

        case LetExp (defns, exp) => {


            defns.head match {
                //single line function
                case Defn(idn, Vector(FunLine(_,Vector(),simpleExp))) => 
                    genall(translateExpression(AppExp(LamExp(idn,exp),simpleExp)))
                //fun with simple pattern
                

            }


           
            
            
        }

        

        case AppExp (a, b) => 
            (a,b) match { 
                case (IdnUse("head"), b) => {
                    genall(translateExpression(b))
                    gen(IHead())
                }
                case (IdnUse("tail"), b) => {
                    genall(translateExpression(b))
                    gen(ITail())
                }
                case (IdnUse("length"), b) => {
                    genall(translateExpression(b))
                    gen(ILength())
                }
                //funcname, exps
                case _ => { 
                    genall(translateExpression(a))
                    genall(translateExpression(b))
                    gen(ICall())
                }
            }

            
        
        // FIXME
        // handle:
        //    IfExp
        //    AppExp - "head" exp
        //           - "tail" exp
        //           - "length" exp
        //           - all other: exp exp
        //    ListExp
        //    LetExp

        case _ =>
            gen (IPrint ())
        }

        // Gather the expression's instructions and return them
        expInstrBuffer.result ()

    }

    

}
