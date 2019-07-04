package org.scalajs.linker.backend.emitter

import java.io.File

import org.scalajs.io.{NodeFS, WritableMemVirtualBinaryFile}
import org.scalajs.ir.Trees.{ClassDef, IRNode, Ident, MethodDef}
import org.scalajs.ir.Types.{ArrayTypeRef, ClassRef, ClassType}
import org.scalajs.ir.{Definitions, Position, Trees}
import org.scalajs.linker.backend.javascript.JSFileBuilder
import org.scalajs.linker.backend.javascript.{Trees => jsTrees}
import org.scalajs.linker.irio.{NodeVirtualJarScalaJSIRContainer, NodeVirtualScalaJSIRFile}
import org.scalajs.linker.standard._
import org.scalajs.linker.LinkerOutput

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel, JSImport}
import scala.util.Try

@JSExportTopLevel("generator")
object Main {
  val jarsClassPath = Seq(
    new NodeVirtualJarScalaJSIRContainer(
        js.Dynamic.global.process.env.HOME.asInstanceOf[String]
          + "/.ivy2/cache/org.scala-js/scalajs-library_2.12/jars/scalajs-library_2.12-1.0.0-M6.jar"
      )
  )

  val objectClass = jarsClassPath.flatMap(_.sjsirFiles).find(_.tree.name.name == Definitions.ObjectClass).get

  def irToClass(cls: Trees.ClassDef, allowedReferences: Option[Set[String]] = None): LinkedClass = {
    val filteredMembers = cls.memberDefs.collect {
      case m: Trees.MethodDef if m.body.isDefined =>
        if (allowedReferences.forall(refs =>
          findClassReferences(m).forall(r => isSpecial(r._1.className) || refs.contains(r._1.className))
        )) {
          new Versioned(m, None)
        } else {
          implicit val pos = Position.NoPosition
          new Versioned(m.copy(
            body = Some(Trees.Null())
          )(optimizerHints = m.optimizerHints, hash = None), None)
        }
    }

    new LinkedClass(
      name = cls.name,
      kind = cls.kind,
      jsClassCaptures = cls.jsClassCaptures,
      superClass = cls.superClass,
      interfaces = cls.interfaces,
      jsSuperClass = cls.jsSuperClass,
      jsNativeLoadSpec = cls.jsNativeLoadSpec,
      fields = cls.memberDefs.collect {
        case f: Trees.FieldDef => f
      },
      staticMethods = filteredMembers.filter(_.value.static),
      memberMethods = filteredMembers.filter(!_.value.static),
      List.empty,
      List.empty,
      cls.optimizerHints,
      cls.pos,
      ancestors = List(cls.name.name) ++ cls.superClass.map(_.name).toSeq ++ cls.interfaces.map(_.name).toSeq,
      true,
      true,
      true,
      None
    )
  }

  def findClassReferences(node: IRNode): Seq[(ClassType, Option[String])] = {
    node match {
      case c: Trees.ClassDef =>
        c.memberDefs.flatMap(findClassReferences)
      case md: Trees.MethodDef =>
        md.body.toSeq.flatMap(findClassReferences)
      case block: Trees.Block =>
        block.stats.flatMap(findClassReferences)
      case apply: Trees.Apply =>
        findClassReferences(apply.receiver) ++ apply.args.flatMap(findClassReferences)
      case newTree: Trees.New =>
        Seq(
          newTree.cls -> Some("$c_" + newTree.cls.className),
          newTree.cls -> Some("$d_" + newTree.cls.className)
        ) ++ newTree.args.flatMap(findClassReferences)
      case applyStatically: Trees.ApplyStatically =>
        Seq(
          applyStatically.cls -> Some("$c_" + applyStatically.cls.className),
          applyStatically.cls -> Some("$d_" + applyStatically.cls.className),
          applyStatically.cls -> Some("$h_" + applyStatically.cls.className),
          applyStatically.cls -> Some("$f_" + applyStatically.cls.className + "__" + applyStatically.method.name)
        ) ++ applyStatically.args.flatMap(findClassReferences)
      case loadModule: Trees.LoadModule =>
        Seq(loadModule.cls -> Some("$m_" + loadModule.cls.className))
      case isInstanceOf: Trees.IsInstanceOf =>
        (isInstanceOf.typeRef match {
          case clazz: ClassRef =>
            Seq(ClassType(clazz.className) -> Some("$is_" + clazz.className))
          case arrayOf: ArrayTypeRef =>
            Seq(ClassType(arrayOf.baseClassName) -> Some("$isArrayOf_" + arrayOf.baseClassName))
        }) ++ findClassReferences(isInstanceOf.expr)
      case asInstanceOf: Trees.AsInstanceOf =>
        (asInstanceOf.typeRef match {
          case clazz: ClassRef =>
            Seq(ClassType(clazz.className) -> Some("$as_" + clazz.className))
          case arrayOf: ArrayTypeRef =>
            Seq(ClassType(arrayOf.baseClassName) -> Some("$asArrayOf_" + arrayOf.baseClassName))
        }) ++ findClassReferences(asInstanceOf.expr)
      case _if: Trees.If =>
        findClassReferences(_if.cond) ++ findClassReferences(_if.thenp) ++ findClassReferences(_if.elsep)
      case unaryOp: Trees.UnaryOp =>
        findClassReferences(unaryOp.lhs)
      case _throw: Trees.Throw =>
        findClassReferences(_throw.expr)
      case binaryOp: Trees.BinaryOp =>
        findClassReferences(binaryOp.lhs) ++ findClassReferences(binaryOp.rhs)
      case assign: Trees.Assign =>
        findClassReferences(assign.lhs) ++ findClassReferences(assign.rhs)
      case varDef: Trees.VarDef =>
        findClassReferences(varDef.rhs)
      case unbox: Trees.Unbox =>
        findClassReferences(unbox.expr)
      case _match: Trees.Match =>
        findClassReferences(_match.selector) ++
          _match.cases.flatMap(c => findClassReferences(c._2)) ++
          findClassReferences(_match.default)
      case jsUnaryOp: Trees.JSUnaryOp =>
        findClassReferences(jsUnaryOp.lhs)
      case jsBinaryOp: Trees.JSBinaryOp =>
        findClassReferences(jsBinaryOp.lhs) ++ findClassReferences(jsBinaryOp.rhs)
      case jsBracketSelect: Trees.JSBracketSelect =>
        findClassReferences(jsBracketSelect.qualifier) ++ findClassReferences(jsBracketSelect.item)
      case jsNew: Trees.JSNew =>
        findClassReferences(jsNew.ctor) ++ jsNew.args.flatMap(findClassReferences)
      case loadJSConstructor: Trees.LoadJSConstructor =>
        Seq(loadJSConstructor.cls -> None)
      case jsGlobalRef: Trees.JSGlobalRef =>
        Seq.empty
      case arraySelect: Trees.ArraySelect =>
        findClassReferences(arraySelect.array) ++ findClassReferences(arraySelect.index)
      case arrayLength: Trees.ArrayLength =>
        findClassReferences(arrayLength.array)
      case _while: Trees.While =>
        findClassReferences(_while.cond) ++ findClassReferences(_while.body)
      case closure: Trees.Closure =>
        findClassReferences(closure.body)
      case labeled: Trees.Labeled =>
        findClassReferences(labeled.body)
      case _return: Trees.Return =>
        findClassReferences(_return.expr)
      case tryCatch: Trees.TryCatch =>
        findClassReferences(tryCatch.block) ++ findClassReferences(tryCatch.handler)
      case tryFinally: Trees.TryFinally =>
        findClassReferences(tryFinally.block) ++ findClassReferences(tryFinally.finalizer)
      case jsFunctionApply: Trees.JSFunctionApply =>
        findClassReferences(jsFunctionApply.fun) ++ jsFunctionApply.args.flatMap(findClassReferences)
      case getClass: Trees.GetClass =>
        findClassReferences(getClass.expr)
      case jsBracketMethodApply: Trees.JSBracketMethodApply =>
        findClassReferences(jsBracketMethodApply.receiver) ++ findClassReferences(jsBracketMethodApply.method) ++ jsBracketMethodApply.args.flatMap(findClassReferences)
      case loadJSModule: Trees.LoadJSModule =>
        Seq(loadJSModule.cls -> None)
      case jsArrayConstr: Trees.JSArrayConstr =>
        jsArrayConstr.items.flatMap(findClassReferences)
      case jsObjectConstr: Trees.JSObjectConstr =>
        jsObjectConstr.fields.flatMap(t => findClassReferences(t._2))
      case propertyDef: Trees.PropertyDef =>
        propertyDef.getterBody.toSeq.flatMap(findClassReferences)
      case doWhile: Trees.DoWhile =>
        findClassReferences(doWhile.cond) ++ findClassReferences(doWhile.body)
      case jsSpread: Trees.JSSpread =>
        findClassReferences(jsSpread.items)
      case jsDelete: Trees.JSDelete =>
        findClassReferences(jsDelete.prop)
      case Trees.ClassOf(typeRef: ClassRef) =>
        val refName = typeRef.className
        Seq(ClassType(refName) -> Some("$d_" + refName))
      case jsLinkingInfo: Trees.JSLinkingInfo =>
        Seq.empty
      case newArray: Trees.NewArray =>
        Seq.empty
      case storeModule: Trees.StoreModule =>
        Seq.empty // ???
      case fieldDef: Trees.FieldDef =>
        Seq.empty // ???
      case select: Trees.Select =>
        Seq.empty // ???
      case varRef: Trees.VarRef => Seq.empty
      case literal: Trees.Literal => Seq.empty
      case ident: Trees.Ident => Seq.empty
      case skip: Trees.Skip => Seq.empty
      case _this: Trees.This => Seq.empty
    }
  }

  val targetClasses = "./minimal-scalajs/target/scala-2.12/classes"

  println("creating map?")
  val classPathMap = jarsClassPath.flatMap(_.sjsirFiles.map(f => f.tree.name.name -> f.tree)).toMap
  def getClassOnPath(name: String): Option[Trees.ClassDef] = {
    val relativePath = name.dropWhile(_ == 'L').split('_').mkString("/") + ".sjsir"
    if (NodeFS.asInstanceOf[js.Dynamic].existsSync(targetClasses + "/" + relativePath).asInstanceOf[Boolean]) {
      Some(new NodeVirtualScalaJSIRFile(targetClasses + "/" + relativePath, relativePath).tree)
    } else {
      classPathMap.get(name)
    }
  }

  def collectInheritedClasses(name: String): List[LinkedClass] = {
    val currentClass = getClassOnPath(name)
    currentClass.toList.flatMap(c => irToClass(c) :: c.superClass.toList.flatMap(n => collectInheritedClasses(n.name)))
  }

  def isSpecial(name: String) = {
    Set("J", "F", "I", "B", "D", "C", "Z", "S").contains(name)
  }

  def generateForIR(tree: Trees.ClassDef): String = {
    val references = findClassReferences(tree) ++ Seq(
      ClassType("T") -> Some("$f_T__indexOf__T__I"),
      ClassType("T") -> Some("$f_T__substring__I__I__T"),
      ClassType("T") -> Some("$f_T__substring__I__T")
    )
    val referencedLinked = references.map(_._1.className).distinct.filterNot(isSpecial)
      .toList.flatMap(c => getClassOnPath(c).map(irToClass(_)))

    val emitter = new Emitter(CommonPhaseConfig())
    val filteredIR = irToClass(tree, Some(referencedLinked.map(_.name.name).toSet))
    emitter.startRun(
      new LinkingUnit(
        CoreSpec.Defaults,
        filteredIR :: collectInheritedClasses(tree.name.name).tail ::: referencedLinked,
        List.empty
      )
    )

    val result = try {
      val generatedClass = emitter.genClass(
        filteredIR,
        irToClass(objectClass.tree)
      )

      val out = new WritableMemVirtualBinaryFile
      val builder = new JSFileBuilder(LinkerOutput(out))

      val toExport = mutable.Queue[String]()
      def processExports(tree: jsTrees.Tree): Unit = {
        tree match {
          case v: jsTrees.VarDef =>
            toExport.enqueue(v.name.name)
          case f: jsTrees.FunctionDef =>
            toExport.enqueue(f.name.name)
          case b: jsTrees.Block =>
            b.stats.foreach(processExports)
          case a =>
        }
      }

      generatedClass.main.foreach { (b: jsTrees.Tree) =>
        processExports(b)
        builder.addJSTree(b)
      }

      builder.addLine(s"""module.exports = { ${toExport.map(s => s"$s: $s").mkString(",")} };""")

      builder.complete()
      val mapping = references.distinct.groupBy(_._1).flatMap { case (c, v) =>
        getClassOnPath(c.className) match {
          case Some(cls) =>
            v.flatMap { case (_, usedTerm) =>
              usedTerm.filter(t => !toExport.contains(t)).map { used =>
                used -> s"""require("sjs://${c.className}").$used"""
              }
            }
          case None =>
            Seq.empty
            //builder.addLine(s"""// unlinkable class: ${c.className}""")
        }
      }.toSeq

      mapping.foldLeft(new String(out.content)) { (cur, swap) =>
        cur.split('\n').map { l =>
          if (l.startsWith("function") || l.startsWith("module.exports")) {
            l
          } else {
            l.replaceAllLiterally(swap._1 + "(", "(" + swap._2 + ")(")
              .replaceAllLiterally(swap._1 + ".", swap._2 + ".")
              .replaceAllLiterally(swap._1 + " ", swap._2 + " ")
              .replaceAllLiterally(swap._1 + ";", swap._2 + ";")
              .replaceAllLiterally(" " + swap._1 + ")", " " + swap._2 + ")")
          }
        }.mkString("\n")
      }
    } catch {
      case e: NoSuchElementException =>
        e.printStackTrace()
        "// unlinkable references"
    }

    result
  }

  @JSExport("path")
  def path(name: String): js.UndefOr[String] = {
    val relativePath = name.dropWhile(_ == 'L').split('_').mkString("/") + ".sjsir"
    if (classPathMap.contains(name)) {
      js.undefined
    } else targetClasses + "/" + relativePath
  }

  @JSExport("generate")
  def generate(name: String) = {
    getClassOnPath(name).map(generateForIR).getOrElse("")
  }
}
