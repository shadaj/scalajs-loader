package org.scalajs.linker.backend.emitter

import org.scalajs.linker.ModuleInitializer
import org.scalajs.linker.standard.{CommonPhaseConfig, CoreSpec, LinkedClass, LinkingUnit}

object CreateClassEmitter {
  def createJSGen(config: CommonPhaseConfig) = {
    import config.coreSpec._
    val internalOptions = InternalOptions()
    new JSGen(semantics, esFeatures, moduleKind, internalOptions,
      Set.empty)
  }

  def createClassEmitter(gen: JSGen) = {
    new ClassEmitter(gen)
  }

  def createLinkingUnit(spec: CoreSpec, classes: List[LinkedClass], modules: List[ModuleInitializer]) = {
    new LinkingUnit(spec, classes, modules)
  }
}
