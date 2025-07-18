package org.jetbrains.kotlin.compiler.plugin.template

import org.jetbrains.kotlin.backend.common.extensions.IrGenerationExtension
import org.jetbrains.kotlin.compiler.plugin.CompilerPluginRegistrar
import org.jetbrains.kotlin.compiler.plugin.template.ir.SimpleIrGenerationExtension
import org.jetbrains.kotlin.config.CompilerConfiguration

class SimplePluginComponentRegistrar: CompilerPluginRegistrar() {
    override val supportsK2: Boolean
        get() = true

    override fun ExtensionStorage.registerExtensions(configuration: CompilerConfiguration) {
        IrGenerationExtension.registerExtension(SimpleIrGenerationExtension())
    }
}
