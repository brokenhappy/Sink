package com.woutwerkman.sink.compiler.plugin.ir

import com.woutwerkman.sink.compiler.plugin.ir.DeclarationContainer.ObjectAsContainer
import org.jetbrains.kotlin.backend.common.extensions.IrPluginContext
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrFile
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.name
import org.jetbrains.kotlin.ir.expressions.IrCall
import org.jetbrains.kotlin.ir.types.IrSimpleType
import org.jetbrains.kotlin.ir.util.hasShape
import org.jetbrains.kotlin.ir.util.isObject
import org.jetbrains.kotlin.ir.visitors.IrTransformer
import org.jetbrains.kotlin.javac.resolve.classId
import org.jetbrains.kotlin.name.CallableId
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name


internal fun DependencyGraphFromSources.transformAllGetCalls(
    module: IrModuleFragment,
    pluginContext: IrPluginContext,
    injectableCacheType: IrSimpleType,
    transform: (IrCall, graphToResolveFrom: DependencyGraphFromSources, IrFile) -> IrElement,
) {
    val getFunctionSymbol = pluginContext.referenceFunctions(
        CallableId(
            classId = ClassId.topLevel(FqName("com.woutwerkman.sink.InjectionCache")),
            callableName = Name.identifier("get"),
        ),
    ).single { fn ->
        fn.owner.typeParameters.size == 1 && fn.owner.hasShape(
            dispatchReceiver = true,
            extensionReceiver = false,
            contextParameters = 0,
            regularParameters = 1,
            parameterTypes = listOf(injectableCacheType),
        )
    }

    val objectsToGraphs = mutableMapOf<IrClass, DependencyGraphFromSources>()
    val filesToGraphs = mutableMapOf<IrFile, DependencyGraphFromSources>()
    fun DependencyGraphFromSources.addAll() {
        when (val container = container) {
            is DeclarationContainer.ModuleAsContainer -> { /* Do nothing */ }
            is DeclarationContainer.FileAsContainer -> filesToGraphs[container.file] = this
            is ObjectAsContainer -> objectsToGraphs[container.objectDeclaration] = this
        }
        children.forEach { it.addAll() }
    }
    addAll()

    module.files.forEach { file ->
        file.transformChildren(object: IrTransformer<DependencyGraphFromSources>() {
            override fun visitClass(declaration: IrClass, data: DependencyGraphFromSources): IrStatement =
                if (declaration.isObject) super.visitClass(declaration, objectsToGraphs[declaration] ?: error("Somehow we didn't create a graph for ${declaration.name}"))
                else super.visitClass(declaration, data)

            override fun visitCall(expression: IrCall, data: DependencyGraphFromSources): IrElement {
                val original = super.visitCall(expression, data).let { it as? IrCall ?: return it }
                return if (original.symbol != getFunctionSymbol) original
                else transform(expression, data, file)
            }
        }, filesToGraphs[file] ?: error("Somehow we didn't create a graph for ${file.name}"))
    }
}

class Foo {
    companion object {
        private val a = 1
    }

    val b = a // TODO: Support this!!!! Currently we consider the companion object out of scope right?
}