package org.jetbrains.kotlin.compiler.plugin.template

import org.jetbrains.kotlin.GeneratedDeclarationKey
import org.jetbrains.kotlin.backend.common.extensions.IrGenerationExtension
import org.jetbrains.kotlin.compiler.plugin.CompilerPluginRegistrar
import org.jetbrains.kotlin.compiler.plugin.template.ir.SimpleIrGenerationExtension
import org.jetbrains.kotlin.compiler.plugin.template.ir.asValidJavaIdentifier
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.fir.FirModuleData
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.extensions.*
import org.jetbrains.kotlin.fir.moduleData
import org.jetbrains.kotlin.fir.plugin.createTopLevelClass
import org.jetbrains.kotlin.fir.plugin.createTopLevelFunction
import org.jetbrains.kotlin.fir.symbols.impl.FirClassLikeSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirNamedFunctionSymbol
import org.jetbrains.kotlin.fir.types.constructType
import org.jetbrains.kotlin.name.CallableId
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name

class SimplePluginComponentRegistrar: CompilerPluginRegistrar() {
    override val supportsK2: Boolean
        get() = true

    override fun ExtensionStorage.registerExtensions(configuration: CompilerConfiguration) {
        IrGenerationExtension.registerExtension(SimpleIrGenerationExtension())
        FirExtensionRegistrarAdapter.registerExtension(object : FirExtensionRegistrar() {
            override fun ExtensionRegistrarContext.configurePlugin() {
                +::FirGenerator
            }
        })
    }
}

@OptIn(ExperimentalTopLevelDeclarationsGenerationApi::class)
class FirGenerator(session: FirSession) : FirDeclarationGenerationExtension(session) {
    override fun getTopLevelClassIds(): Set<ClassId> = setOf(metadataOverloadType())

    private fun metadataOverloadType(): ClassId = ClassId(
        packageFqName = metadataFunctionCallableId.packageName,
        topLevelName = Name.identifier(session.moduleData.somewhatIdentifyingName()),
    )

    override fun getTopLevelCallableIds(): Set<CallableId> = setOf(metadataFunctionCallableId)

    override fun hasPackage(packageFqName: FqName): Boolean =
        packageFqName == metadataFunctionCallableId.packageName || super.hasPackage(packageFqName)

    override fun generateFunctions(
        callableId: CallableId,
        context: MemberGenerationContext?
    ): List<FirNamedFunctionSymbol> = listOf(
        createTopLevelFunction(
            SinkGeneratedDeclaration,
            callableId,
            session.builtinTypes.unitType.coneType,
        ) {
            this.valueParameter(
                Name.identifier("a"),
                generateTopLevelClassLikeDeclaration(metadataOverloadType()).constructType(),
            )
        }.symbol
    )

    override fun generateTopLevelClassLikeDeclaration(classId: ClassId): FirClassLikeSymbol<*> = createTopLevelClass(
        classId = classId,
        key = SinkGeneratedMetadataOverloadClass,
        classKind = ClassKind.INTERFACE,
        config = {},
    ).symbol
}

internal object SinkGeneratedMetadataOverloadClass: GeneratedDeclarationKey()
internal object SinkGeneratedDeclaration: GeneratedDeclarationKey()

/**
 * Each module processed by the compiler plugin will also generate some metadata.
 * This metadata is stored in a function that is always called [metadataPackageName].[somewhatIdentifyingName].
 * We do this so we can request all functions by that name, and then ask for the metadata for all these functions.
 *
 * However, if that was everything, then all modules would create the same function. Which would cause issues.
 * Therefore, we also overload the function with a type. This type should get a unique name for each module.
 *
 * That's what this function does. It gets a name that's "somewhat unique" to this module.
 * In theory there could be a collision, but I don't think that will happen.
 */
internal fun FirModuleData.somewhatIdentifyingName(): String = (name.asString().asValidJavaIdentifier() + "_" + listOf(
    this.dependencies.size,
    this.friendDependencies.size,
    this.dependsOnDependencies.size,
    this.isCommon,
).hashCode().toLong()).plus(Integer.MAX_VALUE.toLong()) + this.platform.toString().asValidJavaIdentifier()

internal val metadataFunctionCallableId = CallableId(FqName("__sink_metadata__"), null, Name.identifier("_sinkMetadata_"))
