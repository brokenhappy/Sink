package org.jetbrains.kotlin.compiler.plugin.template.ir

import org.jetbrains.kotlin.ir.declarations.IrFunction
import java.util.*

internal fun <T> Map<T, List<T?>>.findCycles(): List<List<T>> {
    val visited = mutableSetOf<T>()
    val recursionStack = mutableSetOf<T>()
    val cycles = mutableListOf<List<T>>()
    val currentPath = LinkedList<T>()

    fun dfs(node: T) {
        if (node in recursionStack) {
            val cycleStart = currentPath.indexOf(node)
            cycles.add(currentPath.subList(cycleStart, currentPath.size))
            return
        }
        if (node in visited) return

        visited.add(node)
        recursionStack.add(node)
        currentPath.addLast(node)

        this[node]?.forEach { next ->
            if (next != null) dfs(next)
        }

        recursionStack.remove(node)
        currentPath.removeLast()
    }

    keys.forEach { node ->
        if (node !in visited) {
            dfs(node)
        }
    }

    return cycles
}