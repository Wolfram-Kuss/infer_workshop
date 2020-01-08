package types

import syntax.*
import kotlin.Exception

inline class Substitution(val subst: HashMap<Int, Monotype> = hashMapOf()) {
    fun apply(ty: Monotype): Monotype = when (ty) {
        is Monotype.Unknown ->
            subst[ty.u]?.let { apply(it) } ?: ty
        is Monotype.Function ->
            Monotype.Function(apply(ty.argument), apply(ty.result))
        else -> ty
    }

    override fun toString(): String =
        "{ " + subst.toList().joinToString("\n, ") { (u, ty) -> "u$u ↦ ${ty.pretty()}" } + "\n}"
}

inline class Environment(val env: HashMap<Name, Monotype> = hashMapOf()) {
    fun clone(): Environment = Environment(HashMap(env))
    operator fun get(name: Name): Monotype? = env[name]
    operator fun set(name: Name, ty: Monotype) {
        env[name] = ty
    }
}

data class CheckState(
    var environment: Environment = Environment(),
    val substitution: Substitution = Substitution(),
    var fresh_supply: Int = 0
)

class TypeChecker(var checkState: CheckState) {

    // Returns a fresh `Unknown`, where fresh means "not ever used before"
    private fun freshUnknown(): Monotype = Monotype.Unknown(++checkState.fresh_supply)

    // Applies the current substitution to a given type
    fun zonk(ty: Monotype): Monotype = checkState.substitution.apply(ty)

    fun unify(ty1: Monotype, ty2: Monotype) {
        val ty1 = zonk(ty1)
        val ty2 = zonk(ty2)

        if (ty1 == ty2) return
        when {
            ty1 is Monotype.Unknown -> solveType(ty1.u, ty2)
            ty2 is Monotype.Unknown -> solveType(ty2.u, ty1)
            ty1 is Monotype.Function && ty2 is Monotype.Function -> {
                unify(ty1.argument, ty2.argument)
                unify(ty1.result, ty2.result)
            }
            else -> throw Exception("Can't match ${ty1.pretty()} with ${ty2.pretty()}")
        }
    }

    private fun solveType(u: Int, ty: Monotype) {
        if (ty.unknowns().contains(u)) throw Exception("Occurs check failed")
        checkState.substitution.subst[u] = ty
    }

    private fun infer(expr: Expression): Monotype {
        return when (expr) {
            is Expression.Int -> Monotype.Int
            is Expression.Bool -> Monotype.Bool
            is Expression.String -> Monotype.String
            is Expression.Var ->
                checkState.environment[expr.name] ?: throw Exception("Unknown variable ${expr.name}")
            is Expression.Let -> {
                val tyBinder = freshUnknown()
                withName(expr.binder, tyBinder) {
                    val inferredBinder = infer(expr.expr)
                    unify(tyBinder, inferredBinder)
                    infer(expr.body)
                }
            }
            is Expression.Lambda -> {
                val tyArg = freshUnknown()
                val tyRes = withName(expr.binder, tyArg) {
                    infer(expr.body)
                }
                Monotype.Function(tyArg, tyRes)
            }
            is Expression.App -> {
                val tyFun = infer(expr.function)
                val tyArg = infer(expr.argument)
                val tyRes = freshUnknown()
                unify(tyFun, Monotype.Function(tyArg, tyRes))
                tyRes
            }
            is Expression.If -> {
                val tyCond = infer(expr.condition)
                unify(tyCond, Monotype.Bool)
                val tyThen = infer(expr.thenCase)
                val tyElse = infer(expr.elseCase)
                unify(tyThen, tyElse)
                tyThen
            }
        }
    }

    private fun <T> withName(binder: Name, ty: Monotype, action: () -> T): T {
        val tmp = checkState.environment.clone()
        checkState.environment[binder] = ty
        val result = action()
        checkState.environment = tmp
        return result
    }

    fun inferExpr(expr: Expression): Monotype = zonk(infer(expr))
}