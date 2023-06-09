// The following files are imported for their effects.
// In particular, they populate the defmethod tables for
// various kinds of arithmetic
import './emmy.numbers.js';
import './emmy.abstract.number.js';
import './emmy.value.js';
let core = require('./cljs.core.js');
let generic = require('./emmy.generic.js')
let value = require('./emmy.value.js')
let emmy_function = require('./emmy.function.js')
let abstract_function = require('./emmy.abstract.function.js')
let lagrange = require('./emmy.mechanics.lagrange.js')
let structure = require('./emmy.structure.js')
let derivative = require('./emmy.calculus.derivative.js')
let render = require('./emmy.expression.render.js')
let quadrature = require('./emmy.numerical.quadrature')
let minimize = require('./emmy.numerical.minimize.js')
let ode = require('./emmy.numerical.ode')

console.log(lagrange);

export class Emmy {

    // TODO:
    // 1. Consider whether make_es6_callable should live in structure? it
    //    probably belongs in value.
    // 2. Consider detecting structure nature in the result of generic
    //    operations and decorating the return values of all the generic
    //    ops. We can use an in-place metadata tag to store the application
    //    of these properties to avoid doing it twice.
    // 3. Rename make_es6_gettable to make_es6_indexable which is less uncouth
    public up(...xs: any[]) {
        return structure.make_es6_gettable(
            value.make_es6_callable(
                structure.up(...xs),
                structure.make_es6_gettable))
    }
    public isUp(u: any): boolean {
        return structure.up_QMARK_(u)
    }
    public isVector(v: any): boolean {``
        return core.vector_QMARK_(v)
    }
    public down(...xs: any[]) {
        return structure.make_es6_gettable(
            value.make_es6_callable(
                structure.down(...xs),
                structure.make_es6_gettable))
    }
    public add = generic._PLUS_
    public div = generic._SLASH_
    public sub = generic._
    public mul = generic._STAR_

    public mapr = structure.mapr
    public velocity = lagrange.velocity
    public toInfix = render.__GT_infix
    public Gamma = lagrange.Gamma
    public time = lagrange.time
    public coordinate = lagrange.coordinate
    public stateAdvancer = ode.state_advancer
    public flatten = core.flatten
    public toJS = core.clj__GT_js
    public osculatingPath = lagrange.osculating_path
    public count = core.count

    public solveLinear(a: any, b: any): any {
        return generic.solve_linear.call(null, a, b)
    }

    public crossProduct(v: any, w: any): any {
        return structure.make_es6_gettable(structure.cross_product(v, w))
    }

    public minimize(f: any, x0: number, x1: number) {
        return core.clj__GT_js(minimize.minimize(f, x0, x1))
    }

    public multidimensionalMinimize(f: any, x0: any, opts?: object) {
        return core.clj__GT_js(minimize.multidimensional_minimize(f, x0, opts))
    }

    public definiteIntegral(f: any, a: number, b: number, opts?: object): number {
        return quadrature.definite_integral(f, a, b, opts)
    }

    public linearInterpolants(x0: any, x1: any, n: number) {
        return lagrange.linear_interpolants(x0, x1, n)
    }

    public makePath(t0: any, q0: any, t1: any, q1: any, qs: any) {
        // this raises the open question of whether it is better to make every
        // constructed polynomial object callable by interpolating make_es6_callable
        // into it construction path, or only using that process here in the JS
        // wrapper.
        return value.make_es6_callable(lagrange.make_path(t0, q0, t1, q1, qs), f=>f)
    }
    public dotProduct(u: any, v: any) {
        return generic.dot_product.call(null, u, v)
    }

    public D(f: any) {
        return derivative.D.call(null, f)
    }
    public compose(...fs: any[]) {
        return emmy_function.compose.call(null, ...fs)
    }

    // add(...xs: any[]): any {
    //     return generic._PLUS_(...xs)
    // }
    public nth = core.nth
    eq(a: any, b: any): boolean {
        return core._EQ_(a, b)
    }
    atan(y: any, x?: any) {
        if (x === undefined) return generic.atan.call(null, y)
        return generic.atan.call(null, y, x)
    }
    simplify(a: any): any {
        return generic.simplify.call(null, a)
    }
    sin(a: any): any {
        return generic.sin.call(null, a)
    }
    expt(a: any, b: any): any {
        return generic.expt.call(null, a, b)
    }
    cos(a: any): any {
        return generic.cos.call(null, a)
    }
    literalFunction(s: string): any {
        return value.make_es6_callable(abstract_function.literal_function(e.symbol(s)), x=>x)
    }
    partial(...ns: number[]): any {
        return (...xs: any[]) => derivative.partial(...ns).call(null, ...xs)
    }
    typeof(x: any): string {
        return core.type(x).toString()
    }
    LagrangianToStateDerivative(L: any): any {
        return lagrange.Lagrangian__GT_state_derivative(L)
    }
    LagrangeEquations(L: any): any {
        return lagrange.Lagrange_equations(L)
    }
    LagrangianToEnergy(L: any): any {
        return lagrange.Lagrangian__GT_energy(L)
    }
    make_es6_callable(x: any): any { return value.make_es6_callable(x) }
    square(v: any): any { return generic.square.call(null, v) }
    sqrt(v: any): any { return generic.sqrt.call(null, v) }
    print(x: any) {
        core.print(x)
    }
    symbol(s: string): any { // this actually has a return type: can we say what it is?
        return core.symbol(s)
    }
    withSymbols(symbols: string, f: (...a: any[]) => any) {
        const args = symbols.trim().split(/\s+/).map(x => {
            if (x.endsWith('()')) {
                return this.literalFunction(x.substring(0, x.length - 2))
            } else {
                return this.symbol(x)
            }
        })
        return f(...args)
    }
}

const e = new Emmy()
