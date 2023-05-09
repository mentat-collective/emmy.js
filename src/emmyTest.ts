import { Emmy } from './emmy';
import { Assertion, expect } from 'chai'
import 'mocha';

const e = new Emmy()

Assertion.addMethod('emmyEqual', function (this: Chai.AssertionStatic, expected: any) {
    let obj = this._obj
    this!.assert(e.eq(obj, expected),
        `expected ${obj} and ${expected} to be equal in the Emmy sense`,
        `expected ${obj} and ${expected} not to be equal in the Emmy sense`,
        expected)
})

declare global {
    module Chai {
        interface Assertion {
            emmyEqual(expected: any): void;
        }
    }
}

describe('Emmy', () => {
    const pe = (expr: any) => e.toInfix(e.simplify(expr))

    describe('works', () => {
        e.withSymbols('x y', (x, y) => {
            it('can add x and 1', () => {
                expect(e.add(x, 1).toString()).to.equal("(+ x 1)")
            })
            it('can add 2 and y', () => { expect(e.add(2, y).toString()).to.equal("(+ 2 y)") })
            it('can add x and y', () => { expect(e.add(x, y).toString()).to.equal("(+ x y)") })
            it('can add several things', () => expect(e.add(x, y, x, y, x).toString()).to.equal("(+ x y x y x)"))
        })
        expect(1).to.be.ok
    })
    describe('literal functinos', () => {
        const f = e.literalFunction('f')
        const x = e.symbol('x')
        it('can be applied', () => {
            expect(pe(f(x))).to.equal('f(x)')
        })
    })
    describe('structures', () => {
        const u = e.up(4, 5, 6, 7)
        describe('are indexable', () => {
            it('by small integers', () => {
                expect(u[0]).to.equal(4)
                expect(u[1]).to.equal(5)
                expect(u[2]).to.equal(6)
                expect(u[3]).to.equal(7)
            })
            it('using at', () => {
                expect(u.at(1)).to.equal(5)
            })
        })
        describe('are callable', () => {
            const u = e.up(Math.sin, Math.cos)
            it('(up sin cos)', () => {
                1
                expect(u(0)).to.emmyEqual([0, 1])
                expect(u(Math.PI)[0]).to.be.closeTo(0, 1e-10)
                expect(u(Math.PI)[1]).to.be.closeTo(-1, 1e-10)
            })
        })
        describe('can be built', () => {
            it('from spread arguments', () => {
                const a = [6, 12, 18];
                const d = e.down(...a);
                expect(d).to.emmyEqual(e.down(6, 12, 18))
            })
        })
        describe('can build arrays', () => {
            it('using Array.from', () => {
                const a = Array.from(e.down(4, 5, 6))
                expect(a).to.deep.equal([4, 5, 6])
            })
        })
        describe('can map', () => {
            it('in one dimension', () => {
                const u = e.up(4, 5, 6)
                expect(u.map((x: number) => x + 1)).to.emmyEqual(e.up(5, 6, 7))
            })
            it('in two dimensions', () => {
                const u = e.down(e.up(1, 2), e.up(3, 4))
                expect(u.map((x: number) => x * x)).to.emmyEqual(
                    e.down(e.up(1, 4), e.up(9, 16))
                )
            })
        })
    })
    describe('can do calculus', () => {
        it('can operate on functions with D', () => {
            expect(e.D(e.sin)(0)).to.equal(1)
            expect(e.D(e.sin)(e.symbol('x')).toString()).to.equal('(cos x)')
        })
    })
    describe('can compute physics of free space', () => {
        let T = m => ([t, x, v]) => e.mul(0.5, m, e.square(v));
        let V = m => ([t, x, v]) => 0
        let L = e.sub(T, V);
        it('can apply T to symbolic arguments', () => {
            expect(
                e.withSymbols('m t x v', (m, t, x, v) => T(m)([t, x, v])).toString()
            ).to.equal('(* 0.5 m (expt v 2))');
        })
        it('can apply L to symbolic arguments (3)', () => {
            expect(
                e.withSymbols('m t x v', (m, t, x, v) => L(m)([t, x, v])).toString()
            ).to.equal('(* 0.5 m (expt v 2))');
        })
        let sd = e.LagrangianToStateDerivative(L(e.symbol('m')))
        it('can compute state derivative', () => {
            e.withSymbols('t x v', (t, x, v) => {
                expect(sd(e.up(t, x, v))).to.emmyEqual(e.up(1, v, 0))
            })
        })
        it('can compute a partial derivative', () => {
            e.withSymbols('m t x v', (m, t, x, v) => {
                const Lm = L(m)
                const state = e.up(t, x, v)
                expect(e.simplify(e.partial(0)(Lm)(state))).to.equal(0)
                expect(e.simplify(e.partial(1)(Lm)(state))).to.equal(0)
                expect(e.simplify(e.partial(2)(L(m))(state)).toString()).to.equal('(* m v)')
            })
        })
        it('can compute Lagrange equations', () => {
            const Lm = L(e.symbol('m'))
            const x = e.literalFunction(e.symbol('x'))
            expect(
                e.simplify(
                    e.LagrangeEquations(Lm)(x)(e.symbol('t'))
                ).toString(
                )).to.equal('(* m (((expt D 2) x) t))')
        })
    })
    describe("SICM chapter 1", () => {
        const LFreeParticle = mass => local => {
            const v = e.velocity(local)
            return e.mul(1 / 2, mass, e.dotProduct(v, v))
        }
        const L_harmonic = (m, k) => ([t, q, v]) => e.sub(
            e.mul(1 / 2, m, e.square(v)),
            e.mul(1 / 2, k, e.square(q))
        )
        const [m, t, k] = ['m', 't', 'k'].map(e.symbol)
        describe('1.4 Computing Actions', () => {
            const q = e.up(
                e.literalFunction('x'),
                e.literalFunction('y'),
                e.literalFunction('z'))

            it('computes Γ(q)(t)', () => {
                expect(pe(e.Gamma(q)(t))).to.equal(
                    'up(t, up(x(t), y(t), z(t)), up(Dx(t), Dy(t), Dz(t)))')
            })
            it('can compose LFreeParticle with Gamma', () => {
                expect(pe(e.compose(LFreeParticle(m), e.Gamma(q))(t))).to.equal(
                    '0.5 m (Dx(t))² + 0.5 m (Dy(t))² + 0.5 m (Dz(t))²'
                )
            })
            function LagrangianAction(L: any, q: any, t1: number, t2: number) {
                return e.definiteIntegral(e.compose(L, e.Gamma(q)), t1, t2)
            }
            const testPath = (t: any) => e.up(
                e.add(e.mul(4, t), 7),
                e.add(e.mul(3, t), 5),
                e.add(e.mul(2, t), 1)
            )
            expect(LagrangianAction(LFreeParticle(3.0), testPath, 0, 10.0)).to.equal(435)
            const makeEta = (nu: any, t1: number, t2: number) => (t: any) => e.mul(e.sub(t, t1), e.sub(t, t2), nu(t))
            const variedFreeParticleAction = (mass: number, q: any, nu: any, t1: number, t2: number) => (eps: number) => {
                const eta = makeEta(nu, t1, t2)
                return LagrangianAction(LFreeParticle(mass), e.add(q, e.mul(eps, eta)), t1, t2)
            }
            const nu = e.up(e.sin, e.cos, e.square)
            const vfpa = variedFreeParticleAction(3.0, testPath, nu, 0.0, 10.0)
            it('can vary free particle action', () => {
                expect(vfpa(0.001)).to.be.closeTo(436.2912143, 1e-7)
            })
            it('can minimize over one variable', () => {
                const minResult = e.minimize(vfpa, -2.0, 1.0)
                expect(minResult['converged?']).to.be.true
                expect(minResult.value).to.be.closeTo(435, 1e-7)
                expect(minResult.result).to.be.closeTo(0, 1e-7)
            })

            function parametricPathAction(Lagrangian: any, t0: number, q0: number[], t1: number, q1: number[]) {
                return (qs: any) => {
                    const path = e.makePath(t0, q0, t1, q1, qs)
                    return LagrangianAction(Lagrangian, path, t0, t1)
                }
            }

            function findPath(Lagrangian: any, t0: number, q0: any, t1: number, q1: any, n: number) {
                const initialQs = e.linearInterpolants(q0, q1, n)
                const minimizingQs = e.multidimensionalMinimize(parametricPathAction(Lagrangian, t0, q0, t1, q1), initialQs)
                return e.makePath(t0, q0, t1, q1, minimizingQs)
            }

            it('can find a minimal parametric path', () => {
                const q_harmonic = findPath(L_harmonic(1, 1), 0, 1, (Math.PI / 2), 0, 3)
                expect(q_harmonic(0.8)).to.be.closeTo(Math.cos(0.8), 1e-5)
            })
        })
        describe('1.5 The Euler Lagrange Equations', () => {
            const generalTestPath = (t: any) => e.withSymbols('a a_0 b b_0 c c_0', (a, a_0, b, b_0, c, c_0) =>
                e.up(
                    e.add(e.mul(a, t), a_0),
                    e.add(e.mul(b, t), b_0),
                    e.add(e.mul(c, t), c_0)))

            it('can compute L equations for general test path', () => {
                expect(pe(e.LagrangeEquations(LFreeParticle(m))(generalTestPath)(t))).to.equal('down(0, 0, 0)')
            })
            it('can compute L equations with literal function', () => {
                expect(pe(e.LagrangeEquations(LFreeParticle(m))(e.literalFunction('x'))(t))).to.equal('m D²x(t)')
            })
            it('can solve given a proposed solution', () => {
                const proposedSolution = e.withSymbols('A omega phi',
                    (A, omega, phi) =>
                        (t: any) => e.mul(A, e.cos(e.add(e.mul(omega, t), phi))))

                expect(pe(e.LagrangeEquations(L_harmonic(m, k))(proposedSolution)(t))).to.equal(
                    '- A m ω² cos(ω t + φ) + A k cos(ω t + φ)')
            })
            it('Exercise 1.11: Kepler\'s third law', () => {
                const LKeplerCentralPolar = (m, V) => ([t, [r, phi], [rdot, phidot]]) => e.sub(
                    e.mul(1 / 2, m, e.add(e.square(rdot), e.square(e.mul(r, phidot)))),
                    V(r)
                )
                const gravitationalEnergy = (G, m1, m2) => r => e.sub(e.div(e.mul(G, m1, m2), r))
                const circle = t => e.up(e.symbol('a'), e.mul(e.symbol('n'), t))
                const lagrangianReduced = e.withSymbols('G M_1 m_2', (G, M1, m2) =>
                    LKeplerCentralPolar(
                        e.div(e.mul(M1, m2), e.add(M1, m2)),
                        gravitationalEnergy(G, M1, m2)))

                expect(pe(e.LagrangeEquations(lagrangianReduced)(circle)(t))).to.equal(
                    'down((- M₁ a³ m₂ n² + G M₁² m₂ + G M₁ m₂²) / (M₁ a² + a² m₂), 0)')
            })
        })
        describe('1.6 How to Find Lagrangians', () => {
            const LUniformAcceleration = (m, g) => ([t, [x, y], v]) => e.sub(
                e.mul(1/2, m, e.square(v)),
                e.mul(m, g, y)
            )
            it('can find L.E. for uniform acceleration', () => {
                e.withSymbols('m g x() y() U()', (m, g, x, y, U) => {
                    expect(pe(e.LagrangeEquations(LUniformAcceleration(m, g))(e.up(x, y))(t))).to.equal(
                        'down(m D²x(t), g m + m D²y(t))')
                })
            })
            const LCentralRectangular = (m, U) => ([t, q, v]) => e.sub(
                e.mul(1/2, m, e.square(v)),
                U(e.sqrt(e.square(q)))
            )
            it('can find L.E. for central rectangular coordinates', () => {
                e.withSymbols('m x() y() U()', (m, x, y, U) => {
                    expect(pe(e.LagrangeEquations(LCentralRectangular(m, U))(e.up(x, y))(t))).to.equal(
                        'down((m D²x(t) sqrt((x(t))² + (y(t))²) + x(t) DU(sqrt((x(t))² + (y(t))²))) / sqrt((x(t))² + (y(t))²), (m D²y(t) sqrt((x(t))² + (y(t))²) + y(t) DU(sqrt((x(t))² + (y(t))²))) / sqrt((x(t))² + (y(t))²))')
                })
            })
        })
    })
})
