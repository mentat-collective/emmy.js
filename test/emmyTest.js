const e = require('../build/emmy-library.js')
const assert = require('assert').strict
const {Assertion, expect} = require('chai')

/**
 * Add a chai predicate, emmyEqual, which is true when the
 * objects are equal in the Clojure sense of `=`.
 */
Assertion.addMethod('emmyEqual', function (t, expected) {
    let obj = t._obj
    assert(e.eq(obj, expected),
        `expected ${obj} and ${expected} to be equal in the Emmy sense`,
        `expected ${obj} and ${expected} not to be equal in the Emmy sense`,
        expected)
})

describe('Emmy', () => {
    /**
     * pe - a `print-expression` for the unit test cases
     * @param expr
     * @returns the infix form of the simplified result.
     *
     * We regard the infix output as being the preferred form
     * in the JavaScript case.
     */
    const pe = expr => e.to_infix(e.simplify(expr))
    /**
     * time is the Lagrangian time selector for local tuples. It merely
     * extracts the initial entry.
     */
    const time = x => e.nth(x, 0)

    describe('works', () => {
        e.with_symbols('x y', (x, y) => {
            it('can add x and 1', () => {
                expect(e.add(x, 1).toString()).to.equal("(+ x 1)")
            })
            it('can add 2 and y', () => { expect(e.add(2, y).toString()).to.equal("(+ 2 y)") })
            it('can add x and y', () => { expect(e.add(x, y).toString()).to.equal("(+ x y)") })
            it('can add several things', () => expect(e.add(x, y, x, y, x).toString()).to.equal("(+ x y x y x)"))
        })
    })
    describe('literal functions', () => {
        const f = e.literal_function('f')
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
            it('can destructure into let', () => {
                const [a, b, c, d] = u
                expect(a).to.equal(4)
                expect(b).to.equal(5)
                expect(c).to.equal(6)
                expect(d).to.equal(7)
            })
            it('has length', () => {
                expect(u.length).to.equal(4)
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
                expect(u.map(x => x + 1)).to.emmyEqual(e.up(5, 6, 7))
            })
            it('in two dimensions', () => {
                const u = e.down(e.up(1, 2), e.up(3, 4))
                expect(u.map(x => x * x)).to.emmyEqual(
                    e.down(e.up(1, 4), e.up(9, 16))
                )
            })
        })
        describe('have expected properties', () => {
            it('up? is true', () => {
                expect(e.up_p(u)).to.be.true
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
                e.with_symbols('m t x v', (m, t, x, v) => T(m)([t, x, v])).toString()
            ).to.equal('(* 0.5 m (expt v 2))');
        })
        it('can apply L to symbolic arguments (3)', () => {
            expect(
                e.with_symbols('m t x v', (m, t, x, v) => L(m)([t, x, v])).toString()
            ).to.equal('(* 0.5 m (expt v 2))');
        })
        let sd = e.Lagrangian_to_state_derivative(L(e.symbol('m')))
        it('can compute state derivative', () => {
            e.with_symbols('t x v', (t, x, v) => {
                expect(sd(e.up(t, x, v))).to.emmyEqual(e.up(1, v, 0))
            })
        })
        it('can compute a partial derivative', () => {
            e.with_symbols('m t x v', (m, t, x, v) => {
                const Lm = L(m)
                const state = e.up(t, x, v)
                expect(e.simplify(e.partial(0)(Lm)(state))).to.equal(0)
                expect(e.simplify(e.partial(1)(Lm)(state))).to.equal(0)
                expect(e.simplify(e.partial(2)(L(m))(state)).toString()).to.equal('(* m v)')
            })
        })
        it('can compute Lagrange equations', () => {
            const Lm = L(e.symbol('m'))
            const x = e.literal_function(e.symbol('x'))
            expect(
                e.simplify(
                    e.Lagrange_equations(Lm)(x)(e.symbol('t'))
                ).toString(
                )).to.equal('(* m (((expt D 2) x) t))')
        })
    })
    describe("SICM chapter 1", () => {
        const LFreeParticle = mass => local => {
            const v = e.velocity(local)
            return e.mul(1 / 2, mass, e.dot_product(v, v))
        }
        const LHarmonic = (m, k) => ([t, q, v]) => e.sub(
            e.mul(1 / 2, m, e.square(v)),
            e.mul(1 / 2, k, e.square(q))
        )
        const LKeplerCentralPolar = (m, V) => ([t, [r, phi], [rdot, phidot]]) => e.sub(
            e.mul(1 / 2, m, e.add(e.square(rdot), e.square(e.mul(r, phidot)))),
            V(r)
        )
        const [m, t, k] = ['m', 't', 'k'].map(x => e.symbol(x))
        const FtoC = F => local => e.up(
            time(local),
            F(local),
            e.add(e.partial(0)(F)(local),
                e.mul(e.partial(1)(F)(local),
                    e.velocity(local))))
        const PtoR = ([t, [r, phi], dot]) => e.up(e.mul(r, e.cos(phi)), e.mul(r, e.sin(phi)))

        describe('1.4 Computing Actions', () => {
            const q = e.up(
                e.literal_function('x'),
                e.literal_function('y'),
                e.literal_function('z'))

            it('computes Γ(q)(t)', () => {
                expect(pe(e.Gamma(q)(t))).to.equal(
                    'up(t, up(x(t), y(t), z(t)), up(Dx(t), Dy(t), Dz(t)))')
            })
            it('can compose LFreeParticle with Gamma', () => {
                expect(pe(e.compose(LFreeParticle(m), e.Gamma(q))(t))).to.equal(
                    '0.5 m (Dx(t))² + 0.5 m (Dy(t))² + 0.5 m (Dz(t))²'
                )
            })
            function LagrangianAction(L, q, t1, t2) {
                return e.definite_integral(e.compose(L, e.Gamma(q)), t1, t2)
            }
            const testPath = t => e.up(
                e.add(e.mul(4, t), 7),
                e.add(e.mul(3, t), 5),
                e.add(e.mul(2, t), 1)
            )
            expect(LagrangianAction(LFreeParticle(3.0), testPath, 0, 10.0)).to.equal(435)
            const makeEta = (nu, t1, t2) => t => e.mul(e.sub(t, t1), e.sub(t, t2), nu(t))
            const variedFreeParticleAction = (mass, q, nu, t1, t2) => eps => {
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

            function parametricPathAction(Lagrangian, t0, q0, t1, q1) {
                return qs => {
                    const path = e.make_path(t0, q0, t1, q1, qs)
                    return LagrangianAction(Lagrangian, path, t0, t1)
                }
            }

            function findPath(Lagrangian, t0, q0, t1, q1, n) {
                const initialQs = e.linear_interpolants(q0, q1, n)
                const minimizingQs = e.multidimensional_minimize(parametricPathAction(Lagrangian, t0, q0, t1, q1), initialQs)
                return e.make_path(t0, q0, t1, q1, minimizingQs)
            }

            it('can find a minimal parametric path', () => {
                const q_harmonic = findPath(LHarmonic(1, 1), 0, 1, (Math.PI / 2), 0, 3)
                expect(q_harmonic(0.8)).to.be.closeTo(Math.cos(0.8), 1e-5)
            })
        })
        describe('1.5 The Euler Lagrange Equations', () => {
            const generalTestPath = t => e.with_symbols('a a_0 b b_0 c c_0', (a, a_0, b, b_0, c, c_0) =>
                e.up(
                    e.add(e.mul(a, t), a_0),
                    e.add(e.mul(b, t), b_0),
                    e.add(e.mul(c, t), c_0)))

            it('can compute L equations for general test path', () => {
                expect(pe(e.Lagrange_equations(LFreeParticle(m))(generalTestPath)(t))).to.equal('down(0, 0, 0)')
            })
            it('can compute L equations with literal function', () => {
                expect(pe(e.Lagrange_equations(LFreeParticle(m))(e.literal_function('x'))(t))).to.equal('m D²x(t)')
            })
            it('can solve given a proposed solution', () => {
                const proposedSolution = e.with_symbols('A omega phi',
                    (A, omega, phi) =>
                        t => e.mul(A, e.cos(e.add(e.mul(omega, t), phi))))

                expect(pe(e.Lagrange_equations(LHarmonic(m, k))(proposedSolution)(t))).to.equal(
                    '- A m ω² cos(ω t + φ) + A k cos(ω t + φ)')
            })
            it('Exercise 1.11: Kepler\'s third law', () => {
                const gravitationalEnergy = (G, m1, m2) => r => e.sub(e.div(e.mul(G, m1, m2), r))
                const circle = t => e.up(e.symbol('a'), e.mul(e.symbol('n'), t))
                const lagrangianReduced = e.with_symbols('G M_1 m_2', (G, M1, m2) =>
                    LKeplerCentralPolar(
                        e.div(e.mul(M1, m2), e.add(M1, m2)),
                        gravitationalEnergy(G, M1, m2)))

                expect(pe(e.Lagrange_equations(lagrangianReduced)(circle)(t))).to.equal(
                    'down((- M₁ a³ m₂ n² + G M₁² m₂ + G M₁ m₂²) / (M₁ a² + a² m₂), 0)')
            })
        })
        describe('1.6 How to Find Lagrangians', () => {
            const LUniformAcceleration = (m, g) => ([t, [x, y], v]) => e.sub(
                e.mul(1 / 2, m, e.square(v)),
                e.mul(m, g, y)
            )
            it('can find L.E. for uniform acceleration', () => {
                e.with_symbols('m g x() y() U()', (m, g, x, y, U) => {
                    expect(pe(e.Lagrange_equations(LUniformAcceleration(m, g))(e.up(x, y))(t))).to.equal(
                        'down(m D²x(t), g m + m D²y(t))')
                })
            })
            const LCentralRectangular = (m, U) => ([t, q, v]) => e.sub(
                e.mul(1 / 2, m, e.square(v)),
                U(e.sqrt(e.square(q)))
            )
            describe('it can find Lagrange equations for central force', () => {
                it('in rectangular coordinates', () => {
                    e.with_symbols('m x() y() U()', (m, x, y, U) => {
                        expect(pe(e.Lagrange_equations(LCentralRectangular(m, U))(e.up(x, y))(t))).to.equal(
                            'down((m D²x(t) sqrt((x(t))² + (y(t))²) + x(t) DU(sqrt((x(t))² + (y(t))²))) / sqrt((x(t))² + (y(t))²), (m D²y(t) sqrt((x(t))² + (y(t))²) + y(t) DU(sqrt((x(t))² + (y(t))²))) / sqrt((x(t))² + (y(t))²))')
                    })
                })
                it('in polar coordinates', () => {
                    e.with_symbols('m r() phi() U()', (m, r, phi, U) => {
                        expect(pe(e.Lagrange_equations(LKeplerCentralPolar(m, U))(e.up(r, phi))(t))).to.equal(
                            'down(- m r(t) (Dφ(t))² + m D²r(t) + DU(r(t)), m (r(t))² D²φ(t) + 2 m r(t) Dφ(t) Dr(t))'
                        )
                    })
                })
            })
            describe('coordinate transformation', () => {
                const RtoP = ([t, X, dot]) => {
                    const [x, y] = X
                    return e.up(e.sqrt(e.square(X)), e.atan(y, x))
                }
                e.with_symbols('r phi rdot phidot U()', (r, phi, rdot, phidot, U) => {
                    const local = e.up(t, e.up(r, phi), e.up(rdot, phidot))
                    it('can compute polar velocity', () => {
                        expect(pe(e.velocity(FtoC(PtoR)(local)))).to.equal('up(- phidot r sin(φ) + rdot cos(φ), phidot r cos(φ) + rdot sin(φ))')
                    })
                    const LCentralPolar = (m, U) => e.compose(LCentralRectangular(m, U), FtoC(PtoR))
                    it('can compute polar Lagrangian', () => {
                        expect(pe(LCentralPolar(m, U)(local))).to.equal('0.5 m phidot² r² + 0.5 m rdot² - U(r)')
                    })
                })
                describe('coriolis and centrifugal forces', () => {
                    const LFreeRectangular = m => ([t, x, [vx, vy]]) => e.mul(1 / 2, m, e.add(e.square(vx), e.square(vy)))
                    const LFreePolar = m => e.compose(LFreeRectangular(m), FtoC(PtoR))
                    const F = Omega => ([t, [r, theta], v]) => e.up(r, e.add(theta, e.mul(Omega, t)))
                    const LRotatingPolar = (m, Omega) => e.compose(LFreePolar(m), FtoC(F(Omega)))
                    const LRotatingRectangular = (m, Omega) => e.compose(LRotatingPolar(m, Omega), FtoC(RtoP))
                    it('can compute rotating rectangular Lagrangian', () => {
                        e.with_symbols('Omega x_r y_r xdot_r ydot_r', (Omega, x_r, y_r, xdot_r, ydot_r) => {
                            expect(pe(LRotatingRectangular(m, Omega)(e.up(t, e.up(x_r, y_r), e.up(xdot_r, ydot_r))))).to.equal(
                                '0.5 Ω² m x_r² + 0.5 Ω² m y_r² + Ω m x_r ydot_r - Ω m xdot_r y_r + 0.5 m xdot_r² + 0.5 m ydot_r²')
                        })
                    })
                    it('can compute L.E. for rotating rectangular', () => {
                        e.with_symbols('m Omega x_r() y_r()', (m, Omega, x_r, y_r) => {
                            expect(pe(e.Lagrange_equations(LRotatingRectangular(m, Omega))(e.up(x_r, y_r))(t))).to.equal(
                                'down(- Ω² m x_r(t) - 2 Ω m Dy_r(t) + m D²x_r(t), - Ω² m y_r(t) + 2 Ω m Dx_r(t) + m D²y_r(t))'
                            )
                        })
                    }).timeout(6000)
                })
            })
            describe('systems with rigid constraints', () => {
                describe('pendulum driven at the pivot', () => {
                    const TPend = (m, l, g, ys) => ([t, theta, thetadot]) => {
                        const vys = e.D(ys)
                        return e.mul(1 / 2, m, e.add(
                            e.square(e.mul(l, thetadot)),
                            e.square(vys(t)),
                            e.mul(2, l, vys(t), thetadot, e.sin(theta)))
                        )
                    }
                    const VPend = (m, l, g, ys) => ([t, theta, thetadot]) => e.mul(m, g, e.sub(ys(t), e.mul(l, e.cos(theta))))
                    const LPend = e.sub(TPend, VPend)
                    it('can compute L.E.', () => {
                        e.with_symbols('m l g ys() theta()', (m, l, g, ys, theta) => {
                            expect(pe(e.Lagrange_equations(LPend(m, l, g, ys))(theta)(t))).to.equal(
                                'g l m sin(θ(t)) + l² m D²θ(t) + l m sin(θ(t)) D²ys(t)'
                            )
                        })
                    })
                    it('works expressed as a coordinate transformation', () => {
                        const dpCoordinates = (l, y_s) => ([t, theta, thetadot]) => e.up(
                            e.mul(l, e.sin(theta)),
                            e.sub(y_s(t), e.mul(l, e.cos(theta)))
                        )
                        const LPend = (m, l, g, y_s) => e.compose(LUniformAcceleration(m, g), FtoC(dpCoordinates(l, y_s)))
                        e.with_symbols('m l g y_s() theta thetadot', (m, l, g, y_s, theta, thetadot) => {
                            expect(pe(LPend(m, l, g, y_s)(e.up(t, theta, thetadot)))).to.equal(
                                '0.5 l² m thetadot² + l m thetadot sin(θ) Dy_s(t) + g l m cos(θ) - g m y_s(t) + 0.5 m (Dy_s(t))²'
                            )
                        })
                    })
                    describe('evolution of dynamical state', () => {
                        const LagrangianToAcceleration = L => {
                            const P = e.partial(2)(L)
                            const F = e.partial(1)(L)
                            return e.solve_linear(
                                e.partial(2)(P),
                                e.sub(F, e.add(e.partial(0)(P)), e.mul(e.partial(1)(P), e.velocity))
                            )
                        }
                        const Lagrangian_to_state_derivative = L => {
                            const acceleration = LagrangianToAcceleration(L)
                            return state => e.up(1, e.velocity(state), acceleration(state))
                        }
                        const harmonicStateDerivative = (m, k) => Lagrangian_to_state_derivative(LHarmonic(m, k))
                        it('can compute harmonic state derivative', () => {
                            e.with_symbols('m k x y v_x v_y', (m, k, x, y, v_x, v_y) => {
                                expect(pe(harmonicStateDerivative(m, k)(e.up(t, e.up(x, y), e.up(v_x, v_y))))).to.equal(
                                    'up(1, up(v_x, v_y), up(- k x / m, - k y / m))'
                                )
                            })
                        })
                        it('can compute L.E. first order', () => {
                            const qvToStatePath = (q, v) => t => e.up(t, q(t), v(t))
                            const LagrangeEquationsFirstOrder = L => (q, v) => {
                                const statePath = qvToStatePath(q, v)
                                return e.sub(e.D(statePath), e.compose(Lagrangian_to_state_derivative(L), statePath))
                            }
                            e.with_symbols('x() y() v_x() v_y()', (x, y, v_x, v_y) => {
                                expect(pe(LagrangeEquationsFirstOrder(LHarmonic(m, k))(e.up(x, y), e.up(v_x, v_y))(t))).to.equal(
                                    'up(0, up(Dx(t) - v_x(t), Dy(t) - v_y(t)), up((k x(t) + m Dv_x(t)) / m, (k y(t) + m Dv_y(t)) / m))'
                                )
                            })
                        })
                        it('can integrate numerically', () => {
                            const s = e.state_advancer(harmonicStateDerivative, 2, 1)(e.up(1, e.up(1, 2), e.up(3, 4)), 10, 1e-12)
                            const expected = e.up(11, e.up(3.712791664, 5.420620823), e.up(1.614803092, 1.818910372))
                            const d = e.sub(s, expected)
                            expect(Math.max(...e.clj_to_js(e.flatten(d)).map(Math.abs))).to.be.lessThan(1e-7)
                        })
                    })
                    describe('can solve periodically driven pendulum', () => {
                        const periodicDrive = (amplitude, frequency, phase) => t => e.mul(amplitude, e.cos(e.add(e.mul(frequency, t), phase)))
                        const LPeriodicallyDrivenPendulum = (m, l, g, A, omega) => {
                            const ys = periodicDrive(A, omega, 0)
                            return LPend(m, l, g, ys)
                        }
                        it('can find L.E. for driven pendulum', () => {
                            e.with_symbols('l g A omega theta()', (l, g, A, omega, theta) => {
                                expect(pe(e.Lagrange_equations(LPeriodicallyDrivenPendulum(m, l, g, A, omega))(theta)(t))).to.equal(
                                    '- A l m ω² sin(θ(t)) cos(ω t) + g l m sin(θ(t)) + l² m D²θ(t)'
                                )
                            })
                        })
                        it('can find the state derivative', () => {
                            const pendStateDerivative = (m, l, g, A, omega) => e.Lagrangian_to_state_derivative(
                                LPeriodicallyDrivenPendulum(m, l, g, A, omega)
                            )
                            e.with_symbols('l g A omega theta thetadot', (l, g, A, omega, theta, thetadot) => {
                                expect(pe(pendStateDerivative(m, l, g, A, omega)(e.up(t, theta, thetadot)))).to.equal(
                                    'up(1, thetadot, (A ω² sin(θ) cos(ω t) - g sin(θ)) / l)'
                                )
                            })
                        })
                    })
                })
            })
        })
        describe('1.8 conserved quantities', () => {
            describe('1.8.3 central forces in three dimensions', () => {
                const T3Spherical = m => ([t, [r, theta, phi], [rdot, thetadot, phidot]]) => e.mul(
                    1 / 2, m, e.add(e.square(rdot), e.square(e.mul(r, thetadot)), e.square(e.mul(r, e.sin(theta), phidot)))
                )
                const L3Central = (m, Vr) => {
                    const Vs = ([t, [r], dots]) => Vr(r)
                    return e.sub(T3Spherical(m), Vs)
                }
                e.with_symbols('r theta phi rdot thetadot phidot V()', (r, theta, phi, rdot, thetadot, phidot, V) => {
                    const state = e.up(t, e.up(r, theta, phi), e.up(rdot, thetadot, phidot))
                    it('can compute partial 1', () => {
                        expect(pe(e.partial(1)(L3Central(m, V))(state))).to.equal(
                            'down(m phidot² r sin²(θ) + m r thetadot² - DV(r), m phidot² r² cos(θ) sin(θ), 0)'
                        )
                    })
                    it('can compute partial 2', () => {
                        expect(pe(e.partial(2)(L3Central(m, V))(state))).to.equal(
                            'down(m rdot, m r² thetadot, m phidot r² sin²(θ))'
                        )
                    })
                    it('can compute angular momentum about z axis', () => {
                        const angMomZ = m => ([t, xyz, v]) => e.cross_product(xyz, e.mul(m, v))[2]
                        const StoR = ([t, [r, theta, phi], dots]) => e.up(
                            e.mul(r, e.sin(theta), e.cos(phi)),
                            e.mul(r, e.sin(theta), e.sin(phi)),
                            e.mul(r, e.cos(theta))
                        )
                        expect(pe(e.compose(angMomZ(m), FtoC(StoR))(state))).to.equal(
                            'm phidot r² sin²(θ)'
                        )
                    })
                    it('can compute energy for the system', () => {
                        expect(pe(e.Lagrangian_to_energy(L3Central(m, V))(state))).to.equal(
                            '0.5 m phidot² r² sin²(θ) + 0.5 m r² thetadot² + 0.5 m rdot² + V(r)'
                        )
                    })
                })
            })
            describe('1.8.4 the restricted three-body problem', () => {
                const L0 = (m, V) => ([t, q, v]) => e.sub(e.mul(1 / 2, m, e.square(v)), V(t, q))
                const V = (a, GM0, GM1, m) => (t, [x, y]) => {
                    const GMT = e.add(GM0, GM1)
                    const Omega = e.sqrt(e.div(GMT, e.expt(a, 3)))
                    const a0 = e.mul(e.div(GM1, GMT), a)
                    const a1 = e.mul(e.div(GM0, GMT), a)
                    const OmegaT = e.mul(Omega, t)
                    const x0 = e.mul(-1, a0, e.cos(OmegaT))
                    const y0 = e.mul(-1, a0, e.sin(OmegaT))
                    const x1 = e.mul(+1, a1, e.cos(OmegaT))
                    const y1 = e.mul(+1, a1, e.sin(OmegaT))
                    const r0 = e.sqrt(e.add(e.square(e.sub(x, x0)), e.sub(y, y0)))
                    const r1 = e.sqrt(e.add(e.square(e.sub(x, x1)), e.sub(y, y1)))
                    return e.sub(e.add(e.div(e.mul(GM0, m), r0)), e.div(e.mul(GM1, m), r1))
                }
                const LR3B1 = (m, a0, a1, Omega, GM0, GM1) => ([t, q, qdot]) => {
                    const [x, y] = q
                    const [xdot, ydot] = qdot
                    const r0 = e.sqrt(e.add(e.square(e.add(x, a0)), e.square(y)))
                    const r1 = e.sqrt(e.add(e.square(e.sub(x, a1)), e.square(y)))
                    return e.add(
                        e.mul(1 / 2, m, e.square(qdot)),
                        e.mul(1 / 2, m, e.square(Omega), e.square(q)),
                        e.mul(m, Omega, e.sub(e.mul(x, ydot), e.mul(xdot, y))),
                        e.div(e.mul(GM0, m), r0),
                        e.div(e.mul(GM1, m), r1)
                    )
                }
                it('computes the energy', () => {
                    e.with_symbols('a_0 a_1 Omega GM_0 GM_1 x_r y_r v_r^x v_r^y',
                        (a0, a1, Omega, GM0, GM1, xr, yr, vrx, vry) => {
                            const state = e.up(t, e.up(xr, yr), e.up(vrx, vry))
                            expect(pe(e.Lagrangian_to_energy(LR3B1(m, a0, a1, Omega, GM0, GM1))(state))).to.equal(
                                '(- 0.5 Ω² m x_r² sqrt(a₀² a₁² - 2 a₀² a₁ x_r + a₀² x_r² + a₀² y_r² + 2 a₀ a₁² x_r - 4 a₀ a₁ x_r² + 2 a₀ x_r³ + 2 a₀ x_r y_r² + a₁² x_r² + a₁² y_r² - 2 a₁ x_r³ - 2 a₁ x_r y_r² + x_r⁴ + 2 x_r² y_r² + y_r⁴) - 0.5 Ω² m y_r² sqrt(a₀² a₁² - 2 a₀² a₁ x_r + a₀² x_r² + a₀² y_r² + 2 a₀ a₁² x_r - 4 a₀ a₁ x_r² + 2 a₀ x_r³ + 2 a₀ x_r y_r² + a₁² x_r² + a₁² y_r² - 2 a₁ x_r³ - 2 a₁ x_r y_r² + x_r⁴ + 2 x_r² y_r² + y_r⁴) + 0.5 m v_r^x² sqrt(a₀² a₁² - 2 a₀² a₁ x_r + a₀² x_r² + a₀² y_r² + 2 a₀ a₁² x_r - 4 a₀ a₁ x_r² + 2 a₀ x_r³ + 2 a₀ x_r y_r² + a₁² x_r² + a₁² y_r² - 2 a₁ x_r³ - 2 a₁ x_r y_r² + x_r⁴ + 2 x_r² y_r² + y_r⁴) + 0.5 m v_r^y² sqrt(a₀² a₁² - 2 a₀² a₁ x_r + a₀² x_r² + a₀² y_r² + 2 a₀ a₁² x_r - 4 a₀ a₁ x_r² + 2 a₀ x_r³ + 2 a₀ x_r y_r² + a₁² x_r² + a₁² y_r² - 2 a₁ x_r³ - 2 a₁ x_r y_r² + x_r⁴ + 2 x_r² y_r² + y_r⁴) - GM₀ m sqrt(a₁² - 2 a₁ x_r + x_r² + y_r²) - GM₁ m sqrt(a₀² + 2 a₀ x_r + x_r² + y_r²)) / sqrt(a₀² a₁² - 2 a₀² a₁ x_r + a₀² x_r² + a₀² y_r² + 2 a₀ a₁² x_r - 4 a₀ a₁ x_r² + 2 a₀ x_r³ + 2 a₀ x_r y_r² + a₁² x_r² + a₁² y_r² - 2 a₁ x_r³ - 2 a₁ x_r y_r² + x_r⁴ + 2 x_r² y_r² + y_r⁴)'
                            )
                        })
                })
            })
        })
        describe('1.9 abstraction of path functions', () => {
            const GammaBar = fBar => local => fBar(e.osculating_path(local))(time(local))
            const FtoC1 = F => local => {
                const n = local.length
                const fBar = qPrime => {
                    const q = e.compose(F, e.Gamma(qPrime))
                    return e.Gamma(q, n)
                }
                return GammaBar(fBar)(local)
            }
            it('computes the path', () => {
                e.with_symbols('r theta rdot thetadot', (r, theta, rdot, thetadot) => {
                    expect(pe(FtoC1(PtoR)(e.up(t, e.up(r, theta), e.up(rdot, thetadot))))).to.equal(
                        'up(t, up(r cos(θ), r sin(θ)), up(- r thetadot sin(θ) + rdot cos(θ), r thetadot cos(θ) + rdot sin(θ)))'
                    )
                })
            })
            const Dt = F => state => {
                const n = e.count(state)
                // NB: state.length doesn't work here, because the tuple has been computed from
                // inside Emmy, and so hasn't been decorated with the getter feature that would
                // enable that. This is an example of what can go wrong if we don't apply that
                // treatment to every structure, but we have to consider the performance impact
                const DFonPath = q => e.D(e.compose(F, e.Gamma(q, e.sub(n, 1))))
                return GammaBar(DFonPath)(state)
            }
            const EulerLagrangeOperator = L => e.sub(Dt(e.partial(2)(L)), e.partial(1)(L))
            e.with_symbols('x v a', (x, v, a) => {
                it('EL operator can apply to LHarmonic', () => {
                    expect(pe(EulerLagrangeOperator(LHarmonic(m, k))(e.up(t, x, v, a)))).to.equal(
                        'a m + k x'
                    )
                })
            })
            e.with_symbols('x()', (x) => {
                it('EL operator can compose with Gamma', () => {
                    expect(pe(e.compose(EulerLagrangeOperator(LHarmonic(m, k)), e.Gamma(x, 4))(t))).to.equal(
                        'k x(t) + m D²x(t)'
                    )
                })
            })
        })
    })
})
