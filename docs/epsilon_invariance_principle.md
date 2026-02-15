# Epsilon Invariance Principle

## Design Note — Structural Dynamics Model v6.0

**Date:** 2026-02-11
**Status:** Active
**Enforced by:** Linter rule 23 (CONTEXT_ARITY)

---

## 1. The Principle

**Base extractiveness (epsilon) is an intrinsic property of the constraint, not of the observer or the measurement basis.**

If the same constraint yields different epsilon values depending on *what you measure* (eigenvalues vs. eigenvectors, monetary flow vs. information flow, etc.), you do not have one constraint with a measurement parameter — you have two distinct constraints that happen to share a name.

## 2. Decision Rule

When analyzing a candidate constraint story:

1. Identify the observable(s) the constraint operates on.
2. Compute epsilon for each observable independently.
3. **If epsilon is the same across observables:** single constraint story, proceed normally.
4. **If epsilon differs across observables:** decompose into separate constraint stories, one per observable. Link them via `affects_constraint/2`.

The decomposed stories inherit their own:
- Classification type (may differ: one may be Mountain, another Tangled Rope)
- Beneficiary/victim declarations (may differ or be absent)
- Enforcement requirements (may differ)
- Temporal measurements (threshold-dependent per story)

## 3. The BGS Example

The Bohigas-Giannoni-Schmit (BGS) conjecture (1984) originally appeared as a single claim: "quantum systems with chaotic classical limits follow random matrix theory predictions." But this conflates two structurally distinct claims:

| Component | Observable | Epsilon | Classification | Enforcement |
|-----------|-----------|---------|---------------|-------------|
| Spectral universality | Eigenvalue statistics | 0.08 | Mountain | None (nature self-enforces) |
| Eigenvector thermalization | Eigenstate expectation values | 0.42 | Tangled Rope | Peer review norms (ETH orthodoxy) |

**Why the difference matters:**
- Spectral universality is a mathematical regularity verified by direct numerical computation. No agent benefits, no agent is victimized. It simply IS.
- Eigenvector thermalization involves a social enforcement mechanism: the quantum chaos community enforces ETH compliance as a publication gate, creating winners (mainstream) and losers (non-thermal systems researchers).

**Resolution:** Two separate constraint stories linked by network edges:
```prolog
affects_constraint(ehrenfest_barrier, bgs_spectral_universality).
affects_constraint(ehrenfest_barrier, bgs_eigenvector_thermalization).
affects_constraint(bgs_spectral_universality, bgs_eigenvector_thermalization).
```

## 4. The Comitatus Lesson (context/4 Is Not Extensible)

A rejected proposal attempted to handle the BGS split by adding a 5th context axis (`measurement_basis`) to the indexing tuple. This was wrong for two reasons:

1. **Context axes are structural, not parametric.** The four axes (agent_power, time_horizon, exit_options, spatial_scope) characterize the *observer's position*, not the *observable*. Adding `measurement_basis` would make context describe the experiment, not the experimenter.

2. **Precedent: the Comitatus anomaly.** Six early testset files (comitatus_bond, s1_visa, s1_visa_judgment_sharing_agreement, blackstone_carried_interest_taxation, universal_mathematics_communication, coinbase_crypto_volatility) smuggled `constraint_beneficiary` and `constraint_victim` into context tuples, creating context/5 and context/6. This broke test patterns, confused the indexing engine, and violated the separation between perspectival data (context) and structural data (beneficiary/victim). The fix was to extract beneficiary/victim to module-level facts and enforce context/4. The same logic applies to `measurement_basis`.

## 5. Anti-Patterns

| Anti-Pattern | Why It's Wrong | Correct Approach |
|-------------|---------------|-----------------|
| `context(P, T, E, measurement_basis(eigenvalues), S)` | Context describes observer, not observable | Separate constraint stories |
| `context(P, T, E, constraint_beneficiary(...), constraint_victim(...), S)` | Structural data in perspectival tuple | Module-level beneficiary/victim facts |
| Single constraint with epsilon that varies by measurement | Epsilon is intrinsic, not observer-relative | Decompose via affects_constraint/2 |
| `constraint_metric(C, extractiveness, 0.08)` and `constraint_metric(C, extractiveness, 0.42)` in same file | Contradictory epsilon for same constraint | Two files, two constraint IDs |

## 6. Pipeline Protection

**Linter rule 23 (CONTEXT_ARITY)** enforces the structural boundary:

- Scans all `context(` terms inside `constraint_classification` clauses
- Counts top-level arguments by tracking parenthesis depth
- Errors if argument count is not exactly 4
- Skips commented-out lines (starting with `%`)
- Reports the line number of each violation

This prevents future regressions of the Comitatus anomaly and blocks any attempt to add a 5th context axis for measurement basis, beneficiary, victim, or any other non-perspectival data.
