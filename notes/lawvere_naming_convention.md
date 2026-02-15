# Lawvere Naming Convention Guide

## For Future Development of the Deferential Realism Codebase

---

## 1. Principles

### 1.1 Supplement, Never Replace

Categorical names annotate existing predicates via comments. They do not replace domain or code names. The predicate is `false_natural_law/2`, not `naturality_failure_witness/2`. The categorical comment explains *what it is* in formal terms; the predicate name says *what it does* in domain terms.

Rationale: The codebase is used by domain experts (political economists, social theorists) who understand "false natural law" but not "naturality failure witness." Categorical annotations are for mathematicians reading the code, not for changing the interface.

### 1.2 Rigor Gate

Only use categorical vocabulary when the correspondence is **STRICT** or **STRUCTURAL**. Never annotate a predicate with categorical language when the mapping is **LOOSE**. Specifically:

- DO annotate `cross_index_coupling/2` as "naturality square test" — this IS a naturality condition
- DO annotate `fpn_iterate/5` as "greatest fixed point computation" — it IS computing a fixed point
- DO NOT annotate `composition_rule/3` as "lattice meet" — it is NOT a lattice meet
- DO NOT annotate `resolve_modal_signature_conflict/3` as "meet operator" — it is NOT a meet

When in doubt, use the rigor classification from `lawvere_implementation_notes.md`.

### 1.3 One Line, One Comment

Categorical annotations are single-line `%` comments (not `%%`), placed immediately before the predicate's existing doc comment block. They follow the format:

```prolog
% Categorical: [concept] — [description]
```

This keeps the annotation visible but unobtrusive, and avoids interaction with SWI-Prolog's pldoc system (which treats `%%` as documentation markers).

---

## 2. Six Naming Patterns

### 2.1 Site-Indexed Predicates

Pattern: `pred(Constraint, Context, Result)`

The first two arguments are the constraint (object being classified) and the context (point of the site). The result is the presheaf value at that stalk.

```prolog
% Categorical: Presheaf evaluation — computes type at a point of the site
dr_type(C, Context, Type)

% Categorical: Distribution on Omega — probability measure at a site point
maxent_distribution(C, Context, Distribution)
```

Future additions should follow this pattern. If a predicate evaluates something at a context, it should take `(C, Context, Result)` in that order.

### 2.2 Naturality Tests

Pattern: `test(Constraint, Result)` where Result is `compliant(Score) | non_compliant(Score, Threshold) | inconclusive(Reason)`

```prolog
% Categorical: Naturality condition [STRICT]
boltzmann_compliant(C, Result)
```

The three-valued result (compliant/non_compliant/inconclusive) is the correct categorical pattern: naturality either holds, fails, or cannot be tested. Future naturality tests should use this same result structure.

### 2.3 Override Resolution

Pattern: `resolve_X_with_Y(Input1, Input2, Output)`

The two inputs represent competing signals; the output is the resolved value. This is NOT a lattice meet — it is a priority-based dispatch.

```prolog
resolve_modal_signature_conflict(MetricType, Signature, ResolvedType)
```

Future override mechanisms should follow this pattern. The name should say "resolve" (not "meet" or "join"), and the doc comment should explain the priority ordering.

### 2.4 Distribution Computations

Pattern: `dist(Constraint, Context, Distribution)`

Distributions are lists of `Type-Probability` pairs summing to 1.0.

```prolog
maxent_distribution(C, Context, Dist)
```

Future probabilistic classifiers should follow this pattern and use log-sum-exp normalization.

### 2.5 Fixed-Point Iterations

Pattern: `fpn_*(args)`

The `fpn_` prefix is reserved for the fixed-point network subsystem. The naming convention within FPN is:

- `fpn_run/2` — main entry point
- `fpn_ep/3` — state variable (effective purity)
- `fpn_iterate/5` — iteration loop
- `fpn_compute_ep/3` — per-node computation
- `fpn_intrinsic/2` — initial state

Future fixed-point computations should use a similar prefix convention and follow the Jacobi-style pattern (compute all new values from old state, batch update, check convergence).

### 2.6 Trajectory and Isomorphism

Pattern: `structural_*(C1, C2, Level, Evidence)`

Isomorphism tests take two constraints, return a level (strict/trajectory/family/none) and structured evidence.

```prolog
structural_isomorphism(C1, C2, Level, Evidence)
structural_family(C, FamilyID)
```

Future equivalence relation predicates should follow this pattern, with explicit evidence terms and hierarchical level classifications.

---

## 3. Comment Annotation Format

### 3.1 Single-Line Format

```prolog
% Categorical: [concept] [rigor] — [description]
% existing doc comment...
predicate(Args) :-
```

- Use `%` (single), not `%%`
- Place immediately before the existing doc comment block
- Include the rigor level in brackets when the mapping is not obvious
- Keep to one line

### 3.2 Examples

```prolog
% Categorical: Naturality square test [STRICT] — checks commutativity on Power x Scope grid
%% cross_index_coupling(+Constraint, -CouplingScore)
%  Computes coupling score...
cross_index_coupling(C, CouplingScore) :-
```

```prolog
% Categorical: Greatest fixed point computation — Jacobi iteration converging to equilibrium
%% fpn_iterate(+Constraints, +Context, +K, +MaxIter, +Eps)
%  Recursive iteration loop...
fpn_iterate(Constraints, Context, K, MaxIter, Eps) :-
```

### 3.3 When NOT to Add Annotations

- Do not annotate helper predicates (internal utilities, list processing, formatting)
- Do not annotate backward-compatibility wrappers (`dr_type/2`, `effective_purity/3`)
- Do not annotate predicates where the categorical mapping is LOOSE
- Do not annotate configuration or cleanup predicates

---

## 4. Anti-Patterns

### 4.1 Do Not Rename Predicates After Categorical Concepts

Wrong:
```prolog
naturality_failure_witness(C, Evidence) :-
    false_natural_law(C, Evidence).
```

Right: Keep `false_natural_law/2` and add a categorical comment.

Rationale: Domain names are for domain users. Categorical names are for mathematicians reading the code. These are different audiences served by different mechanisms (predicate names vs. comments).

### 4.2 Do Not Use "Functor" in Prolog Code

The word "functor" in Prolog means the name of a compound term (e.g., `f` in `f(a, b)`). Using it in the categorical sense creates confusion. Write "presheaf" or "classification map" instead.

### 4.3 Do Not Create Categorical Wrapper Modules

Wrong:
```prolog
:- module(categorical_interface, [
    presheaf_evaluation/3,    % wraps dr_type/3
    naturality_test/2,        % wraps boltzmann_compliant/2
    ...
]).
```

Right: Add categorical comments to existing predicates. No new modules.

Rationale: Wrapper modules create maintenance burden, indirection, and the illusion that the categorical structure is a separate layer. It isn't — the categorical structure IS the existing code.

### 4.4 Do Not Impose Structure the Code Doesn't Have

Wrong:
```prolog
% Categorical: Heyting algebra meet — lattice operation on type space
composition_rule(mountain, X, mountain).
```

Right:
```prolog
% Categorical: Binary operation on type space — NOT a lattice meet (two absorbing elements)
composition_rule(mountain, X, mountain).
```

If the code doesn't implement the categorical structure, say so. Honest annotations are more valuable than flattering ones.

### 4.5 Do Not Add Categorical Assertions or Checks

Wrong:
```prolog
:- assert(is_heyting_algebra(type_space)).  % NOT TRUE
```

The categorical annotations are comments only. They do not affect execution. Never add runtime assertions about categorical properties.

---

## 5. Ten Concrete Examples

### 5.1 Existing Names — How They Would Have Been Named Under Convention

| # | Existing Name | Convention Name | Reason We Keep Existing | Comment Added |
|---|---------------|-----------------|------------------------|---------------|
| 1 | `false_natural_law/2` | `naturality_failure_witness/2` | Domain users understand "false natural law" | Yes: naturality failure witness [STRICT] |
| 2 | `effective_purity/4` | `contamination_adjusted_purity/4` | Clearer domain term, but existing name is entrenched | Yes: contravariant purity propagation |
| 3 | `zone_migration` (FPN) | `purity_zone_shift` | Matches the actual computation better | No comment needed (internal term) |
| 4 | `coupling_invariant_rope/2` | `naturality_certificate/2` | Domain users know "CI Rope" | Yes: naturality certificate [STRICT] |
| 5 | `cross_index_coupling/2` | `factorization_deviation/2` | "Coupling" is already well-understood in the codebase | Yes: naturality square test [STRICT] |
| 6 | `gauge_orbit/2` | `context_orbit/2` | Gauge theory vocabulary is productive and precise | Yes: orbit under site automorphisms |
| 7 | `constraint_trajectory/3` | `transformation_profile/3` | "Trajectory" is intuitive and well-established | Yes: natural transformation representative |
| 8 | `structural_family/2` | `isomorphism_class/2` | "Family" is less intimidating than "isomorphism class" | No: STRUCTURAL only |
| 9 | `maxent_distribution/3` | `type_probability/3` | "MaxEnt" identifies the method, which matters for diagnostics | Yes: distribution on Omega |
| 10 | `drift_event/3` | `temporal_degradation_signal/3` | "Drift" is concise and well-understood | No: too granular for annotation |

### 5.2 Future Additions — How They Should Be Named

| # | Hypothetical Predicate | Pattern | Comment |
|---|----------------------|---------|---------|
| 1 | `sheaf_gluing_test/3` | Site-indexed | `% Categorical: Gluing axiom verification — tests whether local sections extend to global` |
| 2 | `heyting_implication/3` | New algebraic structure | `% Categorical: Heyting implication — defined only if type space is formalized as lattice` |
| 3 | `adjunction_unit/3` | New formal structure | `% Categorical: Unit of sigmoid-forgetful adjunction — η: Id → GF` |
| 4 | `giry_lift/3` | Distribution | `% Categorical: Giry monad action — lifts deterministic classification to probabilistic` |
| 5 | `counit_test/3` | Naturality test pattern | `% Categorical: Counit verification — tests ε: FG → Id triangle identity` |

These are hypothetical. None should be created unless the corresponding categorical structure has been formally verified (STRICT rigor).
