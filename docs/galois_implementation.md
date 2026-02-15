# Galois Structural Isomorphism in Deferential Realism

## The Coalition–Consensus Duality

**Date:** 2026-02-14
**Prerequisites:** *Noether Implementation*, *Categorical Architecture* (Lawvere), *Grothendieck Framing*
**Status:** Substantially implemented (orbit side); coalition–consensus duality is new analysis of existing data; one new computable invariant identified (splitting degree)

---

## 1. What Galois Theory Is About

Galois theory establishes a bijective correspondence between two lattices: the subgroups of a symmetry group and the intermediate structures fixed by those subgroups. The classical version concerns field extensions: given a polynomial, the Galois group permutes its roots, and the subgroups of that group correspond to intermediate fields between the base field and the splitting field. Larger subgroups fix smaller fields; smaller subgroups fix larger fields. The correspondence is order-reversing — an *antitone* bijection.

The power of Galois theory is not in the orbits alone (which Noether handles) but in the **duality between the group side and the fixed-structure side**. Knowing the lattice of subgroups tells you the lattice of intermediate structures, and vice versa. This duality is what's partially implemented and partially missing in the DR system.

---

## 2. The DR Galois Connection

### 2.1 The Two Lattices

In classical Galois theory, the two lattices are subgroups and intermediate fields. In DR, they are:

**Lattice 1: Observer Coalitions.** A coalition is a subset S ⊆ {U₁, U₂, U₃, U₄} of the four standard contexts. There are 2⁴ = 16 coalitions, partially ordered by inclusion. The full coalition {U₁, U₂, U₃, U₄} is at the top; the empty coalition at the bottom. The non-trivial coalitions of interest are the 14 proper non-empty subsets.

An observer coalition represents a group of observers who pool their information. The question a coalition asks is: "what can we all agree on?"

**Lattice 2: Consensus Classifications.** For a given constraint C and coalition S, the consensus is:

```
Consensus(C, S) = T    if dr_type(C, Ui) = T for all Ui ∈ S
Consensus(C, S) = ⊥    if any two members of S disagree on C's type
```

A consensus classification is "real" in the same sense as a Noether conserved quantity or a Grothendieck global section: it is the type that *every* member of the coalition would independently assign.

### 2.2 The Galois Maps

The two antitone maps that form the Galois connection are:

**Agreement:** Given a type T and constraint C, the agreement set is the largest coalition that sees C as T:

```
Agreement(C, T) = { Ui : dr_type(C, Ui) = T }
```

**Consensus:** Given a coalition S and constraint C, the consensus is the type (if any) that all members agree on (defined above).

These are antitone: a larger coalition has a harder time reaching consensus (Consensus is antitone in S), and demanding agreement on more types shrinks the agreement set (Agreement is antitone if extended to sets of types).

The closure operators are:

```
S ↦ Agreement(C, Consensus(C, S))    — the maximal coalition that agrees on what S agrees on
T ↦ Consensus(C, Agreement(C, T))    — the type agreed by everyone who agrees on T
```

A coalition S is **Galois-closed** if S = Agreement(C, Consensus(C, S)). A type T is **Galois-closed** if T = Consensus(C, Agreement(C, T)).

**Rigor: STRICT.** This is a standard Galois connection between two posets. The closure operators satisfy the defining properties (extensive, monotone, idempotent).

### 2.3 What the Galois Connection Reveals Per Constraint

For a constraint with orbit [snare, snare, rope, snare]:

| Coalition | Consensus | Galois-Closed? |
|---|---|---|
| {U₁} | snare | No → closes to {U₁, U₂, U₄} |
| {U₂} | snare | No → closes to {U₁, U₂, U₄} |
| {U₃} | rope | Yes (only U₃ sees rope) |
| {U₄} | snare | No → closes to {U₁, U₂, U₄} |
| {U₁, U₂} | snare | No → closes to {U₁, U₂, U₄} |
| {U₁, U₃} | ⊥ | — (no consensus) |
| {U₁, U₂, U₃} | ⊥ | — |
| {U₁, U₂, U₄} | snare | **Yes** — maximal snare coalition |
| {U₁, U₂, U₃, U₄} | ⊥ | — (descent fails) |

The Galois-closed coalitions are: {U₃} and {U₁, U₂, U₄}. These are the *structurally meaningful* observer groups — the ones where adding or removing a member would change the consensus. The institutional observer (U₃) is structurally isolated: it alone sees a rope. The other three form a closed bloc that agrees on snare.

For a constraint with orbit [rope, rope, rope, rope]:

Every coalition has consensus = rope. The only Galois-closed coalition is the full set {U₁, U₂, U₃, U₄}. There is no structural division among observers. This is the Galois expression of descent: the Galois lattice collapses to a single point.

For a constraint with orbit [indexically_opaque, tangled_rope, rope, snare] (H¹ = 6):

Every singleton coalition is Galois-closed (each observer sees a unique type). The Galois lattice has 4 atoms and no non-trivial joins with consensus — the observers are maximally fragmented. No two observers agree on anything.

### 2.4 The Galois Lattice as a Per-Constraint Invariant

For each constraint, the Galois-closed coalitions form a lattice. This lattice captures the *structure of observer agreement* — not just whether observers disagree (H¹ measures that) but *which groups of observers form natural consensus blocs*.

Two constraints can have the same H¹ but different Galois lattices. Consider:

- Orbit [snare, snare, rope, snare]: H¹ = 3. Galois lattice = {U₃}, {U₁,U₂,U₄}. One dissenter.
- Orbit [rope, snare, snare, rope]: H¹ = 4 (hypothetical). Galois lattice = {U₁,U₄}, {U₂,U₃}. Symmetric split.

The H¹ value is 3 vs 4 — a quantitative difference. But the Galois lattice reveals a qualitative difference: the first has one isolated dissenter against a bloc; the second has two equal-sized blocs. These represent structurally different *politics* of disagreement.

**Rigor: STRICT.** The Galois lattice per constraint is a well-defined finite lattice computed from the shift vector.

---

## 3. What's Already Implemented

The **orbit side** of the Galois structure is thoroughly implemented across three modules:

### 3.1 Dirac Classification (Orbit Computation)

| Predicate | Galois Role | Status |
|---|---|---|
| `gauge_orbit/2` | Computes the orbit = the set of types in the image of the presheaf | ✓ Implemented |
| `preserved_under_context_shift/2` | Tests if orbit is trivial (Galois lattice collapses) | ✓ Implemented |
| `dirac_class/3` | Classifies orbit structure (first-class / second-class / mixed) | ✓ Implemented |
| `gauge_freedom/3` | Measures orbit size (degree of symmetry breaking) | ✓ Implemented |
| `gauge_fixed/3` | Identifies contexts where classification is locally but not globally stable | ✓ Implemented |

### 3.2 Logical Fingerprints (Shift Patterns)

| Predicate | Galois Role | Status |
|---|---|---|
| `fingerprint_shift/2` | The **ordered** type profile [T₁, T₂, T₃, T₄] — finer than orbit (preserves which context sees what) | ✓ Implemented |
| `shift_family/2` | Groups constraints by identical shift = identical Galois lattice | ✓ Implemented |

The shift is the critical piece: two constraints with the same orbit but different shifts have different Galois lattices. The shift tells you not just *what types appear* but *who sees what*.

### 3.3 Trajectory Mining (Structural Isomorphism)

| Predicate | Galois Role | Status |
|---|---|---|
| `structural_isomorphism/4` | Tests whether two constraints have equivalent transformation behavior | ✓ Implemented |
| `structural_family/2` | Equivalence classes — constraints with similar Galois + metric structure | ✓ Implemented |
| `cross_domain_twins/3` | Same structural family, different domains — the Galois correspondence applied | ✓ Implemented |
| `trajectory_distance/4` | Metric on transformation behavior (quantifies "how different" two Galois structures are) | ✓ Implemented |

### 3.4 What the Orbit Side Gives You

The orbit side answers: "How does this constraint transform under context shifts?" The shift pattern gives a complete answer for the discrete type level. Trajectory mining enriches this with continuous metrics, producing structural families that are effectively Galois equivalence classes.

The cross-domain twin detection IS the Galois correspondence at work: two constraints from unrelated domains belong to the same equivalence class because they have the same transformation structure under the observer group. This is the fundamental Galois insight — structural identity is determined by symmetry behavior, not by internal properties.

---

## 4. What's Not Implemented: The Coalition Side

The orbit side asks: "given a constraint, how does it look from each observer?" The coalition side asks the converse: "given a group of observers, what can they agree on?" This is the other arm of the Galois connection, and it is not currently computed.

### 4.1 Coalition Consensus Computation

For each constraint C and each of the 16 coalitions S ⊆ {U₁, U₂, U₃, U₄}:

```prolog
coalition_consensus(C, Coalition, Consensus) :-
    maplist(dr_type_at_context(C), Coalition, Types),
    (   all_equal(Types, T)
    ->  Consensus = agreed(T)
    ;   Consensus = disagreed
    ).
```

This is trivially computable from existing `dr_type/3` data. For each constraint, it produces a 16-entry table of consensus/disagreement values. The Galois-closed coalitions are those where the closure operator fixes the coalition.

**Cost:** 16 × 1023 = ~16K evaluations, each a lookup. Negligible.

**Value:** The coalition consensus table reveals the *structure of observer politics* per constraint — which groups form natural blocs, which observers are isolated dissenters, and whether disagreement is symmetric or asymmetric.

### 4.2 Splitting Degree

The **splitting degree** of a constraint is the size of the minimal coalition that fully determines the constraint's shift pattern. Formally:

```
splitting_degree(C) = min { |S| : for all Ui, dr_type(C, Ui) is determined by 
                                   the types at S }
```

For a 4-point site with types drawn from a finite set, the shift pattern is the ordered tuple [T₁, T₂, T₃, T₄]. The splitting degree is the number of positions you need to know to reconstruct the full tuple.

This depends on the *actual distribution* of shift patterns in the corpus. If 95% of constraints with T₁ = snare have shift [snare, snare, rope, snare], then knowing T₁ = snare essentially determines the full shift. The splitting degree measures how *redundant* the observers are for a given constraint.

| Shift Pattern | Splitting Degree | Interpretation |
|---|---|---|
| [rope, rope, rope, rope] | 1 | Any single observer suffices |
| [snare, snare, rope, snare] | 2 | Need one from the snare-bloc and U₃ |
| [indexically_opaque, tangled_rope, rope, snare] | 4 | Every observer contributes unique information |

The splitting degree is a Galois invariant not captured by H¹, orbit family, or trajectory distance. It measures **observer redundancy** — how much of the perspectival structure is redundant given partial observation.

**Relationship to H¹:** Splitting degree and H¹ are related but not identical. H¹ counts disagreeing pairs (a function of the orbit type-set). Splitting degree counts the minimum observers needed to determine the shift (a function of the shift pattern and its frequency in the corpus). A constraint can have H¹ = 3 with splitting degree 1 (if the shift is so common that one observation predicts the rest) or splitting degree 2 (if the shift is ambiguous from a single observation).

**Cost:** Computing splitting degree requires knowing the shift-pattern distribution, which `shift_family/2` already provides. The computation is combinatorial (check all subsets of {U₁,U₂,U₃,U₄} for sufficiency) but small (15 non-empty subsets × 36 shift patterns).

### 4.3 The Observer Lattice

The Galois-closed coalitions for all constraints, aggregated across the corpus, reveal a **corpus-level observer lattice**: which observer groupings are structurally meaningful across the entire constraint space.

If the coalition {U₁, U₂, U₄} (powerless + moderate + analytical) is Galois-closed for 315 constraints (the {rope, snare} orbit family — where institutional is the lone dissenter), this tells you that the institutional observer has a structurally distinct perspective on roughly 30% of the corpus. The institutional observer is the one who sees coordination mechanisms (ropes) where everyone else sees extraction (snares).

From the corpus data:

| Orbit Family | Size | Isolated Observer | Coalition Bloc |
|---|---|---|---|
| {rope, snare} (shift: snare,snare,rope,snare) | 315 | U₃ (institutional) | {U₁, U₂, U₄} |
| {rope, tangled_rope} (shift: tangled_rope,tangled_rope,rope,tangled_rope) | 135 | U₃ (institutional) | {U₁, U₂, U₄} |
| {rope, snare, tangled_rope} | 164 | varies | varies |
| {mountain, unknown} | 50 | U₂, U₃ (moderate, institutional) | {U₁, U₄} |

**The dominant pattern:** The institutional observer (U₃) is the most frequent dissenter. In the largest orbit families, it's the institutional observer — with generational time horizon and arbitrage exit options — who sees a rope where others see a snare. This is the Galois expression of a central DR claim: institutional power changes what you see, and the change is systematic.

**Rigor: STRICT for the lattice computation. STRUCTURAL for the corpus-level aggregation (the observer lattice is empirical, not derived from the site structure alone).**

---

## 5. The Orbit-Stabilizer Relationship

The orbit-stabilizer theorem states: for a group G acting on a set X, |Orbit(x)| × |Stabilizer(x)| = |G|.

In DR, the "group" acting on the type space via the presheaf evaluation is not a group in the strict sense — context shifts don't compose as permutations of the type space because the presheaf is not equivariant under arbitrary context permutations. However, a weaker version holds:

For each constraint C, define:
- **Orbit size** = |{dr_type(C, Ui) : i = 1..4}| = number of distinct types in the gauge orbit
- **Agreement multiplicity** = the partition of {U₁, U₂, U₃, U₄} into agreement classes (contexts that assign the same type)

The orbit size and the agreement partition are complementary: more distinct types in the orbit → finer agreement partition → smaller agreement classes. This is the orbit-stabilizer relationship in the presheaf setting.

| Orbit Size | Agreement Partition Structure | H¹ | Count | Example |
|---|---|---|---|---|
| 1 | {U₁,U₂,U₃,U₄} (one bloc) | 0 | 212 | [rope, rope, rope, rope] |
| 2 | 3+1 or 2+2 | 3 or 4 | 567 | [snare, snare, rope, snare] |
| 3 | 2+1+1 | 5 | 240 | [tangled_rope, snare, rope, tangled_rope] |
| 4 | 1+1+1+1 (four singletons) | 6 | 4 | [opaque, tangled, rope, snare] |

The orbit size fully determines the *range* of possible agreement partition structures. Orbit size 2 allows either a 3+1 split (one dissenter) or a 2+2 split (symmetric disagreement). The actual partition structure — which is recorded by the shift pattern — determines the Galois lattice. The orbit family records only the type-set; the shift records the full partition structure.

**This is where Galois adds resolution beyond both Noether and Grothendieck.** Noether detects symmetry breaking (orbit size > 1). Grothendieck counts the cost of breaking (H¹). Galois reveals the *coalition structure* of breaking (who agrees with whom).

---

## 6. Relationship to the Existing Framework Stack

### 6.1 Galois Refines Grothendieck

The Grothendieck computation established that H¹ is constant within orbit families. Galois reveals that **shift patterns refine orbit families**, and the coalition structure varies within a fixed orbit family.

Two constraints in the {rope, snare} orbit family both have H¹ = 3. But one might have shift [snare, snare, rope, snare] (institutional dissenter) while another has [snare, rope, snare, snare] (moderate dissenter). Same orbit, same H¹, different coalition structure, different politics.

The fingerprint_shift/2 predicate already computes this. What it lacks is the Galois interpretation: the shift pattern encodes which observer coalitions are structurally meaningful, not just which types appear where.

### 6.2 Galois Extends Noether

Noether's framework identifies conservation (orbit = singleton) and symmetry breaking (orbit > singleton). Galois asks: *which subgroup of the symmetry group is preserved?* A constraint that changes type only at the institutional context has broken the full symmetry but preserved the {powerless, moderate, analytical} subgroup symmetry. The stabilizer subgroup tells you which observer-shifts are "free" (type doesn't change) and which are "constrained" (type changes).

### 6.3 Galois Grounds the Trajectory Metric

The trajectory mining distance metric has a shift component (weight 0.35) that measures how similarly two constraints' types are distributed across contexts. This IS a Galois distance — it measures how similar two constraints' agreement partitions are. Constraints in the same shift family have shift distance 0; constraints in different shift families have positive shift distance proportional to how different their agreement partitions are.

The `type_distance/3` lookup table inside the shift distance component implicitly encodes the Galois structure: type pairs that are "close" (rope ↔ tangled_rope) contribute less distance than pairs that are "far" (mountain ↔ snare), reflecting the lattice distance in the type space.

---

## 7. What New Computation Would Add

### 7.1 Minimal New Module (~80 lines)

A small `galois_analysis.pl` module could compute:

```prolog
% Per-constraint Galois invariants
galois_closed_coalitions(C, ClosedCoalitions).   % List of Galois-closed subsets
splitting_degree(C, Degree).                      % Minimum observers to determine shift
agreement_partition(C, Partition).                % Partition of contexts by agreed type
isolated_observer(C, Context).                    % Singleton Galois-closed coalitions

% Corpus-level aggregates  
observer_dissent_frequency(Context, Frequency).   % How often each context is the dissenter
coalition_stability(Coalition, Fraction).         % Fraction of corpus where coalition achieves consensus
```

**Cost:** All computable from existing `dr_type/3` data. No new classification, no model changes, pure analysis.

**Value:** Modest but real. The splitting degree is a genuinely new invariant. The observer dissent frequency quantifies *which observers are structurally distinctive* — the answer (institutional observer, by a wide margin) is known qualitatively but not currently computed quantitatively. The coalition stability measure would reveal which observer groupings form robust consensus across the corpus.

### 7.2 Whether It's Worth Implementing

The honest assessment: the Galois analysis is computable from existing data and would take ~80 lines of Prolog. The splitting degree and observer dissent frequency are genuine invariants not captured by existing diagnostics. But the value is primarily theoretical (it enriches the mathematical description) rather than diagnostic (it doesn't find constraints that the existing stack misses).

**Recommendation:** Implement only if pursuing the formal publication path. For analytical and creative use, the existing shift patterns and orbit families already capture the Galois structure implicitly. The module would make the implicit explicit — useful for a mathematical audience, unnecessary for a practitioner.

---

## 8. The Three-Way Galois Correspondence

The deepest Galois structure in the system is a three-way correspondence between:

**A. Observer Coalitions** — subsets of {U₁, U₂, U₃, U₄}
**B. Type Partitions** — partitions of the type space Ω into "visible" and "invisible" types for a coalition
**C. Constraint Classes** — subsets of the corpus sharing the same coalition → type behavior

For a fixed coalition S:
- A → B: The coalition sees a restricted set of types (the consensus types achievable by S)
- B → C: The constraints for which S achieves consensus on a given type form a class
- C → A: The maximal coalition that agrees on all members of a constraint class

These three maps compose into the Galois closure. The "Galois group" of a constraint class is the maximal coalition under which all members are simultaneously invariant.

For the descended constraints (H⁰ = 212), the Galois group is the full coalition {U₁, U₂, U₃, U₄}. For the maximally fractured constraints (H¹ = 6), the Galois group is trivial (only singleton coalitions are closed).

**Rigor: STRICT for the formal structure. STRUCTURAL for the empirical content (which specific coalitions and classes arise depends on the corpus, not on the Galois theory).**

---

## 9. Honest Assessment

### What Galois adds that's genuinely new

1. **Coalition structure.** The *who agrees with whom* question is not answered by orbits (which types appear), H¹ (how many pairs disagree), or trajectories (continuous metric similarity). The Galois lattice per constraint captures the observer politics that the other frameworks miss.

2. **Splitting degree.** How many observers do you need to fully characterize a constraint? This is a measure of observer redundancy that has no counterpart in the Noether, Lawvere, or Grothendieck layers.

3. **The institutional observer as dominant dissenter.** The corpus data implies that U₃ (institutional) is the most frequent isolated observer in Galois-closed coalitions. This would be a formal, quantitative version of a central qualitative claim: institutional power is the structurally decisive perspective.

### What Galois does NOT add

1. **New detections.** No constraint is identified by Galois analysis that isn't already identified by the orbit + shift + trajectory stack.

2. **Algorithmic power.** The Galois computation is a rearrangement of existing `dr_type/3` data. It computes a lattice from a lookup table. The computational content is trivial.

3. **Formal Galois group.** The context shifts do not form a group acting on the type space in the strict algebraic sense (the presheaf is not equivariant under context permutations in general). The "Galois group" language is STRUCTURAL, not STRICT. A formal Galois theory would require defining the automorphism group of the presheaf as a categorical object, which is well-defined in principle (automorphisms in PSh(C)) but has not been computed.

### The bottom line

Galois theory provides the *coalition-consensus duality* that sits between Noether (symmetry conservation) and Grothendieck (descent/cohomology). The orbit side is fully implemented. The coalition side is trivially computable but not yet computed. The genuine contribution is the Galois lattice as a per-constraint invariant encoding observer politics — specifically, the identification of the institutional observer as the structurally decisive dissenter.

The formal Galois connection as an adjunction is STRICT. The "Galois group" of the presheaf is STRUCTURAL. The empirical findings about coalition structure are EMPIRICAL.

---

## 10. Rigor Summary

| Claim | Rigor | Notes |
|---|---|---|
| Coalition–consensus Galois connection | STRICT | Standard antitone Galois connection between two finite posets |
| Galois-closed coalitions per constraint | STRICT | Well-defined closure operation |
| Splitting degree as invariant | STRICT | Combinatorially well-defined |
| Orbit-stabilizer analogy | STRUCTURAL | Holds for presheaf evaluation but contexts don't form a group acting on types |
| "Galois group" of the presheaf | STRUCTURAL | Automorphism group in PSh(C) is well-defined but not computed |
| Institutional observer as dominant dissenter | EMPIRICAL | Inferred from orbit family structure; not derived from Galois theory |
| Galois refines Grothendieck's H¹ | STRICT | Shift patterns refine orbit families; coalition structure varies within fixed H¹ |
| Three-way correspondence (coalitions, types, constraint classes) | STRICT | Formal Galois connection on three finite lattices |

---

## 11. Relationship to Other Frameworks

| Framework | What It Measures | What Galois Adds |
|---|---|---|
| **Noether** | Is symmetry conserved? (Yes/No + type of break) | Which *subgroup* of the symmetry is preserved? |
| **Grothendieck** | How much does global truth fail? (H¹ count) | What is the *structure* of the failure? (Coalition lattice) |
| **Lawvere** | What categorical structure does the code implement? (Presheaf, naturality) | The presheaf's agreement partition is a Galois invariant |
| **Dirac** | What is the orbit? (Type-set across contexts) | The shift pattern refines the orbit; same orbit can have different Galois lattices |
| **Trajectory Mining** | How similar are two constraints' transformation behavior? | The shift distance component IS a Galois distance on agreement partitions |

The hierarchy of refinement:

```
Orbit Family (24 families, from gauge_orbit — unordered type sets)
    ↓ refined by
Shift Pattern (36 patterns, from fingerprint_shift — ordered type profiles)
    ↓ refined by
Galois Lattice (per-constraint — which coalitions are structurally closed)
    ↓ refined by
Full Trajectory (per-constraint — continuous metrics at each context)
    ↓ refined by
Structural Family (26 families, from trajectory_mining — HAC clusters)
```

Each level adds resolution. Galois sits between shift patterns and full trajectories: it extracts the combinatorial structure of observer agreement from the shift, without requiring the continuous metrics of the trajectory.
