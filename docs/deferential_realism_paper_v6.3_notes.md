# v6.2 → v6.3 Revision Notes

## What Happened

An FCR (False CI Rope) ablation experiment tested whether the ~62% tangled_rope convergence between Corpus A and Corpus B was driven by the FCR override or reflected structural tendency. The investigation surfaced three findings: a deduplication bug in `known_constraint/1`, a measurement-layer mismatch in the ablation script, and the fact that the corpus is living (now ~1,051 constraints, growing by three per analytical run) meaning corpus-level statistics are snapshots, not fixed points.

The ablation confirmed one important structural result: the H¹ gap (values 1 and 2 empty) survives FCR ablation in both corpora. The FCR redistributes type labels within existing orbit families without altering the presheaf's disagreement topology.

## Changes by Section

---

### Abstract (line 9–15)

**Change:** Replace "Two independently generated corpora with inverted input distributions confirm the engine correctly computes these consequences and converge to identical framework-level outputs" with:

> Two independently generated corpora with inverted input distributions confirm the engine correctly computes these consequences. Structural invariants — the H¹ gap, spectral eigenvalues, contextuality fraction gap, and institutional dissent direction — are identical across both corpora and survive FCR override ablation, confirming they are fixed-point attractors of the axioms. Corpus-dependent statistics (type distributions, descent rates, coalition structure) vary between corpora as expected.

**Rationale:** The original phrasing implied convergence of all outputs. The structural invariants genuinely converge; the type distributions don't, and shouldn't be claimed to.

---

### §4.3 Corpus Provenance (lines 259–269)

**Add after existing text:**

> **Living corpus note.** The corpus is not static. Each analytical run generates new constraint stories that are added to the active testset. The corpus contained 907 (Haiku) and 887 (Flash) constraints when the cross-corpus comparison was first computed; at time of writing it contains approximately 1,051 active constraints. Corpus-level statistics reported in §5 are snapshots at the time of computation. Structural invariants (§5.1, §5.2 invariant table) are stable across corpus growth; corpus-dependent statistics (§5.3) will shift as the corpus evolves.

> **Deduplication note.** A bug in `known_constraint/1` (logical_fingerprint.pl) caused Prolog backtracking to yield each constraint ID multiple times (once per matching `constraint_metric/3`, `constraint_claim/2`, and `constraint_classification/3` fact). This inflated constraint counts by approximately 8–11× in any query iterating over `known_constraint/1` without deduplication. Fixed in v4.2 via `findall/sort` wrapper. All statistics in this paper use deduplicated counts.

---

### §5.2 Convergence Under Inversion (lines 303–337)

**Change the framing.** Replace the opening sentence:

OLD: "The strongest empirical validation: two corpora with opposite input distributions converge to identical framework-level outputs."

NEW: 

> **Structural invariants under inversion.** The strongest empirical validation: structural invariants derived from the axioms are identical across two corpora with opposite input distributions. Corpus-dependent statistics diverge as expected, confirming that the invariants are properties of the framework rather than properties of any particular dataset.

**Reframe the type distribution table (lines 307–315).** Keep the table but change the surrounding text:

OLD: "Both corpora converge to approximately 62% tangled_rope after signature integration..."

NEW:

> The type distributions after signature integration differ between corpora. The structural invariants — eigenvalues, H¹ gap, CF gap, spectral weight — are identical. This separation is the point: what the axioms guarantee is invariant; what the corpus contributes varies.

**Modify the invariant table (lines 319–336).** Remove the row "Post-override tangled_rope rate | ~62% in both" and move it to §5.3 (corpus-dependent findings) with appropriate qualification. The remaining rows are genuinely invariant.

**Add FCR ablation result after the invariant table:**

> **FCR override ablation.** Disabling the FCR override (gating all three intervention points while preserving detection) confirms the structural invariants are independent of the FCR mechanism. Specifically:
>
> The H¹ gap (values 1 and 2 empty) holds in both corpora with FCR disabled. The H¹ distribution is nearly unchanged — the FCR redistributes type labels within existing orbit families without altering the presheaf's disagreement topology. The FCR effect is asymmetric: Corpus B (snare-dominated inputs) loses more tangled_rope classifications than Corpus A (tangled_rope-dominated inputs), consistent with the FCR reclassifying metric-snares that exhibit cross-perspectival coupling.
>
> The ablation confirms that the FCR operates within the structure the axioms create rather than generating that structure. The theorems hold without it; the FCR refines the type assignment within the space the theorems define.

---

### §5.2 "What drives convergence" paragraph (lines 317)

**Replace entirely:**

OLD: "**What drives convergence.** The FCR (False CI Rope) override acts as a fixed-point attractor..."

NEW:

> **Role of the FCR override.** The FCR (False CI Rope) override applies a 3× boost to tangled_rope probability when the Boltzmann independence test detects cross-perspectival coupling alongside extraction. This is a property of the rule cascade's prioritization (Axiom 6), not a direct measurement of social reality. FCR ablation shows the override shifts type distributions (asymmetrically, with greater effect on snare-heavy corpora) without altering structural invariants. Whether the override's reclassifications are substantively correct — whether constraints exhibiting cross-perspectival coupling with extraction genuinely warrant tangled_rope classification — requires metric-level sensitivity analysis (§6.6) and real-world corpus validation.

---

### §6.2 What Is STRICT (line 379)

**Add** to the STRICT list: "FCR ablation invariance of H¹ gap"

---

### §6.5 What the Framework Cannot Do (lines 392–406)

**Modify the "Distinguish framework properties from LLM priors" entry:**

OLD: "The convergence under inversion demonstrates engine stability, not real-world correspondence."

NEW:

> **Distinguish framework properties from LLM priors.** The structural invariants under inversion demonstrate that axiom-derived properties are stable across different LLM-generated corpora. Both corpora inherit whatever latent political grammar their training data shares. Corpus-dependent statistics (type distributions, coalition structure, descent rates) vary between corpora and will shift as the living corpus grows. The invariants that hold are properties of the axioms; the statistics that vary are properties of the data.

---

### §6.6 What Would Strengthen the Framework (lines 407–417)

**Reframe item 3:**

OLD: "Clean corpus without d-pattern anchoring. D-pattern concentration below 30% for proper perspective diversity."

NEW:

> 3. **Corpus diversity.** The corpus is living and grows with each analytical run. D-pattern concentration should be monitored; new constraints generated from analytical use naturally diversify the distribution away from the anchoring artifacts present in the original LLM-generated corpora.

**Add new item 10:**

> 10. **Per-constraint diagnostic walkthrough.** The framework's value is most visible at the individual constraint level (see sample reports). A structured walkthrough of 3–4 constraints — showing the full diagnostic stack, theorem instantiation, omega resolution scenarios, and the contrast between structurally clean constraints (GREEN verdict, gauge-invariant) and coordination-washed constraints (YELLOW verdict, false CI rope signature) — would demonstrate the engine's analytical output more concretely than corpus-level statistics.

**Reframe "no critical parameters" language in §5.1 (line 301):**

OLD: "The framework has no critical parameters in the sensitivity-sweep sense."

NEW:

> No parameter produces distant bifurcation; the closest margin is snare_chi_floor at 0.8% below baseline (14 type-label flips). The asymmetric sensitivity in rope_chi_ceiling — six times more sensitive upward than downward — indicates corpus clustering near the upper boundary of rope classification. All 148 remaining parameters produce zero type-label flips across the [0.5×, 2.0×] sweep range. The one parameter previously flagged as critical (power_modifier_analytical) was a timeout artifact: 37 tests that never ran within the 600-second wall, not 37 classification failures.

---

## Summary of What Changed

| Item | v6.2 | v6.3 |
|------|------|------|
| Convergence claim | "converge to identical framework-level outputs" | Structural invariants identical; corpus-dependent statistics diverge as expected |
| ~62% tangled_rope | In invariant table | Moved to corpus-dependent findings |
| FCR role | "fixed-point attractor" | Shifts type distributions without altering structural invariants; ablation-confirmed |
| Corpus status | Implicitly static | Explicitly living, ~1,051 constraints, growing |
| Deduplication bug | Not documented | Documented and fixed |
| "No critical parameters" | Binary claim | Nuanced: closest margin 0.8%, asymmetric sensitivity documented |
| H¹ gap robustness | Confirmed across corpora | Confirmed across corpora AND across FCR ablation |

---

# Math Suggestions

Two extensions that are implementable in Prolog and useful for the diagnostic purpose, ordered by effort.

## 1. Wasserstein Refinement of H¹ (low effort, high diagnostic value)

**What it is.** H¹ counts disagreeing observer-pairs as a binary: same type or different type. But [tangled_rope, tangled_rope, rope, tangled_rope] (H¹ = 3) and [snare, snare, mountain, snare] (H¹ = 3) have the same H¹ but very different diagnostic meaning — the second represents a much larger classificatory jump.

You already compute MaxEnt distributions at each observer position. The Wasserstein distance (earth-mover's distance) between adjacent observers' distributions would give a continuous measure per edge of the site:

$$W_1(P_i, P_j) = \inf_{\gamma \in \Gamma(P_i, P_j)} \int |x - y| \, d\gamma(x,y)$$

For discrete distributions over 8 types, this reduces to a simple linear program (or, if you put a metric on the type space — which you've already considered — it's just the optimal transport cost between two histograms).

**What it buys you.** A per-edge "perspectival distance" that refines H¹. Two constraints with H¹ = 3 but different Wasserstein profiles are structurally different in a way H¹ can't see. The total transport cost across all edges gives a corpus-level "total perspectival fracture" measure that's more sensitive than the H¹ distribution.

**In the report output**, this would appear as a line in the SCOPE EFFECT ANALYSIS or CLASSIFICATION CONVERGENCE section: "Edge transport: U₁→U₂: 0.02, U₂→U₃: 0.87, U₃→U₄: 0.15" — immediately showing where the classification jump concentrates.

**Implementation.** You need a metric on the type space. The extraction ordering (mountain < rope < tangled_rope < snare) gives one for the in-chain types. Incomparable types (scaffold, piton, naturalized) would need a decision — either a default distance or exclusion. Then the Wasserstein computation for discrete distributions on an ordered space is just the L¹ distance between cumulative distribution functions — no linear programming needed.

**Paper status:** STRICT once implemented. The computation is deterministic and the metric is defined.

## 2. Persistence Barcode on the Bifurcation Sweep (moderate effort, structural insight)

**What it is.** You already run bifurcation sweeps, varying each threshold parameter and recording where type-label flips occur. Right now you report critical values — the parameter setting where classification changes. A persistence diagram tracks not just *where* a disagreement appears but *how long it persists* as the parameter varies.

For each constraint, as you sweep a parameter from 0.5× to 2.0×, its H¹ value may change. Track the birth and death of each H¹ class:

- A constraint goes from H¹ = 0 to H¹ = 3 at parameter value p₁ and back to H¹ = 0 at p₂. That's a persistence interval [p₁, p₂].
- Long bars in the barcode = structurally robust disagreements (persist across wide parameter ranges).
- Short bars = threshold-sensitive disagreements (exist only near a classification boundary).

**What it buys you.** The distinction between "this constraint is genuinely perspectivally fractured" and "this constraint happens to sit near a classification boundary" — which is exactly the question the snare_chi_floor at 0.8% raises. The 14 constraints that flip at snare_chi_floor would show up as short persistence bars; constraints with robust H¹ = 3 or H¹ = 6 would show long bars.

**Implementation.** You already have the sweep infrastructure. The persistence computation is bookkeeping: for each constraint × each parameter, record the H¹ value at each sweep point. The barcode is the set of intervals where H¹ > 0. No new math library needed — it's just interval tracking on existing data.

**In the report output**, this could appear as: "Perspectival persistence: robust (H¹ = 3 survives ±40% parameter variation)" or "Perspectival persistence: fragile (H¹ = 3 depends on snare_chi_floor within 0.8%)."

**Paper status:** STRICT once implemented. It's persistent homology applied to the parameter space, which is well-understood mathematics.

**Connection to existing literature:** This is a standard application of topological data analysis (Edelsbrunner & Harer 2010). The novelty is applying it to the parameter space of a classification presheaf rather than to a point cloud. The barcodes would characterize the "structural stability" of each theorem's predictions — which is exactly what the bifurcation sweep was already trying to do, just without the persistence formalism.

## What I'd skip for now

- The vector-valued Laplacian (already on your roadmap, high effort, changes the spectral analysis significantly — save for v7)
- Non-linear sites / DAG extensions (changes the theorems, not a refinement of existing ones)
- Markov category verification (delete-map naturality — pure math cleanup, doesn't change diagnostic output)
- Bigraded cohomology H^{p,q} (requires temporal site extension, not incremental)

The Wasserstein refinement is an afternoon's work and immediately improves every diagnostic report. The persistence barcode is a weekend's work and answers the robustness question more rigorously than the current sweep.
