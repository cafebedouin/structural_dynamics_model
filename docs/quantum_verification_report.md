
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================
<!-- QUANTUM_VERIFICATION_START -->
# Quantum Complexity Verification Report

## Query 1: MaxEnt Divergence vs H^1 Clustering

**Prediction:** High MaxEnt divergence (>0.05) clusters at H^1 > 0.

### Raw Results

Constraints with divergence > 0.05: **8**

| Constraint | Divergence | H^1 |
|---|---|---|
| artificial_snow_2026 | 0.1949 | 3 |
| clawderberg_recursive_slop | 0.6306 | 3 |
| france_cordon_sanitaire_2026 | 0.0516 | 3 |
| glp1_payload_efficiency_pivot | 0.1803 | 3 |
| moltbook_agent_theater | 0.0634 | 3 |
| notary_ink_dependency | 0.1096 | 3 |
| ship_of_theseus | 0.1379 | 3 |
| ulysses_ithaca_1904 | 0.0650 | 3 |

High-divergence with H^1 > 0: **8/8 (100.0%)**

**Reverse direction:** H^1 > 0 constraints with high divergence: **8/790 (1.0%)**

H^1 distribution of high-divergence constraints:
  H^1=3: 8

### Prediction Outcome

**Confirmed.** 100.0% of high-divergence constraints have H^1 > 0.

**Reverse finding:** Only 1.0% of H^1 > 0 constraints show high MaxEnt divergence. H^1 detects a broader class of observer-dependence than what MaxEnt captures. The cohomological formalism sees structure that the classical oracle misses.

---

## Query 2: Hub-Conflict Constraints and H^1 Bands

**Prediction:** Hub-conflict constraints cluster at H^1 >= 4.

### Type A Conflicts
Classified as mountain but Chi > 0.6600 (snare-level extraction)

Found: **0** constraints

### Type B Conflicts
Mountain at one context, non-mountain at another, with mountain-compatible metrics
(Hub 2 immutability flip is the sole driver of classification change)

Found: **15** constraints

| Constraint | Mountain Context | Other Context | Other Type | H^1 |
|---|---|---|---|---|
| biological_curiosity | powerless | moderate | scaffold | 4 |
| burali_forti_paradox | powerless | moderate | scaffold | 4 |
| click_chemistry_paradigm_2026 | powerless | moderate | scaffold | 4 |
| climate_attribution_2026 | powerless | moderate | scaffold | 4 |
| currys_paradox | powerless | moderate | scaffold | 4 |
| endowment_effect | powerless | moderate | scaffold | 4 |
| ergo_nipopows | powerless | moderate | scaffold | 4 |
| galois_theory_symmetry | powerless | moderate | scaffold | 4 |
| genetic_algorithms_evolution | powerless | moderate | scaffold | 4 |
| heuristic_optimization | powerless | moderate | scaffold | 4 |
| hilberts_hotel_infinity | powerless | moderate | scaffold | 4 |
| information_foraging_theory | powerless | moderate | scaffold | 4 |
| mvt_theorem_constraint | powerless | moderate | scaffold | 4 |
| quine_self_replication | powerless | moderate | rope | 4 |
| skolems_paradox | powerless | moderate | scaffold | 4 |

### Combined Hub-Conflict H^1 Distribution

Total unique hub-conflict constraints: **15**

  H^1=4: 15 constraints

### Prediction Outcome

**Confirmed.** 100.0% of hub-conflict constraints have H^1 >= 4.

---

## Query 3: Restricted-View Divergence vs gauge_fixed

**Prediction:** >= 70% overlap between restricted-view divergence and gauge_fixed.

### Raw Counts

Total (Constraint, Context) pairs evaluated: **4132** (1033 constraints x 4 contexts)

**Restricted classification outcomes by context:**

| Context | Indeterminate | Definite (non-indet) | Differs from full |
|---|---|---|---|
| powerless | 390 | 643 | 274 |
| moderate | 416 | 617 | 197 |
| institutional | 544 | 489 | 363 |
| analytical | 261 | 772 | 76 |

### Overlap Analysis

- Set A (restricted differs from full, non-indeterminate): **910** pairs
- Set B (gauge_fixed = true): **1231** pairs
- A ∩ B: **15** pairs

- Fraction of A that is gauge_fixed: **1.6%** (15/910)
- Fraction of gauge_fixed that differs in restricted: **1.2%** (15/1231)

### Mismatch Analysis

- In A but not B (restricted differs, not gauge_fixed): **895**
  Interpretation: accessibility table is more restrictive than gauge_fixed.
  These are cases where restricted data produces a wrong answer even though
  the full classifier does not identify gauge-dependence.

  Sample (up to 5):
    abstraction_boundary_overrun at institutional: full=rope, restricted=piton
    abstraction_leakage at powerless: full=tangled_rope, restricted=snare
    academic_fashion_modernism_2026 at institutional: full=rope, restricted=piton
    academic_peer_review_gatekeeping at powerless: full=tangled_rope, restricted=snare
    adaptive_lag_trap at institutional: full=rope, restricted=piton

- In B but not A (gauge_fixed, but restricted agrees): **1216**
  Interpretation: gauge_fixed detects frame-dependence that does not
  affect the restricted classification outcome.


### Prediction Outcome

**Disconfirmed.** Bidirectional overlap below 70%: A→B=1.6%, B→A=1.2%.

---

## Query 4: Mountain Stability Under Indexing

**Prediction:** Genuine mountains (H^0, epsilon < 0.05) have divergence < 0.01.

Constraints preserved as mountain (H^0): **114**
Genuine mountains (epsilon < 0.05): **25**

### Results

Mountains with MaxEnt data: **25**
Maximum divergence: **0.0000**
Mean divergence: **0.0000**

No anomalous mountains (all divergence <= 0.02).

### Full Table

| Constraint | Base Eps | Divergence |
|---|---|---|
| banach_fixed_point | 0.0100 | 0.0000 |
| basel_problem_convergence | 0.0200 | 0.0000 |
| c_physical_blue_wavelength | 0.0200 | 0.0000 |
| church_turing_thesis | 0.0200 | 0.0000 |
| clt_convergence_2026 | 0.0100 | 0.0000 |
| collatz_conjecture_determinism | 0.0200 | 0.0000 |
| continuum_hypothesis_undecidability | 0.0200 | 0.0000 |
| cosmological_evolution_alpha_omega | 0.0100 | 0.0000 |
| euler_characteristic_topology | 0.0200 | 0.0000 |
| fgh_hierarchy_2026 | 0.0100 | 0.0000 |
| four_color_theorem_topological_bound | 0.0200 | 0.0000 |
| fundamental_theorem_of_algebra | 0.0100 | 0.0000 |
| hd101584_stellar_evolution | 0.0200 | 0.0000 |
| hilbert_hotel_infinite_capacity | 0.0100 | 0.0000 |
| kleene_recursion_theorem | 0.0200 | 0.0000 |
| lobs_theorem | 0.0200 | 0.0000 |
| material_tensile_strength | 0.0200 | 0.0000 |
| monty_hall_conditional_probability | 0.0200 | 0.0000 |
| poincare_conjecture | 0.0100 | 0.0000 |
| pythagorean_geometric_constancy | 0.0200 | 0.0000 |
| quantum_nonlocality_2026 | 0.0000 | 0.0000 |
| relativity_of_simultaneity | 0.0000 | 0.0000 |
| relativity_physical_invariance | 0.0200 | 0.0000 |
| silver_scarcity_2026 | 0.0400 | 0.0000 |
| square_cube_law | 0.0100 | 0.0000 |

### Prediction Outcome

**Confirmed.** All genuine mountains have divergence < 0.01.


---


---

End of report.
