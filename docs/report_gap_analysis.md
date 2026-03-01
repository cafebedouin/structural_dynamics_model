# Gap Analysis: Engine Capabilities vs. Enhanced Report Surfacing

**What the Prolog engine computes (per paper v6.1 §4.2) vs. what the enhanced report generator surfaces for LLM essay writing.**

Methodology: Each diagnostic was traced from paper description → Prolog predicate → `enhanced_report.py` output section → JSON sidecar field. Findings cite code inspection, not documentation claims.

---

## Summary Table

| # | Diagnostic (§4.2) | Computed | Surfaced | LLM-Usable | Notes |
|---|---|---|---|---|---|
| 1 | Gauge orbits | Yes | Yes | Partial | Orbit vector shown but not interpreted |
| 2 | MaxEnt shadow (classical) | Yes | Yes | Good | Entropy, distribution, hard disagreement explained |
| 3 | MaxEnt shadow (indexed mode) | Yes | **No** | **Absent** | Indexed-mode χ-scaled likelihoods not surfaced separately |
| 4 | MaxEnt divergence (classical vs indexed) | Yes | Trigger only | Thin | Appears only as T13 row in abductive table |
| 5 | Naturality certificates (CI Rope) | Yes | Yes | Partial | Signature name shown, no interpretive gloss |
| 6 | Failure witnesses (FNL) | Yes | Yes | Partial | Label "false_natural_law" shown, not explained as "physics-washed" |
| 7 | Failure witnesses (FCR) | Yes | Yes | Partial | Label "false_ci_rope" shown, not explained as "coordination-washed" |
| 8 | Abductive synthesis (T1–T11) | Yes | Yes | Thin | Table of trigger_class/confidence/anomaly — no explanations |
| 9 | Abductive synthesis (T13–T16) | Yes | Yes | Thin | Same table format; no Category B interpretation |
| 10 | Purity score | Yes | Yes | Thin | Number + band, no explanation of 4 subscores |
| 11 | Contamination network (FPN) | Yes | **No** | **Absent** | Contamination paths, neighbors, propagation not surfaced |
| 12 | Diagnostic integration (verdict) | Yes | Yes | Good | GREEN/YELLOW/RED with tensions, agreements, expected conflicts |
| 13 | Expected conflict catalog (P1–P11) | Yes | Yes | Good | Pattern + explanation surfaced in verdict body |
| 14 | Scope effect | Partial | **Minimal** | **Near-absent** | See detailed finding below |
| 15 | H¹ band–hub correspondence | Yes | Partial | Thin | See detailed finding below |
| 16 | Covering analysis (missed transitions) | Yes | Yes | Thin | See detailed finding below |
| 17 | Per-constraint theorem applicability | **No** | **No** | **Absent** | See detailed finding below |
| 18 | Oracle gap (T13/T14/T16) | Yes | Trigger only | Thin | See detailed finding below |
| 19 | Drift events | Yes | Yes | Moderate | Count + type listed, no severity interpretation |
| 20 | Boltzmann compliance | Yes | Yes | Partial | "boltzmann" field in identity, no factorization detail |

---

## Detailed Findings for Flagged Items

### A. Scope Effect Analysis — Near-Absent

**Computed:**
- `constraint_indexing.pl:scope_modifier/2` (lines 236–241) — 6 scope atoms with config-driven multipliers
- `boltzmann_compliance.pl:scope_invariance_test/2` — tests if classification varies across scopes
- `purity_scoring.pl:scope_invariance_subscore/2` (lines 60–71) — purity penalty from scope variance
- `covering_analysis.pl` — classifies at 3 scope values in its 12-cell grid

**Surfaced:**
- Purity score (one number) absorbs the scope invariance subscore without exposing it
- Covering analysis surfaces as "Missed Transitions: N" in Level 3, without isolating which are scope-driven vs power-driven
- No dedicated "SCOPE EFFECT ANALYSIS" section exists in `enhanced_report.py` (confirmed: zero matches for "scope_effect" or "SCOPE EFFECT" in Python code)

**Gap:** The engine knows whether scope changes classification for this constraint. The report does not tell the LLM. The invariant analysis (MEMORY.md) found scope has zero classification effect across the corpus — if this is a structural finding worth reporting per-constraint, the report should say so explicitly rather than burying it in purity.

---

### B. Per-Constraint Theorem Applicability — Absent

**Computed:** The underlying data exists (H¹, orbit vector, MaxEnt divergence, mandatrophy gaps), but no code maps these to theorem numbers.

**Surfaced:** Zero. The string "theorem" does not appear in `enhanced_report.py`. No section tells the LLM which of the six §3 theorems are instantiated by this constraint.

**What should happen (examples):**

| Constraint state | Applicable theorem(s) | What the report should say |
|---|---|---|
| Snare from U₁/U₂/U₄, rope from U₃ | T1 (cover story), T3 (spectral dominance), T6 (Hub 1) | "Institutional observer provides the cover story (Theorem 1). This is Hub 1–driven perspectival fracture (Theorem 6)." |
| H¹ = 3 | T2 (gap structure), T6 (Hub 1) | "H¹=3 confirms discrete bloc clustering (Theorem 2), driven by Hub 1 sigmoid crossing (Theorem 6)." |
| H¹ = 4 | T2 (gap structure), T6 (Hub 2) | "H¹=4 indicates Hub 2 immutability flip (Theorem 6)." |
| T16 fires | T4 (oracle gap) | "MaxEnt is confident but H¹>0: single-position analysis misses cross-position structure (Theorem 4, the Classical Oracle Gap)." |
| Boltzmann-compliant | T5 (functor axiom) | "Boltzmann factorization holds — this constraint's restriction maps compose correctly (Theorem 5)." |

**Impact:** Without theorem mapping, the essay-writing LLM cannot connect this constraint's diagnostics to the formal results in §3. The LLM would need to independently derive these connections from framework knowledge, which it does not reliably have.

---

### C. H¹ Band–Hub Correspondence (Theorem 6) — Thin

**Computed:** `grothendieck_cohomology.pl:cohomological_obstruction/3` (lines 143–150) computes H¹ as disagreeing-pair count.

**Surfaced:** `enhanced_report.py` lines 537–544 show H¹ with a brief descriptor:

```python
h1_desc = {0: "gauge-invariant (all observers agree)",
           3: "power-scaling driven", 4: "hub-conflict driven",
           5: "high fracture", 6: "maximally fractured"}
```

**Gap:** The descriptors hint at hub correspondence ("power-scaling driven" → Hub 1, "hub-conflict driven" → Hub 2) but:
1. They don't name the hubs or explain the mechanism
2. They don't say which specific power boundary the crossing occurs at
3. H¹=3 doesn't tell the LLM "the institutional observer (π=−0.2) flips χ below the snare threshold while other observers remain above it"
4. H¹=4 doesn't tell the LLM "the immutability lookup table produces a mountain/non-mountain split at the biographical→generational time horizon boundary"

The orbit vector IS surfaced in Level 1 (ORBIT CONTEXT), so a sophisticated LLM could infer which observer disagrees by reading the vector. But the connection to the hub architecture is not made explicit.

---

### D. Covering Analysis Missed Transitions — Surfaced but Unexplained

**Computed:** `covering_analysis.pl:missed_transitions/1` (lines 560–610) detects classification changes between adjacent fine-grid points within single coarse cells.

**Surfaced:** `enhanced_report.py:build_structural_section` (lines 855–875) shows:
```
Covering Analysis:
    Missed Transitions: 3
    Unique Type Shifts:  rope -> tangled_rope, snare -> tangled_rope
```

**Gap:**
1. No explanation of what "missed transition" means (a classification boundary that falls between grid points, invisible to the 12-cell evaluation)
2. No indication of WHERE in (power, scope) space the transitions occur
3. No interpretation of whether the transitions matter (a transition in an uninhabited region of parameter space is less important than one near common observer positions)
4. The Erdős–Selfridge analogy is not explained — the LLM gets a count and type shifts with no structural framing

---

### E. Oracle Gap (T13/T14/T16) — Flagged but Not Explained

**Computed:**
- T13 (`trigger_maxent_divergence`, lines 758–801): indexed vs classical MaxEnt divergence + H¹>0
- T14 (`trigger_hub_conflict`, lines 816–860): H¹ = configured band (typically 4)
- T16 (`trigger_classical_oracle_failure`, lines 943–989): low MaxEnt entropy + H¹>0 (confident but cross-positionally wrong)
- T13 blocks T16 via exclusion gate (if T13 fires, T16 is suppressed as weaker)

**Surfaced:** If any of these triggers fire, they appear as a row in the abductive flags table:
```
| Trigger Class | Confidence | Anomaly | Category |
| classical_oracle_failure | 0.68 | low_entropy_h1_positive | genuine |
```

**Gap:**
1. The table entry does not explain what "classical_oracle_failure" means: that a MaxEnt classifier operating on observer-independent metrics is confident in its classification, but cross-position analysis (H¹>0) reveals disagreement it cannot detect
2. The connection to Theorem 4 is not made ("this is an instance of the Classical Oracle Gap")
3. The U₄ paradox is not surfaced: if the constraint fires T16, the report should note that the analytical observer — the position from which DR analysis is conducted — is the position where this gap is largest
4. The relationship between T13 and T16 (T13 is the probabilistic version, T16 is the categorical version; T13 gates T16) is invisible to the LLM

---

### F. MaxEnt Indexed vs Classical Divergence — Not Separately Surfaced

**Computed:** The engine runs MaxEnt in both classical mode (using ε) and indexed mode (using χ per observer context). The divergence between these modes is what T13 detects.

**Surfaced:** Only through T13 in the abductive flags table. The classical-mode and indexed-mode probability distributions are not shown side by side. The corrected total variation distance (mentioned in paper §3, Theorem 4 as ~0.0006 at analytical context) is not surfaced per-constraint.

**Gap:** The paper describes this as a key diagnostic: "The divergence between these modes measures the probabilistic effect of observer-dependence" (§4.2). The report gives the LLM only the classical-mode MaxEnt probabilities (`maxent_probs` in the JSON), not the indexed-mode comparison. The LLM cannot assess whether observer-dependence has probabilistic consequences for this specific constraint.

---

### G. Contamination Network — Not Surfaced

**Computed:**
- `drl_purity_network.pl:constraint_neighbors/3` — discovers neighbors from 4 edge sources (explicit affects, inferred coupling, shared beneficiary, shared victim)
- `drl_fpn.pl:fpn_iterate/5` — Scott-continuous Jacobi iteration converging to fixed point of contamination endofunctor
- `drl_fpn.pl:fpn_ep/3` — effective purity after network propagation (distinct from intrinsic purity)

**Surfaced:** Only the final purity score (one number) and band label. The network topology — which constraints contaminate this one, through what mechanism, at what strength — is not surfaced. FPN zone migration appears only if T6 or T7 fire.

**Gap:** The LLM cannot explain WHY a constraint has low purity. It sees "Purity: 0.42 (borderline)" but not "contaminated by constraint X (shared beneficiary, strength 0.8) and constraint Y (structural coupling, strength 0.5), both classified as snare."

---

## Severity Assessment

| Severity | Gap | Impact on essay quality |
|---|---|---|
| **Critical** | Per-constraint theorem applicability absent | LLM cannot connect diagnostics to formal results; essays will be atheoretical |
| **High** | Oracle gap unexplained | LLM will flag T13/T16 without understanding the epistemic recursion |
| **High** | H¹ hub correspondence thin | LLM cannot explain WHY observers disagree, only THAT they do |
| **High** | Indexed MaxEnt not surfaced | LLM cannot assess probabilistic vs categorical observer-dependence |
| **Moderate** | Contamination network absent | LLM cannot trace purity degradation to structural causes |
| **Moderate** | Abductive triggers unexplained | LLM gets trigger codes without interpretive glosses |
| **Moderate** | Covering analysis unexplained | LLM gets counts without structural meaning |
| **Low** | Scope effect near-absent | Scope has zero classification effect; surfacing its absence is still useful but not urgent |
| **Low** | Naturality certificate labels unexplained | "false_natural_law" is somewhat self-documenting; "false_ci_rope" less so |

---

## What the Report Does Well

For completeness: several diagnostics are surfaced with good LLM-usable context.

- **Verdict banner** (GREEN/YELLOW/RED): clear traffic-light with subsystem counts and tension identification
- **Diagnostic verdict body**: agreements, expected conflicts (with P1-P11 pattern explanations), convergent rejections, tensions — all well-structured
- **MaxEnt shadow classification**: hard disagreement vs high uncertainty vs stable; entropy, margin, top-3 probability distribution — enough for an LLM to discuss classification confidence
- **Mandatrophy gap**: delta_chi with severity (critical/high/moderate) — directly interpretable
- **Omega context**: severity score, gap class, gap pattern, family — links constraint to its epistemic question

---

*Generated 2026-03-01. Sources: `enhanced_report.py` (1223 lines), `report_generator.pl` (807 lines), `abductive_triggers.pl` (989 lines), `covering_analysis.pl` (~800 lines), `grothendieck_cohomology.pl` (~380 lines), `drl_fpn.pl` (~322 lines), `purity_scoring.pl` (89 lines), `deferential_realism_paper_v6.1.md` §3–§4.2.*
