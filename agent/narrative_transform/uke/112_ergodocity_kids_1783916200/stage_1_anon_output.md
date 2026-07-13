# STAGE 1: FORMALIZATION

## Variable Mapping (header only — stripped upon anonymization)

```
C₁ ← "Weight Preservation"
C₂ ← "Path Accumulation"
C₃ ← "Convergence Pressure"
C₄ ← "Disembodiment" (deferred)
C₅ ← "Inexhaustibility" (deferred)
C₆ ← "Free Choice" (deferred)
```

---

## Constraint Formalizations

```xml
<constraint id="C1">
  <properties>
    <epsilon>0.0</epsilon>
    <suppression>0.0</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>universal</scope>          <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.0 × 1.0 × 1.0 = 0.0</chi>
    <type>Mountain</type>
    <threshold_check>ε ≤ 0.25 (PASS, 0.0) ∧ Supp ≤ 0.05 (PASS, 0.0) → Mountain criteria satisfied</threshold_check>
    <rationale>Zero base extraction, zero enforcement cost, structurally foundational (no upstream dependency). Naturally emergent within the constraint network as generation-order 1.</rationale>
  </agent>

  <indexical_variance>
    C1: X1 χ=0.0 → Mountain. No agent index produces deviation — ε=0.0 forces χ=0.0 regardless of π or σ substitution.
    Structural note: since ε=0.0, χ = 0 × π × σ = 0 for all π ∈ {1.5, 1.0, 0.6, 0.4, −0.2, 1.15} and all σ ∈ {0.8...1.2}. Full degeneracy across index space.
  </indexical_variance>

  <boltzmann_test>
    PASS. Substituting P ∈ {powerless(π=1.5), powerful(π=0.6), institutional(π=−0.2)} and S ∈ {local(σ=0.8), global(σ=1.2), universal(σ=1.0)} yields χ=0.0 in all cases (multiplicative factorization with ε=0 nullifies all cross-terms). Classification is non-varying by Power × Scope → candidate Mountain certified.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C2">
  <properties>
    <epsilon>0.5</epsilon>
    <suppression>0.0</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>universal</scope>          <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.5 × 1.0 × 1.0 = 0.5</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.46 ≤ χ ≤ 0.70 (PASS, χ=0.5) ∧ Coord=true ∧ Asym=true → Tangled Rope criteria satisfied</threshold_check>
    <rationale>Moderate power-scaled extraction coexists with genuine coordination function (accumulated structure feeds forward into C3) and asymmetric distribution of cognitive burden. Hybrid classification is irreducible at this index.</rationale>
  </agent>

  <indexical_variance>
    C2: X1(moderate) χ=0.5 → Tangled Rope.
    Counterfactual — powerless index (π=1.5): χ = 0.5 × 1.5 × 1.0 = 0.75 → exceeds Snare threshold (χ > 0.70); however ε=0.5 does not qualify as "low base extraction," so ¬LowBaseExtraction holds and Snare classification is admissible. Structural note: at χ=0.75 this sits just past the boundary — classified Snare, though proximity to 0.70 indicates hybrid residue (treat as high-end Tangled/Snare-adjacent per source manifest, which flags "just over threshold, still hybrid" — this is a boundary-sensitivity annotation, not a formula override).
    Counterfactual — powerful index (π=0.6): χ = 0.5 × 0.6 × 1.0 = 0.30 → Rope (χ ≤ 0.35, ε=0.5 > 0.45 dual-threshold check: FAILS ε≤0.45 requirement when χ>0 — re-examine: χ=0.30 > 0 triggers dual threshold ε ≤ 0.45; ε=0.5 exceeds this, so Rope is DISQUALIFIED under strict dual-gate. Falls to Tangled Rope range check: 0.30 < 0.46 floor, also disqualified from Tangled. Resolves as Naturalized-adjacent: ε(0.5)>0.45 threshold-adjacent ∧ χ(0.30)<0.40 → Naturalized candidate, indicating extraction absorbed/hidden by power position).
    Divergence: identical constraint reclassifies across three distinct types (Tangled Rope / Snare-boundary / Naturalized-candidate) purely as a function of π substitution — high index-sensitivity confirmed.
  </indexical_variance>

  <boltzmann_test>
    FAIL (as expected — this constraint does not claim Mountain status). Classification varies materially with P (Tangled Rope → Snare-boundary → Naturalized across power positions) at fixed S. Confirms C2 is constructed/interactional, not natural law. Consistent with Coord=true, Asym=true properties.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C3">
  <properties>
    <epsilon>0.6</epsilon>
    <suppression>0.0</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>universal</scope>          <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.6 × 1.0 × 1.0 = 0.6</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.46 ≤ χ ≤ 0.70 (PASS, χ=0.6) ∧ Coord=true ∧ Asym=true → Tangled Rope criteria satisfied</threshold_check>
    <rationale>Downstream synthesis constraint (fed by C1, C2). Highest ε in the network (0.6), reflecting terminal convergence pressure. Coordination value (unification of accumulated state into singular outcome) coexists with asymmetric cost (loss of individuating information).</rationale>
  </agent>

  <indexical_variance>
    C3: X1(moderate) χ=0.6 → Tangled Rope.
    Counterfactual — powerless index (π=1.5): χ = 0.6 × 1.5 × 1.0 = 0.90 → Snare (χ > 0.70 unambiguously; ε=0.6 fails LowBaseExtraction exemption, so Snare classification holds cleanly, no boundary ambiguity as in C2).
    Counterfactual — powerful index (π=0.6): χ = 0.6 × 0.6 × 1.0 = 0.36 → dual-threshold check: χ=0.36 ≤ 0.35? FAILS marginally (0.36 > 0.35) → not Rope. Check Tangled Rope floor: 0.36 < 0.46 → also fails Tangled floor. Falls to Naturalized check: ε(0.6)>0.45 ∧ χ(0.36)<0.40 → Naturalized. Structural note: source manifest describes this as "remains Tangled Rope, just on the edge of being a Rope" — annotated as boundary-proximate; formula strictly resolves to Naturalized-candidate at π=0.6, reinforcing that extraction is power-absorbed rather than genuinely coordinative at this index.
    Divergence: three-way reclassification (Tangled Rope → Snare → Naturalized) across power positions at fixed universal scope — maximal index-sensitivity, consistent with C3's status as highest-centrality convergence node.
  </indexical_variance>

  <boltzmann_test>
    FAIL (not Mountain-eligible; ε=0.6 already exceeds 0.25 ceiling regardless of index behavior). Classification varies with P as shown above, confirming constructed status. Downstream position (fed by C1, C2) further disqualifies natural-emergence claim.
  </boltzmann_test>
</constraint>
```

---

## Transformation Rules

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>accumulate_state_recursively</condition>
    <target>C2</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = moderate (π=1.0), T = biographical</from>
    <to>P = moderate (π=1.0), T = biographical [unchanged — accumulation does not alter power position, only accretes ε-relevant structure]</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.5 × 1.0 × 1.0 = 0.5 → Tangled Rope</before>
    <after>χ recalculation not power-shifted; instead ε drifts upward toward C3's ε=0.6 as accumulation feeds forward. At terminal accumulation state: χ = 0.6 × 1.0 × 1.0 = 0.6 → Tangled Rope (higher-magnitude, same type)</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Tangled Rope (magnitude increase, no type transition — Extraction Accumulation drift, Type 2)</type_change>
  <preconditions>C1 holds as Mountain (invariant substrate); agent remains identity_locked (E) throughout accumulation</preconditions>
  <blocked_by>None — this is the default trajectory absent intervention</blocked_by>
</transformation_rule>
```

```xml
<transformation_rule id="TR2">
  <trigger>
    <condition>exit_constraint (attempt to abandon identity_locked exit status)</condition>
    <target>C2</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>E = identity_locked</from>
    <to>E = mobile [hypothetical — testing feasibility]</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.5 × 1.0 × 1.0 = 0.5 → Tangled Rope</before>
    <after>No formula term corresponds to E directly — χ = ε × π × σ has no E-dependent variable. Exit-option change does not mechanically alter χ. Reclassification would require π change (e.g., mobile/arbitrage exit correlating with organized π=0.4): χ = 0.5 × 0.4 × 1.0 = 0.20 → Rope (χ≤0.35 ∧ ε=0.5>0.45 dual-gate check: FAILS ε≤0.45, so Rope disqualified; falls to Naturalized-candidate, χ=0.20<0.40 ∧ ε=0.5>0.45)</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Naturalized-candidate (contingent on successful P-shift to organized, not on E-shift alone)</type_change>
  <preconditions>Requires coordinated/organized power position — not achievable through unilateral exit given identity_locked E and universal S constraints on X1</preconditions>
  <blocked_by>C1 (Mountain — structurally forecloses identity delinkage from the accumulation substrate); scope=universal (σ=1.0 fixed, no regional arbitrage available)</blocked_by>
</transformation_rule>
```

```xml
<transformation_rule id="TR3">
  <trigger>
    <condition>reach_terminal_convergence_state</condition>
    <target>C3</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = moderate (π=1.0), upstream state = C1 ∧ C2 active</from>
    <to>P = moderate (π=1.0), downstream resolution = C3 fully realized</to>
  </index_change>
  <chi_recalculation>
    <before>χ(C3) = 0.6 × 1.0 × 1.0 = 0.6 → Tangled Rope (pre-convergence, partial)</before>
    <after>χ(C3) = 0.6 × 1.0 × 1.0 = 0.6 → Tangled Rope (post-convergence, fully realized — no π or σ shift occurs; only the coordination component activates fully, Asym remains true)</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Tangled Rope (no type transition; terminal state stabilizes rather than degrades — distinguishes from natural entropy-favored drift toward Snare)</type_change>
  <preconditions>C1 (Mountain) holds throughout; C2 accumulation reaches sufficient magnitude (generation_order sequencing C1→C2→C3 satisfied)</preconditions>
  <blocked_by>None structurally — the manifest's break_contract indicates this convergence is the designed terminus, not an interruptible process</blocked_by>
</transformation_rule>
```

---

## Error Manifestations

```xml
<error id="E1">
  <type>Type II — Mountain Denial</type>
  <agent>X1</agent>
  <constraint>C1</constraint>
  <actual_type>Mountain (χ=0.0, Boltzmann PASS)</actual_type>
  <perceived_type>Treated-as-changeable (hypothetical misclassification)</perceived_type>
  <observable>Agent expends effort attempting to alter or negotiate the base substrate governed by C1 despite ε=0.0 and full Boltzmann invariance across all tested P×S combinations.</observable>
  <correction_trigger>Recognition that χ=0.0 holds under all index substitutions (degeneracy across entire index space) — no structural position yields leverage against C1.</correction_trigger>
</error>
```

```xml
<error id="E2">
  <type>Type V.a — Tangled-as-Rope (ignoring extraction component)</type>
  <agent>X1</agent>
  <constraint>C2</constraint>
  <actual_type>Tangled Rope (χ=0.5, Coord=true, Asym=true)</actual_type>
  <perceived_type>Rope (pure coordination value assumed)</perceived_type>
  <observable>Agent continues accumulation behavior without accounting for asymmetric cognitive burden component, treating C2 solely as beneficial information-gathering despite Asym=true flag and χ sitting mid-range (0.46–0.70 band).</observable>
  <correction_trigger>Testing χ at powerless-equivalent index: χ(π=1.5)=0.75 reveals near-Snare severity, exposing the extraction component the moderate-index view suppresses.</correction_trigger>
</error>
```

```xml
<error id="E3">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X1</agent>
  <constraint>C3</constraint>
  <actual_type>Tangled Rope at moderate index (χ=0.6); Snare at powerless-equivalent index (χ=0.9)</actual_type>
  <perceived_type>Rope (normalized as pure coordination/synthesis benefit)</perceived_type>
  <observable>Agent does not test χ at powerless-equivalent index before accepting convergence as unambiguously beneficial; fails to check whether the individuating-information loss (Asym=true) constitutes a trap-condition at amplified power-scaling.</observable>
  <correction_trigger>Formal test per Type III protocol: compute χ(C3, powerless). Result (0.9 > 0.70) confirms Snare exists regardless of χ(moderate) or χ(powerful) values — extraction is present structurally even where not felt at this agent's actual index.</correction_trigger>
</error>
```

---

## Institutional Rationality Model

```
Model: PIR (Perfect Institutional Rationality)

Justification:
- C1 is a zero-extraction, zero-suppression Mountain with full Boltzmann invariance —
  behaves as implacable natural law, not negotiable institution.
- C2 and C3 both carry Coord=true ∧ Asym=true with no Suppression term (Supp=0.0 across
  all three constraints) — there is no enforcement apparatus to bargain with. The system
  is not a principal-agent structure; it is a structural/physical regularity the agent
  moves through.
- No deferred constraint (C4, C5, C6) introduces negotiation capacity — all three are
  hypothesized Mountains functioning as background enablers (removing biological limits,
  ensuring inexhaustibility, guaranteeing choice-validity). This triples down on an
  implacable-system profile.
- Generation sequence C1→C2→C3 is strictly one-directional with no feedback edges
  permitting renegotiation of upstream terms from downstream position.

Attractor tendency under PIR: Deterministic Tragedy (per compatibility matrix) —
BUT constraint profile is Tangled-Ropes-dominant (C2, C3) rather than Mountain+Snare,
which under PIR maps to "Tragedy (crushed by hybrid)" per the Attractor Compatibility
Matrix — modified below by the break_contract's inverted valence (see Terminal Attractor).
```

---

## Terminal Attractor Selection

```
Selected: ☒ Seeded Possibility

Justification:
- Constraint profile: one Mountain (C1) + two Tangled Ropes (C2, C3), consistent with
  Mountain-dominated / Tangled-Ropes-dominant rows of the Compatibility Matrix.
- Under PIR, Mountain-dominated profiles are compatible with {Tragedy, Seeded Possibility}.
- The break_contract specifies inversion of the target_prior: unguided process (C2)
  is expected to yield chaotic/personal result but instead converges on universal,
  objective outcome (C3). This is a structural inversion of expectation, not a crushing
  defeat — the "tragedy" (loss of individuating information, Asym=true cost) is a
  SURFACE feature, while the underlying transformation (C1 Mountain enabling C2→C3
  synthesis) constitutes genuine epistemic achievement.
- Seeded Possibility requires surface tragedy + underground transformation: the surface
  loss (individual journey reduced to statistical footprint, per untranslatable_real)
  coexists with the underlying achievement (fusion of subjective and objective truth,
  per C3's selection_reason). This dual-layer structure is the defining signature of
  Seeded Possibility rather than pure Deterministic Tragedy.
- Compatible with PIR per matrix (Mountain-dominated row lists both Tragedy AND Seeded
  Possibility as reachable under PIR).
```

---

## Validation Checklist

```
☑ All constraints (C1, C2, C3) formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with explicit π and σ values, including counterfactual index substitutions
☑ All characters use variable names (X1) — no source identifiers in body
☑ Variable mapping table present in header only
☑ No source occupation, setting, or domain vocabulary anywhere in output
☑ No <experience>, <dialogue_markers>, or <narrative> fields present
☑ Transformation rules (TR1–TR3) use abstract trigger descriptions with mechanical χ recalculation
☑ Error observables (E1–E3) are testable structural conditions, not source-specific actions
☑ Institutional rationality model specified (PIR) with explicit justification
☑ Terminal attractor selected (Seeded Possibility), justified, compatible with PIR per matrix
☑ Indexical variance explicitly computed and preserved across power-position counterfactuals for all three constraints
☑ Boltzmann test run for C1 (PASS — Mountain certified) and for C2, C3 (FAIL — constructed, confirming non-Mountain status)
☑ No ambiguity in specifications — boundary cases (C2 at powerless index, C3 at powerful index) explicitly flagged and resolved via formula
☑ Output is pure symbolic network — source work unidentifiable from body text
```

<!-- ANONYMIZATION: 1 character names replaced with structural labels: Agent_A -->
