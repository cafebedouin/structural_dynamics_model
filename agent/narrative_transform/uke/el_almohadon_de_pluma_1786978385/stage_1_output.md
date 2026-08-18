## STAGE 1: FORMALIZATION

### Variable Mapping (header only — stripped at anonymization)

```
X₁ ← Alicia
X₂ ← Jordán
X₃ ← Doctors (aggregate analytical agent)
X₄ ← Servant
C₁ ← Medical Ignorance
C₂ ← Gendered Passivity
C₃ ← Hidden Predator
C₄ ← Emotional Repression (deferred, Tangled Rope hypothesis)
C₅ ← Environmental Oppression (deferred, Mountain hypothesis)
C₆ ← Social Isolation (deferred, Tangled Rope hypothesis)
```

---

## 1. CONSTRAINT FORMALIZATIONS

```xml
<constraint id="C1">
  <properties>
    <epsilon>0.10</epsilon>
    <suppression>0.00</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>powerless</power>       <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.10 × 1.5 × 0.8 = 0.12</chi>
    <type>Mountain</type>
    <threshold_check>ε ≤ 0.25 (0.10 ✓), Supp ≤ 0.05 (0.00 ✓) — PASS</threshold_check>
    <rationale>Zero suppression, no coordination function, naturally emergent knowledge gap. Immutable at immediate time horizon with trapped exit.</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>moderate</power>         <!-- π = 1.0 -->
      <time>immediate</time>
      <exit>constrained</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.10 × 1.0 × 0.8 = 0.08</chi>
    <type>Mountain</type>
    <threshold_check>ε ≤ 0.25 ✓, Supp ≤ 0.05 ✓ — PASS</threshold_check>
    <rationale>Lower χ than X1 due to reduced power modifier; classification identical, magnitude differs.</rationale>
  </agent>

  <agent ref="X3">
    <index>
      <power>analytical</power>       <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>analytical</exit>
      <scope>regional</scope>         <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.10 × 1.15 × 0.9 = 0.10</chi>
    <type>Mountain</type>
    <threshold_check>ε ≤ 0.25 ✓, Supp ≤ 0.05 ✓ — PASS</threshold_check>
    <rationale>Analytical index detects the same base fact of the knowledge gap; no beneficiary structure present to reclassify as extraction.</rationale>
  </agent>

  <indexical_variance>
    C1: X1 χ=0.12 → Mountain, X2 χ=0.08 → Mountain, X3 χ=0.10 → Mountain.
    No type divergence — magnitude scales with π but stays within Mountain ceiling for all tested indices.
  </indexical_variance>

  <boltzmann_test>
    PASS. Varying P (powerless→moderate→analytical) and S (local→regional) produces
    only χ-magnitude shift (0.12→0.08→0.10), never a type change. No beneficiary structure
    detected; classification is Power × Scope independent. Candidate Mountain confirmed.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C2">
  <properties>
    <epsilon>0.60</epsilon>
    <suppression>0.60</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.60 × 1.5 × 0.8 = 0.72</chi>
    <type>Snare</type>
    <threshold_check>χ ≥ 0.66 ✓ (0.72), ε ≥ 0.46 ✓ (0.60), Supp ≥ 0.60 ✓ (0.60) — PASS</threshold_check>
    <rationale>High power-scaled extraction combined with high base extraction and active enforcement (Supp=0.60). Identity-locked exit confirms immutability-to-this-index required by SnareImmutability gate; a higher-power index (institutional) perceives the same structure as Rope.</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>institutional</power>    <!-- π = -0.2 -->
      <time>biographical</time>
      <exit>arbitrage</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.60 × (-0.2) × 0.8 = -0.10</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0 → ε ceiling bypassed. χ ≤ 0.35 ✓ (-0.10) — PASS</threshold_check>
    <rationale>Negative χ indicates net beneficiary status. Same constraint, opposite structural position: institutional power extracts FROM the coordination mechanism rather than being extracted BY it.</rationale>
  </agent>

  <indexical_variance>
    C2: X1 χ=0.72 → Snare, X2 χ=-0.10 → Rope.
    Maximal type divergence on identical base constraint — canonical Snare/Rope indexical split
    driven entirely by π sign change (powerless π=1.5 vs institutional π=-0.2).
  </indexical_variance>

  <boltzmann_test>
    FAIL (as expected — not a Mountain candidate). Type changes with P (Snare at powerless,
    Rope at institutional) — constructed, not natural. This is the designed indexical
    variance for C2, not a certification target.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C3">
  <properties>
    <epsilon>1.00</epsilon>
    <suppression>0.90</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 1.00 × 1.5 × 0.8 = 1.20</chi>
    <type>Snare</type>
    <threshold_check>χ ≥ 0.66 ✓ (1.20), ε ≥ 0.46 ✓ (1.00), Supp ≥ 0.60 ✓ (0.90) — PASS</threshold_check>
    <rationale>Maximal base extraction, near-ceiling suppression, trapped exit. Direct extraction target — no natural-law-without-beneficiary exemption applies (a locatable extracting agent exists).</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 1.00 × 1.5 × 0.8 = 1.20</chi>
    <type>Snare</type>
    <threshold_check>Identical to X1 — PASS</threshold_check>
    <rationale>Same index as X1. This is the collapse specified by omega power_scaling: an agent whose index elsewhere diverges from X1 (C2: moderate vs powerless) converges to identical powerless/trapped index under C3.</rationale>
  </agent>

  <agent ref="X4">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 1.00 × 1.5 × 0.8 = 1.20</chi>
    <type>Snare</type>
    <threshold_check>Identical to X1, X2 — PASS</threshold_check>
    <rationale>Third independent agent converges to identical index and χ, confirming the constraint erases pre-existing power distinctions among all subjects sharing local scope.</rationale>
  </agent>

  <indexical_variance>
    C3: X1 χ=1.20 → Snare, X2 χ=1.20 → Snare, X4 χ=1.20 → Snare.
    Zero indexical variance across all three agents — deliberate omega effect. Ignorance
    (C1) forecloses the analytical exit option that would otherwise differentiate agents,
    and the predatory mechanism itself does not scale with prior social power (X2's
    institutional-adjacent standing under C2 does not transfer).
  </indexical_variance>

  <boltzmann_test>
    N/A for Mountain certification — C3 is not a Mountain candidate (χ far exceeds Snare
    floor for all tested indices; ε=1.00 exceeds Mountain ceiling categorically). Test not
    applicable; classification is Snare by direct threshold satisfaction, not by
    Boltzmann-failure inference.
  </boltzmann_test>
</constraint>
```

---

## 2. OMEGA FORMALIZATION

```xml
<omega id="power_scaling">
  <formal_statement>
    ∀Xᵢ ∈ {X1, X2, X4}: Index(Xᵢ, C3) = (powerless, immediate, trapped, local)
    despite Index(X1, C2) ≠ Index(X2, C2)
  </formal_statement>
  <mechanism>
    C3's χ calculation depends on C1 (ε=0.10 Mountain — forecloses analytical detection
    for all subjects regardless of prior standing) as a precondition. The compound effect
    of C1 ∧ C2 on agent state resets P to powerless and E to trapped uniformly at the
    point C3 activates, overriding whatever P/E value each agent carried under C2 alone.
  </mechanism>
  <testable_consequence>
    Any agent index computed for C3 in isolation from C1, C2 upstream state would
    incorrectly predict indexical variance (per X2's institutional standing under C2).
    Observed convergence to uniform χ=1.20 falsifies that isolated computation and
    confirms upstream-dependency propagation (C1, C2 → C3 per generation_sequence).
  </testable_consequence>
</omega>
```

---

## 3. TRANSFORMATION RULES

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>acquire_analytical_capacity</condition>
    <target>C1</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = analytical (π=1.15), E = analytical</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.10 × 1.5 × 0.8 = 0.12 → Mountain</before>
    <after>χ = 0.10 × 1.15 × 0.8 = 0.092 → Mountain</after>
  </chi_recalculation>
  <type_change>Mountain → Mountain (no type change; magnitude only)</type_change>
  <preconditions>External information source breaching C1's suppression floor; requires disruption of C6 (social isolation, deferred) to reach X1</preconditions>
  <blocked_by>C2 (identity-locked exit prevents X1 from accessing or trusting external analytical input); C6 (isolation prevents contact with X3-type agents)</blocked_by>
</transformation_rule>

<transformation_rule id="TR2">
  <trigger>
    <condition>exit_constraint</condition>
    <target>C2</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = identity_locked</from>
    <to>P = moderate (π=1.0), E = mobile</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.60 × 1.5 × 0.8 = 0.72 → Snare</before>
    <after>χ = 0.60 × 1.0 × 0.8 = 0.48 → Tangled Rope</after>
  </chi_recalculation>
  <type_change>Snare → Tangled Rope</type_change>
  <preconditions>Identity-lock must first be broken (biographical-scale process); requires external support structure absent from current graph (would require constructing a new Scaffold not present in manifest)</preconditions>
  <blocked_by>C3 (once C3 activates, downstream convergence in the omega overrides any C2-level exit gain — X1's index resets to powerless/trapped regardless of C2 state); Supp(C2)=0.60 actively resists this transition (T1 direction reversal, entropically disfavored per §IX)</blocked_by>
</transformation_rule>

<transformation_rule id="TR3">
  <trigger>
    <condition>establish_alternative_verification</condition>
    <target>C3</target>
    <agent>X3</agent>
  </trigger>
  <index_change>
    <from>S = local (σ=0.8) [as experienced by X1, X2, X4]</from>
    <to>S = regional (σ=0.9) [external analytical verification imported to local site]</to>
  </index_change>
  <chi_recalculation>
    <before>χ(X1) = 1.00 × 1.5 × 0.8 = 1.20 → Snare</before>
    <after>χ(X1) = 1.00 × 1.5 × 0.9 = 1.35 → Snare (magnitude increase, type stable)</after>
  </chi_recalculation>
  <type_change>Snare → Snare (no type change — scope increase amplifies rather than resolves; harder verification at wider scope does not by itself reduce extraction at the trapped index)</type_change>
  <preconditions>Requires C1 resolution first (TR1) — X3's analytical capacity must penetrate the ignorance floor before verification can reach the trapped agents</preconditions>
  <blocked_by>C1 (Mountain status — Supp=0.00 but structural opacity means no enforcement is needed to maintain the gap, so no simple "removal" action targets it); C3's own Supp=0.90 directly suppresses detection channels</blocked_by>
</transformation_rule>

<transformation_rule id="TR4">
  <trigger>
    <condition>organize_collective</condition>
    <target>C3</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = organized (π=0.4), E = constrained</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 1.00 × 1.5 × 0.8 = 1.20 → Snare</before>
    <after>χ = 1.00 × 0.4 × 0.8 = 0.32 → Rope (candidate) — BLOCKED, see below</after>
  </chi_recalculation>
  <type_change>Formally Snare → Rope if achieved; NOT REACHABLE under current graph</type_change>
  <preconditions>Requires simultaneous resolution of C1 (ignorance forecloses recognizing the threat exists to organize against) AND C2 (identity-lock forecloses coalition-formation) AND C6 (isolation forecloses contact with co-organizers)</preconditions>
  <blocked_by>C1 ∧ C2 ∧ C6 jointly. This transformation is formally computable but structurally unreachable given the upstream dependency chain C1→C2→C3; no single-constraint intervention satisfies preconditions. Recorded as theoretical ceiling, not achievable path.</blocked_by>
</transformation_rule>
```

---

## 4. ERROR MANIFESTATIONS

```xml
<error id="E1">
  <type>Type I — False Mountain</type>
  <agent>X1</agent>
  <constraint>C2</constraint>
  <actual_type>Snare (χ=0.72 at powerless index)</actual_type>
  <perceived_type>Mountain (treated as immutable biographical fact of position)</perceived_type>
  <observable>Does not attempt exit or renegotiation of role-constraint despite Supp=0.60 (non-zero, indicating constructed enforcement rather than natural emergence) and despite X2's institutional index showing the identical constraint yields Rope (χ=-0.10)</observable>
  <correction_trigger>Exposure to X2-equivalent index (institutional power, arbitrage exit) on the same constraint C2 would reveal Boltzmann-failure — classification varies by P, confirming construction, not nature</correction_trigger>
</error>

<error id="E2">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X2</agent>
  <constraint>C2</constraint>
  <actual_type>Rope at X2's own index (χ=-0.10) — this classification is individually correct</actual_type>
  <perceived_type>Generalized as Rope for all agents (extrapolation error, not X2's own misclassification)</perceived_type>
  <observable>System-level test: χ(powerless index on C2) = 0.72 ≥ 0.66, confirming Snare exists at X1's index regardless of X2's χ. Error is structural (whoever generalizes from institutional index to universal claim) rather than located in X2's own experience</observable>
  <correction_trigger>Explicit computation of χ at powerless index (per §V Type III test procedure) surfaces the Snare that institutional-index reasoning alone would miss</correction_trigger>
</error>

<error id="E3">
  <type>Type II — Mountain Denial</type>
  <agent>X3</agent>
  <constraint>C1</constraint>
  <actual_type>Mountain (χ=0.10, passes Boltzmann test)</actual_type>
  <perceived_type>Treated as remediable within immediate time horizon by proximate agents (X1, X2), producing repeated failed intervention attempts</perceived_type>
  <observable>Diagnostic/analytical effort applied at biographical time horizon fails to resolve the condition within the immediate horizon relevant to X1's exposure to C3; energy expended without χ reduction, consistent with fighting a genuine Mountain rather than a constructed barrier</observable>
  <correction_trigger>Reclassification would require either (a) time horizon shift to civilizational scale where the underlying ignorance becomes historically contingent rather than immediate-Mountain, or (b) demonstration that ε varies with an identifiable beneficiary — neither condition is present in the current manifest</correction_trigger>
</error>

<error id="E4">
  <type>Type V.b — Tangled-as-Snare (deferred constraint, flagged for Stage 0 verification)</type>
  <agent>X1</agent>
  <constraint>C4 (Emotional Repression, deferred — hypothesis Tangled Rope)</constraint>
  <actual_type>Hypothesized Tangled Rope (Coord=true per offstage_function: enables some relational function; Asym=true)</actual_type>
  <perceived_type>Risk: if formalized carrying only extraction reading, coordination component (whatever communicative function repression preserves) is lost</perceived_type>
  <observable>Not yet testable — C4 remains deferred/unformalized in this manifest. Flagged so Stage 0 does not resolve C4 by defaulting to pure-Snare without checking Coord=true against thresholds (χ ∈ (0.35, 0.90], ε ≥ 0.30, Supp ≥ 0.40, RequiresActiveEnforcement)</observable>
  <correction_trigger>Full C4 formalization with explicit per-agent χ; premature resolution blocked pending that step</correction_trigger>
</error>
```

---

## 5. INSTITUTIONAL RATIONALITY MODEL

```
Model: PIR (Perfect Institutional Rationality)

Justification:
  - C3 (terminal constraint) exhibits ε=1.00, Supp=0.90 — near-maximal on both axes,
    consistent with an implacable, non-negotiating extraction mechanism.
  - C3.coordination = false: no bargaining surface exists. There is no principal-agent
    structure to satisfice within; the predator does not require legitimation, consent,
    or Pareto-improving negotiation to continue extracting.
  - The omega (power_scaling) demonstrates that agent-level power differences — which
    would matter under BIR's satisficing/bargaining logic — are actively collapsed to
    a uniform powerless/trapped index once C3 activates. This collapse is inconsistent
    with a bounded-rationality negotiation model, where heterogeneous agent power
    should produce heterogeneous outcomes.
  - C1's Mountain status (naturally emergent, zero suppression) supplies the precondition
    for C3's implacability: no amount of bargaining resolves a Mountain within the
    relevant time horizon.

  BIR is rejected: there is no institution here capable of satisficing — C3 has
  coordination=false, meaning it supplies no coordination value to trade against its
  extraction, which is the defining feature BIR-compatible constraints require
  (cf. Tangled Rope / Rope profiles, not present at the terminal node).
```

---

## 6. TERMINAL ATTRACTOR SELECTION

```
Selected: ■ Deterministic Tragedy

Constraint profile: Mountain (C1) + Snare (C2 at X1's index) + Snare (C3, all agents)
→ "Mountain + Snares" row of Attractor Compatibility Matrix
→ PIR-compatible cell: Deterministic Tragedy (only listed compatible attractor for this row under PIR; N/A under BIR)

Justification:
  - C1 is a certified Mountain (Boltzmann PASS) feeding directly into C3: the
    ignorance-substrate cannot be dissolved by any in-graph action within the
    relevant time horizons, and this immutability is load-bearing for C3's function.
  - C3 is Snare at χ=1.20 for every tested agent, with zero indexical variance —
    there is no higher-power index within the manifest at which C3 resolves to
    Rope (contrast with C2, which does have such an index at X2/institutional).
    Snare-without-escape-index is the signature of Deterministic Tragedy rather
    than Negotiated Equilibrium.
  - TR4 (organize_collective) is formally computable as a path to Rope but is
    proven structurally unreachable given the C1∧C2∧C6 blocking set — this is
    precisely the "constraints run to completion" signature: an escape exists
    in the formal space but not in the reachable trajectory space.
  - Rejected alternatives:
    - Negotiated Equilibrium requires a bargaining surface; C3.coordination=false
      forecloses this categorically.
    - Revolutionary Rupture requires disruption of the constraint logic itself;
      no transformation rule in this graph achieves that (TR1–TR3 adjust magnitude
      or partially transform upstream nodes without severing C3's dependency on them).
    - Seeded Possibility requires either a Piton (none certified in this manifest)
      or underground transformation surviving surface tragedy; the omega's uniform
      collapse of all three agents to identical trapped/powerless index leaves no
      differentiated agent positioned to carry a seed forward.
```

---

## 7. VALIDATION CHECKLIST

```
☑ All Stage 0 constraints formalized with ε, Supp, Coord, Asym (C1, C2, C3)
☑ All χ calculations shown with π and σ values, per agent
☑ All characters use variable names (X1–X4) — no source identifiers in body
☑ Variable mapping table present in header only
☑ No source occupation, setting, or domain vocabulary anywhere in output
☑ No <experience>, <dialogue_markers>, or <narrative> fields present
☑ Transformation rules (TR1–TR4) use abstract trigger descriptions
☑ Error observables (E1–E4) are testable structural conditions
☑ Institutional rationality model (PIR) specified with justification
☑ Terminal attractor (Deterministic Tragedy) selected, justified, matrix-compatible
☑ Indexical variance explicitly preserved: C1 (none), C2 (maximal, X1 vs X2), C3 (collapsed by omega)
☑ Boltzmann test run for C1 (PASS — Mountain certified); C2/C3 correctly not Mountain-tested (categorical disqualification noted)
☑ No ambiguity in specifications
☑ Output is pure symbolic network — source work unidentifiable from body text
```