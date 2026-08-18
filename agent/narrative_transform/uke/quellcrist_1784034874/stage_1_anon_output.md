# STAGE 1: FORMALIZATION OUTPUT

## Variable Mapping (Header Only — Stripped Post-Anonymization)

```
C1 ← Recognition_as_Threat
C2 ← Impersonal_Harm
C3 ← Systemic_Liquidation
C4 ← Instrumental_Justice [deferred]
C5 ← Atomization [deferred]
C6 ← Personalized_Resistance [deferred]
```

---

## 1. CONSTRAINT FORMALIZATIONS

```xml
<constraint id="C1">
  <properties>
    <epsilon>0.45</epsilon>
    <suppression>0.40</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>national</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.45 × 1.5 × 1.0 = 0.675</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.46 ≤ 0.675 ≤ 0.70 → PASS (Tangled Rope band)</threshold_check>
    <rationale>Moderate power-scaled extraction combined with true coordination and true asymmetry satisfies the hybrid gate. The classification rule that sorts agents into disposable/non-disposable categories provides real coordination (predictability of engagement) while extracting asymmetric cost from the low-power position.</rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>institutional</power>       <!-- π = -0.2 -->
      <time>biographical</time>
      <exit>arbitrage</exit>
      <scope>national</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.45 × -0.2 × 1.0 = -0.09</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0 → ε ceiling bypassed → PASS (Rope, net-beneficiary case)</threshold_check>
    <rationale>Negative χ indicates net extraction FROM the system rather than by it. Institutional exit option (arbitrage) permits this agent class to leverage the same classificatory rule as a coordination mechanism for consolidating position.</rationale>
  </agent>

  <indexical_variance>
    C1: X₁ χ=0.675 → Tangled Rope, X₂ χ=-0.09 → Rope
    Divergence magnitude: Δχ = 0.765. Same formal rule (binary classification of persons into disposable/non-disposable categories) is simultaneously a hybrid extraction-coordination structure for the low-power index and a pure coordination benefit for the institutional index. Sign flip on χ confirms directional asymmetry, not merely magnitude asymmetry.
  </indexical_variance>

  <boltzmann_test>FAIL — Type(C1, X₁) ≠ Type(C1, X₂) under P-variation alone (S held constant at national/1.0). Classification is non-factorizable across power axis. Confirms C1 is constructed, not natural. Not eligible for Mountain status regardless of ε/Supp values.</boltzmann_test>
</constraint>
```

```xml
<constraint id="C2">
  <properties>
    <epsilon>0.70</epsilon>
    <suppression>0.60</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>national</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.70 × 1.5 × 1.0 = 1.05</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.05 > 0.70, ¬LowBaseExtraction (ε=0.70 high) → PASS (Snare)</threshold_check>
    <rationale>High power-scaled extraction with high base extractiveness confirms genuine Snare — not an artifact of power amplification on a low-ε constraint. The interpretive frame legitimizing harm functions as pure extraction trap from this index: no coordination benefit accrues to the low-power class, only cost absorption.</rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>institutional</power>       <!-- π = -0.2 -->
      <time>biographical</time>
      <exit>arbitrage</exit>
      <scope>national</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.70 × -0.2 × 1.0 = -0.14</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0 → ε ceiling bypassed → PASS (Rope, net-beneficiary case)</threshold_check>
    <rationale>Same interpretive frame operates as coordinating narrative infrastructure for the institutional class — it organizes justificatory consensus among authoring agents rather than extracting from them.</rationale>
  </agent>

  <indexical_variance>
    C2: X₁ χ=1.05 → Snare, X₂ χ=-0.14 → Rope
    Divergence magnitude: Δχ = 1.19 — largest single-constraint divergence in the network. This is the canonical Type III error site (Snare-as-Rope): the constraint is normalized as legitimate coordination by its authors while functioning as a pure extraction trap for its targets.
  </indexical_variance>

  <boltzmann_test>FAIL — Type(C2, X₁) ≠ Type(C2, X₂) under P-variation. Non-factorizable across power axis. Constructed, not natural.</boltzmann_test>
</constraint>
```

```xml
<constraint id="C3">
  <properties>
    <epsilon>1.00</epsilon>
    <suppression>0.90</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>national</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 1.00 × 1.5 × 1.0 = 1.50</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.50 > 0.70, ¬LowBaseExtraction (ε=1.00 maximal) → PASS (Snare, maximal case)</threshold_check>
    <rationale>Maximal base extraction combined with maximal power-scaling produces the network's terminal extraction value. This is the downstream convergence point of C1 and C2 — the mechanism by which classification (C1) and justification (C2) resolve into elimination.</rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>institutional</power>       <!-- π = -0.2 -->
      <time>biographical</time>
      <exit>arbitrage</exit>
      <scope>national</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 1.00 × -0.2 × 1.0 = -0.20</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0 → ε ceiling bypassed → PASS (Rope, net-beneficiary case)</threshold_check>
    <rationale>From the institutional index, the elimination mechanism functions as a coordinating tool of statecraft — a Rope enabling consolidation, tension resolution, and power maintenance, entirely decoupled from the cost it imposes elsewhere.</rationale>
  </agent>

  <indexical_variance>
    C3: X₁ χ=1.50 → Snare, X₂ χ=-0.20 → Rope
    Divergence magnitude: Δχ = 1.70 — maximal divergence in the network, consistent with C3's status as terminal/highest-centrality node. The sign inversion at this node is the structural signature of a constraint whose entire function is redistributive: value transferred from X₁ to X₂ with no shared coordination surplus.
  </indexical_variance>

  <boltzmann_test>FAIL — Type(C3, X₁) ≠ Type(C3, X₂) under P-variation. Non-factorizable. Constructed. Given generation_order and downstream position (fed by C1, C2), C3 is confirmed as the terminal extraction node of the chain rather than an independent constraint.</boltzmann_test>
</constraint>
```

---

## 2. TRANSFORMATION RULES

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>exit_constraint</condition>
    <target>C1</target>
    <agent>X₁</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = organized (π=0.4), E = constrained</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.45 × 1.5 × 1.0 = 0.675 → Tangled Rope</before>
    <after>χ = 0.45 × 0.4 × 1.0 = 0.18 → Rope</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Rope</type_change>
  <preconditions>Collective coordination achieved among X₁-class agents; C5 (Atomization) not dominant.</preconditions>
  <blocked_by>C5 (Atomization) — if active, prevents transition from powerless to organized index by suppressing collective formation.</blocked_by>
</transformation_rule>

<transformation_rule id="TR2">
  <trigger>
    <condition>organize_collective</condition>
    <target>C2</target>
    <agent>X₁</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5)</from>
    <to>P = organized (π=0.4)</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.70 × 1.5 × 1.0 = 1.05 → Snare</before>
    <after>χ = 0.70 × 0.4 × 1.0 = 0.28 → Rope</after>
  </chi_recalculation>
  <type_change>Snare → Rope</type_change>
  <preconditions>Collective visibility sufficient to disrupt unilateral narrative authorship; requires TR1 precondition satisfied first (sequential dependency, since C1 feeds into C3 alongside C2).</preconditions>
  <blocked_by>C4 (Instrumental_Justice) — legitimizing theater apparatus may re-absorb organized resistance into a new justificatory frame, resetting χ upward.</blocked_by>
</transformation_rule>

<transformation_rule id="TR3">
  <trigger>
    <condition>establish_alternative_system</condition>
    <target>C3</target>
    <agent>X₁</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = organized (π=0.4), E = mobile</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 1.00 × 1.5 × 1.0 = 1.50 → Snare</before>
    <after>χ = 1.00 × 0.4 × 1.0 = 0.40 → Rope</after>
  </chi_recalculation>
  <type_change>Snare → Rope</type_change>
  <preconditions>Requires successful upstream transformation of BOTH C1 (TR1) and C2 (TR2), since C3 is fed by both. Sequential/conjunctive dependency, not independent.</preconditions>
  <blocked_by>C3's suppression value (0.90) is the highest in the network — enforcement intensity scales with proximity to terminal node, making direct exit attempts at C3 without upstream transformation highly likely to trigger immediate reclassification back to Snare.</blocked_by>
</transformation_rule>

<transformation_rule id="TR4">
  <trigger>
    <condition>individual_confrontation</condition>
    <target>C3</target>
    <agent>X₁</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = powerless (π=1.5), E = trapped [unchanged — individual action does not alter structural index]</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 1.00 × 1.5 × 1.0 = 1.50 → Snare</before>
    <after>χ = 1.00 × 1.5 × 1.0 = 1.50 → Snare [no change]</after>
  </chi_recalculation>
  <type_change>Snare → Snare (no transformation occurs)</type_change>
  <preconditions>None required — this is the null-transformation case.</preconditions>
  <blocked_by>Individual-index action (C6, Personalized_Resistance) cannot alter π because π is a function of collective/structural position, not individual behavior. This formalizes the Ω uncertainty: personalized confrontation leaves index unchanged, meaning χ remains at Snare level regardless of behavioral intensity. Whether this produces visibility-as-player is empirically undetermined by the index mechanics alone — see Error E3 and Omega register.</blocked_by>
</transformation_rule>
```

---

## 3. ERROR MANIFESTATIONS

```xml
<error id="E1">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X₂</agent>
  <constraint>C2</constraint>
  <actual_type>Snare (at X₁ index, χ=1.05)</actual_type>
  <perceived_type>Rope (X₂ normalizes at own index, χ=-0.14, without checking X₁ index)</perceived_type>
  <observable>X₂-class agents author or endorse the justificatory narrative without evaluating χ at powerless index; narrative is treated as self-evidently coordinating rather than tested against victim-index extraction value.</observable>
  <correction_trigger>Computing χ(C2, X₁) explicitly — any procedure requiring impact assessment at the lowest-power index would immediately surface χ=1.05 > 0.70 threshold, forcing Snare recognition regardless of authorial intent.</correction_trigger>
</error>

<error id="E2">
  <type>Type I — False Mountain</type>
  <agent>X₁</agent>
  <constraint>C1</constraint>
  <actual_type>Tangled Rope (χ=0.675, changeable via TR1 given organized index shift)</actual_type>
  <perceived_type>Mountain (treated as immutable natural sorting of persons)</perceived_type>
  <observable>Does not attempt collective reorganization despite χ falling in Tangled Rope band (0.46–0.70), which by definition indicates a changeable hybrid rather than fixed terrain; classification of persons into disposable/non-disposable categories treated as given rather than constructed.</observable>
  <correction_trigger>Boltzmann test result (FAIL) demonstrates non-factorizability across power axis — this is the formal signature that should trigger reclassification from Mountain to constructed-and-changeable.</correction_trigger>
</error>

<error id="E3">
  <type>Type II — Mountain Denial (candidate) / Unresolved</type>
  <agent>X₁</agent>
  <constraint>C3</constraint>
  <actual_type>Indeterminate — TR4 shows individual-level action does not change π, so whether continued individual confrontation constitutes denial of an effectively fixed constraint (given trapped/biographical/powerless index) or a rational bet on rare index-jump cannot be resolved by χ mechanics alone.</actual_type>
  <perceived_type>Treated by X₁ as potentially transformative (belief that sufficient individual action produces institutional-index recognition).</perceived_type>
  <observable>X₁ persists in individual-index confrontation against C3 despite TR4 showing null structural transformation; no χ recalculation occurs across repeated trigger events.</observable>
  <correction_trigger>Formally unresolvable within current index framework — this is the Ω register item. Correction would require either (a) empirical evidence of index-jump probability under sustained individual action, or (b) a transformation rule not currently derivable from C1–C3 formalization, e.g., a stochastic path from individual to organized index.</correction_trigger>
</error>

<error id="E4">
  <type>Type V.b — Tangled-as-Snare (Missing Coordination)</type>
  <agent>X₁</agent>
  <constraint>C1</constraint>
  <actual_type>Tangled Rope (χ=0.675 — coordination component present per Coord=true)</actual_type>
  <perceived_type>Pure Snare (coordination value of the classificatory rule ignored; treated as if it had zero function beyond extraction)</perceived_type>
  <observable>Strategic responses to C1 (e.g., TR2/TR3 pathways) are not pursued because the classification is treated as if reform is impossible (pure extraction, no coordination surplus to renegotiate) rather than as a hybrid with a genuine — if asymmetric — coordination function that could be rebalanced.</observable>
  <correction_trigger>Recognition that χ falls strictly within the 0.46–0.70 Tangled Rope band, not above the 0.70 Snare threshold, mechanically implies a coordination component exists and is theoretically renegotiable via index shift (TR1), not just escapable or destroyable.</correction_trigger>
</error>
```

---

## 4. INSTITUTIONAL RATIONALITY MODEL

```
Model selected: PERFECT INSTITUTIONAL RATIONALITY (PIR)

Justification:
- X₂ (institutional) index shows χ < 0 across all three constraints (-0.09, -0.14, -0.20),
  indicating consistent net-beneficiary status with no observed negotiation, satisficing,
  or risk-averse moderation.
- C3 exhibits ε=1.00 (maximal) and Supp=0.90 (near-maximal) — consistent with utility
  maximization unbounded by principal-agent friction or risk aversion.
- Generation sequence C1→C2→C3 shows monotonic increase in ε (0.45→0.70→1.00) and
  Supp (0.40→0.60→0.90), indicating escalation toward unbounded extraction rather than
  equilibrium-seeking or bargaining behavior.
- No evidence in constraint manifest of Pareto-improving negotiation between X₁ and X₂
  indices — all indexical variance is characterized as "high" with no convergence.
- BIR would predict satisficing behavior or negotiated de-escalation; the monotonic
  ε/Supp escalation across generation_order contradicts this.

Attractor implications under PIR: Deterministic Tragedy or Revolutionary Rupture
(per Compatibility Matrix — "Pure Snares, no Mountains" + PIR → Tragedy or Rupture).
```

---

## 5. TERMINAL ATTRACTOR SELECTION

```
Constraint profile summary:
  C1: Tangled Rope (X₁) / Rope (X₂) — Boltzmann FAIL
  C2: Snare (X₁) / Rope (X₂) — Boltzmann FAIL
  C3: Snare (X₁) / Rope (X₂) — Boltzmann FAIL

Profile classification: No Mountains present (all three constraints fail Boltzmann
independence test — all constructed). Dominant terminal-node type at highest-centrality
constraint (C3) is Snare from the affected index. Network shows progressive degradation
consistent with natural transition direction: Tangled Rope (C1) → Snare (C2) → Snare,
maximal (C3), matching T2 (Tangled → Snare, coordination loss) as generation_order
advances.

Attractor selected: ☑ DETERMINISTIC TRAGEDY

Justification:
- Profile = "Pure Snares, no Mountains" + PIR → per Compatibility Matrix, compatible
  attractors are {Deterministic Tragedy, Revolutionary Rupture}.
- Revolutionary Rupture requires disruption of the constraint logic itself — TR4
  (individual confrontation) demonstrates that individual-index action produces NO
  index change and NO χ recalculation, formally blocking the rupture pathway via
  personalized resistance (C6 hypothesis).
- TR1/TR2/TR3 show that rupture-scale transformation (Snare→Rope) is theoretically
  reachable but requires sequential, conjunctive, collective-index transformation
  (organize_collective, establish_alternative_system) — none of which are evidenced
  as occurring in the manifest; only C6 (individual/personal resistance philosophy)
  is flagged as active in the deferred set.
- The Missing Floor invariant (partition into ends-in-themselves vs. disposable-means)
  is explicitly marked present/primary — this is the defining feature of Deterministic
  Tragedy profiles: the terminal node (C3) is not a side effect but the designed output
  of the upstream chain.
- Break Contract confirms LIVE prior status for the expectation that institutional
  channels are legitimate/exclusive — this expectation is violated in the underlying
  material, but the violation occurs at the level of individual action (C6) which TR4
  shows to be structurally inert at the index level. This produces tragedy-with-rupture-
  attempt: the rupture is attempted (via C6) but does not achieve index-level
  transformation, collapsing back into Deterministic Tragedy at the structural level.

Compatibility check: PIR + Pure-Snares-no-Mountains + Missing-Floor-primary →
Deterministic Tragedy is the mechanically consistent selection. Revolutionary Rupture
remains formally reachable only via TR1→TR2→TR3 chain (collective/organized index),
which is absent from observed trajectory — recorded as Ω (response_effectiveness)
rather than resolved.
```

---

## 6. OMEGA REGISTER (Unresolved)

```
Ω1 (response_effectiveness): TR4 formalization shows individual-index confrontation
produces null χ recalculation (χ stays at 1.50, Snare, before and after). This means
the index mechanics cannot determine whether repeated individual-level triggers
eventually produce a stochastic index-jump (unmodeled in current framework) or whether
the strategy is purely self-liquidating. This is not resolvable by extending χ
calculation — it requires either an additional transformation rule with probabilistic
index-jump conditions (not currently specified) or empirical/narrative resolution
outside the formal system (deferred to Stage 2+).
```

---

## VALIDATION CHECKLIST

```
☑ All Stage 0 constraints formalized with ε, Supp, Coord, Asym (C1, C2, C3)
☑ All χ calculations shown with π and σ values explicit
☑ All characters use variable names (X₁, X₂) — no source identifiers in body
☑ Variable mapping table present in header only
☑ No source occupation, setting, or domain vocabulary anywhere in output
☑ No <experience>, <dialogue_markers>, or <narrative> fields present
☑ Transformation rules use abstract trigger descriptions (exit_constraint,
  organize_collective, establish_alternative_system, individual_confrontation)
☑ Error observables are testable structural conditions
☑ Institutional rationality model specified (PIR) with explicit justification
☑ Terminal attractor selected (Deterministic Tragedy), justified, compatibility-checked
☑ Indexical variance explicitly preserved across agents for all three constraints
☑ Boltzmann test run for each constraint (all three: FAIL — none eligible for Mountain)
☑ No ambiguity in specifications — all χ values mechanically derived
☑ Output is pure symbolic network — source work unidentifiable from this document
```

<!-- ANONYMIZATION: 2 character names replaced with structural labels: Agent_A, Agent_B -->
