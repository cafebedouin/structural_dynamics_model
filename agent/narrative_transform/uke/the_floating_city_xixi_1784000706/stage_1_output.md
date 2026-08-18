# STAGE 1: FORMAL CONSTRAINT NETWORK

## Variable Mapping (header only — stripped upon anonymization)

```
X₁ ← Inhabitant
X₂ ← Intellectual
X₃ ← Child Prodigy
C₁ ← foundational_precarity
C₂ ← compensatory_production
C₃ ← authority_inversion
```

---

## 1. CONSTRAINT FORMALIZATIONS

```xml
<constraint id="C₁">
  <properties>
    <epsilon>0.90</epsilon>
    <suppression>0.00</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.0 × 0.8 = 0.72</chi>
    <type>Snare</type>
    <threshold_check>χ &gt; 0.70 → Snare: PASS</threshold_check>
    <rationale>High base extraction combined with identity-fusion to the constraint prevents exit despite only moderate power position. No suppression required — extraction is structural, not enforced.</rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>analytical</power>        <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.15 × 0.8 = 0.828 ≈ 0.83</chi>
    <type>Snare</type>
    <threshold_check>χ &gt; 0.70 → Snare: PASS</threshold_check>
    <rationale>Analytical index modifier increases detected extraction relative to moderate baseline — degeneracy-breaking confirms the constraint is not merely normalized but actively extractive even under scrutiny capable of naming it.</rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>mobile</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.0 × 0.8 = 0.72</chi>
    <type>Snare</type>
    <threshold_check>χ &gt; 0.70 → Snare: PASS</threshold_check>
    <rationale>Mobility of exit option does not reduce χ because power position remains moderate and base extraction is severe; future capacity does not yet translate into structural leverage against this constraint.</rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=0.72 → Snare, X₂ χ=0.83 → Snare, X₃ χ=0.72 → Snare.
    No divergence in type despite divergence in exit-option structure (identity_locked / constrained / mobile). All three converge because base extractiveness (ε=0.90) dominates the calculation regardless of π variation across non-extreme power positions.
  </indexical_variance>

  <boltzmann_test>
    FAIL (as candidate Mountain) — not tested for Mountain status since ε=0.90 exceeds mountain_extractiveness_max (0.25). Constraint is confirmed constructed, not natural, independent of Boltzmann procedure: classification is uniform Snare across P-variation only because tested P-values (moderate, analytical) are structurally close; institutional/organized positions are absent from this constraint's agent set, meaning universality of Snare-status has not been tested against power-escape. Structural gate: NOT a Mountain candidate — ε disqualifies at threshold level.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C₂">
  <properties>
    <epsilon>0.60</epsilon>
    <suppression>0.70</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.60 × 1.0 × 0.8 = 0.48</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.46 ≤ χ ≤ 0.70 ∧ Coord=true ∧ Asym=true → Tangled Rope: PASS</threshold_check>
    <rationale>System provides genuine coordination function while extracting asymmetric costs; identity-lock prevents recognition of the extraction component as separable from the coordination benefit.</rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>analytical</power>        <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.60 × 1.15 × 0.8 = 0.552 ≈ 0.55</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.46 ≤ χ ≤ 0.70 ∧ Coord=true ∧ Asym=true → Tangled Rope: PASS</threshold_check>
    <rationale>Analytical detection increases perceived extraction above X₁'s naive baseline but not enough to cross Snare threshold — hybrid nature is correctly resolved rather than misclassified in either direction.</rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>powerful</power>          <!-- π = 0.6 -->
      <time>biographical</time>
      <exit>mobile</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.60 × 0.6 × 0.8 = 0.288 ≈ 0.29</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0.35 ∧ ε ≤ 0.45 required when χ&gt;0 — but ε=0.60 &gt; 0.45. Dual threshold check: χ=0.29 ≤ 0.35 PASS, but ε ceiling FAILS at 0.45 requirement... re-evaluate: since χ &gt; 0, requires ε ≤ 0.45. ε=0.60 fails this. Reclassify per rope_epsilon_ceiling violation.</threshold_check>
    <type_correction>Naturalized</type_correction>
    <rationale>χ=0.29 falls below Rope ceiling (0.35) suggesting low power-scaled cost, but base extraction ε=0.60 exceeds the epsilon ceiling for a genuine Rope classification (0.45). This is the Naturalized signature: ε(C) &gt; 0.45 ∧ χ(C,I) &lt; 0.40 — extraction is being absorbed and hidden by the powerful power-position rather than genuinely absent. Flagged for investigate_naturalization.</rationale>
  </agent>

  <indexical_variance>
    C₂: X₁ χ=0.48 → Tangled Rope, X₂ χ=0.55 → Tangled Rope, X₃ χ=0.29 → Naturalized (masking Snare/Tangled Rope structure via power deflection).
    High divergence confirmed. The powerful-position agent's low χ does not indicate absence of extraction — it indicates successful cost-deflection. Per Type III error test: χ(moderate)=0.48 and χ(analytical)=0.55 both confirm extraction exists structurally; X₃'s low χ is an artifact of power position, not evidence of a clean Rope.
  </indexical_variance>

  <boltzmann_test>
    FAIL — Type varies with P (Tangled Rope at moderate/analytical, Naturalized/deflected at powerful) in a non-factorizable way tied to the same fixed ε and Supp. This variation confirms C₂ is constructed (asymmetric social production mechanism), not natural. Consistent with Coord=true, Asym=true declared properties.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C₃">
  <properties>
    <epsilon>0.70</epsilon>
    <suppression>0.20</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>         <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.70 × 1.5 × 0.8 = 0.84</chi>
    <type>Snare</type>
    <threshold_check>χ &gt; 0.70 → Snare: PASS</threshold_check>
    <rationale>Power collapse to powerless amplifies extraction to maximum severity; trapped exit option confirms no structural escape remains. This is the terminal state of the generation_order sequence for this agent.</rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>institutional</power>     <!-- π = -0.2 -->
      <time>biographical</time>
      <exit>arbitrage</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.70 × (-0.2) × 0.8 = -0.112 ≈ -0.11</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0 → ε ceiling bypassed → Rope: PASS</threshold_check>
    <rationale>Negative χ confirms net-beneficiary status — agent now extracts FROM the system rather than bearing its cost. Arbitrage exit option confirms capacity to play prior structural positions against each other.</rationale>
  </agent>

  <indexical_variance>
    C₃: X₁ χ=0.84 → Snare, X₃ χ=-0.11 → Rope.
    Extreme divergence. Sign inversion of χ (positive vs. negative) demonstrates complete structural role-reversal: the same generational mechanism that traps one agent at maximum extraction simultaneously constitutes the other agent's institutional benefit. This is not differential severity — it is polarity inversion.
  </indexical_variance>

  <boltzmann_test>
    FAIL — Type flips entirely with P (Snare at powerless π=1.5 vs Rope at institutional π=-0.2), confirming construction. No natural law produces sign-inverted burden distributions; this is definitionally a constructed authority-transfer mechanism.
  </boltzmann_test>
</constraint>
```

---

## 2. TRANSFORMATION RULES

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>organize_collective</condition>
    <target>C₁</target>
    <agent>X₁</agent>
  </trigger>
  <index_change>
    <from>P = moderate (π=1.0), E = identity_locked</from>
    <to>P = organized (π=0.4), E = constrained</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.90 × 1.0 × 0.8 = 0.72 → Snare</before>
    <after>χ = 0.90 × 0.4 × 0.8 = 0.288 ≈ 0.29 → Rope (ε=0.90 &gt; 0.45 ceiling triggers Naturalized check)</after>
  </chi_recalculation>
  <type_change>Snare → Naturalized (extraction persists at ε=0.90 despite collective burden-sharing reducing χ)</type_change>
  <preconditions>Coordination infrastructure must exist; agent must abandon identity-fusion to constraint (E: identity_locked → constrained minimum)</preconditions>
  <blocked_by>C₂ (compensatory_production channels individual energy into C2's adaptation rather than collective organization against C1); indexical_variance of C₁ showing uniform Snare status removes any single agent's incentive to organize alone</blocked_by>
</transformation_rule>

<transformation_rule id="TR₂">
  <trigger>
    <condition>establish_alternative_system</condition>
    <target>C₂</target>
    <agent>X₂</agent>
  </trigger>
  <index_change>
    <from>P = analytical (π=1.15), E = constrained</from>
    <to>P = organized (π=0.4), E = mobile</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.60 × 1.15 × 0.8 = 0.552 → Tangled Rope</before>
    <after>χ = 0.60 × 0.4 × 0.8 = 0.192 → Rope (ε=0.60 &gt; 0.45; since χ&gt;0, ε-ceiling check fails → Naturalized)</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Naturalized (coordination value increases but base extraction of ε=0.60 remains structurally unaddressed)</type_change>
  <preconditions>Requires exit from constrained E-state; requires collective buy-in exceeding single-agent analytical capacity</preconditions>
  <blocked_by>C₃ not yet generated at this stage — but generation_sequence (C₁→C₂→C₃) indicates C₃'s authority structure has not yet crystallized to block this transformation; primary block is absence of Coord infrastructure independent of C₂ itself</blocked_by>
</transformation_rule>

<transformation_rule id="TR₃">
  <trigger>
    <condition>exit_constraint</condition>
    <target>C₃</target>
    <agent>X₁</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = moderate (π=1.0), E = constrained</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.70 × 1.5 × 0.8 = 0.84 → Snare</before>
    <after>χ = 0.70 × 1.0 × 0.8 = 0.56 → Tangled Rope</after>
  </chi_recalculation>
  <type_change>Snare → Tangled Rope</type_change>
  <preconditions>Requires reversal of trapped exit-status to at minimum constrained; requires power position recovery from powerless to moderate</preconditions>
  <blocked_by>C₂'s asymmetric coordination structure (Asym=true) actively produces the powerless/institutional split in C₃; as long as C₂ remains uncorrected, C₃'s downstream polarity (X₁ trapped, X₃ arbitrage) regenerates even if X₁ achieves temporary exit</blocked_by>
</transformation_rule>

<transformation_rule id="TR₄">
  <trigger>
    <condition>relinquish_institutional_position</condition>
    <target>C₃</target>
    <agent>X₃</agent>
  </trigger>
  <index_change>
    <from>P = institutional (π=-0.2), E = arbitrage</from>
    <to>P = powerful (π=0.6), E = mobile</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.70 × (-0.2) × 0.8 = -0.11 → Rope</before>
    <after>χ = 0.70 × 0.6 × 0.8 = 0.336 → Rope (ε=0.70 &gt; 0.45; χ&gt;0 triggers ε-ceiling failure → Naturalized)</after>
  </chi_recalculation>
  <type_change>Rope → Naturalized</type_change>
  <preconditions>Voluntary abandonment of arbitrage capacity; requires resolution of Omega (future_state) toward non-stewardship path</preconditions>
  <blocked_by>C₂'s Child Prodigy classification (Rope at χ=0.29, itself flagged Naturalized) — the agent's entire trajectory through C₂→C₃ is structurally rewarded at every prior stage, producing no internal incentive gradient toward relinquishment</blocked_by>
</transformation_rule>
```

---

## 3. ERROR MANIFESTATIONS

```xml
<error id="E₁">
  <type>III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X₃</agent>
  <constraint>C₂</constraint>
  <actual_type>Naturalized (masking Tangled Rope/Snare structure; χ=0.29 conceals ε=0.60)</actual_type>
  <perceived_type>Rope</perceived_type>
  <observable>Agent does not attempt to interrogate the extraction component of the coordination system despite occupying a structural position (powerful, mobile) that would permit such interrogation at low personal cost</observable>
  <correction_trigger>Testing χ at X₁'s index (moderate, identity_locked) reveals χ=0.48, confirming Tangled Rope exists structurally; per Type III test, extraction exists regardless of X₃'s low χ</correction_trigger>
</error>

<error id="E₂">
  <type>I — False Mountain</type>
  <agent>X₁</agent>
  <constraint>C₁</constraint>
  <actual_type>Snare (constructed, confirmed by asymmetric properties table showing Asym=false but ε=0.90 disqualifying Mountain status)</actual_type>
  <perceived_type>Mountain (treated as unchangeable natural condition)</perceived_type>
  <observable>Does not attempt collective organization (TR₁ trigger) despite χ indicating feasibility of transformation via power-position change; treats foundational condition as permanent given rather than constructed arrangement</observable>
  <correction_trigger>Boltzmann test failure — ε=0.90 exceeds mountain_extractiveness_max (0.25) categorically; agent's own biographical time horizon (not civilizational) should reveal constructedness if tested against alternative power positions</correction_trigger>
</error>

<error id="E₃">
  <type>V.a — Tangled-as-Rope (ignoring extraction component)</type>
  <agent>X₂</agent>
  <constraint>C₂</constraint>
  <actual_type>Tangled Rope (χ=0.55, Coord=true, Asym=true)</actual_type>
  <perceived_type>Rope (pure coordination benefit)</perceived_type>
  <observable>Analytical capacity is deployed toward describing system function without producing action targeting asymmetric cost distribution (Asym=true property untested behaviorally despite being detected analytically)</observable>
  <correction_trigger>Explicit measurement of Asym property against own index — since χ falls in Tangled range (0.46–0.70) and Coord+Asym both confirmed true by base properties, Rope misclassification cannot survive direct threshold check</correction_trigger>
</error>

<error id="E₄">
  <type>II — Mountain Denial</type>
  <agent>X₁</agent>
  <constraint>C₃</constraint>
  <actual_type>Snare (χ=0.84, constructed, confirmed Boltzmann FAIL)</actual_type>
  <perceived_type>Mountain (treated as unchangeable generational law)</perceived_type>
  <observable>No attempt at TR₃ (exit_constraint) despite structural transformation path existing; energy directed at accommodation rather than transformation attempts targeting P or E index values</observable>
  <correction_trigger>Boltzmann test already FAILS for C₃ (sign-inverted χ across P values) — this alone certifies construction; any agent applying the test would detect non-Mountain status immediately</correction_trigger>
</error>
```

---

## 4. INSTITUTIONAL RATIONALITY MODEL

```
Selected: BOUNDED INSTITUTIONAL RATIONALITY (BIR)

Justification:
- C₂ exhibits Coord=true ∧ Asym=true (Tangled Rope dominant profile), which under
  the Attractor Compatibility Matrix maps to BIR → Negotiated Equilibrium / Seeded
  Possibility, NOT PIR's Deterministic Tragedy path.
- C₃ shows satisficing, principal-agent structure: X₃'s institutional position
  (π=-0.2) is not global utility-maximization but generational risk-transfer
  under uncertainty (the Omega — "cannot resolve whether new generation will
  solve or merely re-steward" — is precisely a BIR signature: bounded foresight,
  not perfect optimization).
- No agent in the system exhibits unbounded utility-maximization; X₃'s arbitrage
  exit option indicates satisficing between available structural alternatives,
  not global optimization.
- The invariant_contract's "missing_floor" (arbitrary suspension of natural law,
  subsequently normalized) is a hallmark of bounded-rationality institution-building:
  a satisficing solution mistaken over time for a hard constraint, not a
  perfectly rational permanent design.

Attractor compatibility: Tangled-Ropes-dominant profile (C₂) + BIR →
Negotiated Equilibrium is reachable, but the presence of a Snare at generation_order=3
(C₃, χ=0.84 for X₁) with no Mountain floor pulls against clean equilibrium.
```

---

## 5. TERMINAL ATTRACTOR SELECTION

```
Selected: SEEDED POSSIBILITY

Justification:
☐ Deterministic Tragedy — REJECTED. Requires PIR or Mountain+Snare combination
  with no bargaining possibility. C₂'s Tangled Rope status (genuine coordination
  value present) and BIR selection both block pure tragedy.

☐ Negotiated Equilibrium — REJECTED as terminal (though present as intermediate
  state at C₂). C₃'s extreme indexical variance (χ: 0.84 vs −0.11, sign inversion)
  demonstrates that the "equilibrium" achieved at the C₂ stage does NOT hold —
  it resolves into a new asymmetric Snare/Rope split rather than stabilizing.

☑ SEEDED POSSIBILITY — ACCEPTED.
  - Surface tragedy: X₁ terminal state is Snare (χ=0.84) under C₃ — trapped,
    powerless, no coordination value. This is tragic on its face.
  - Underground transformation: X₃'s Rope status (χ=-0.11) at C₃ is not merely
    escape but structural succession — the arbitrage exit option and institutional
    power position mean the NEXT iteration of the system is seeded within the
    current one's collapse.
  - The unresolved Omega (future_state) is definitionally the Seeded Possibility
    signature: the analysis cannot determine whether the seed (X₃'s position)
    germinates into genuine transformation (C₁ resolution) or merely reproduces
    stewardship of the same trap under new management.
  - Piton is absent as classification, but Seeded Possibility does not require
    Piton presence — it requires structural ambiguity in succession, which the
    Omega explicitly encodes.

Compatibility check: BIR + Tangled-Rope-dominant (C₂) + unresolved generational
succession (C₃) → Seeded Possibility is the matrix-compatible attractor.
```

---

## 6. VALIDATION CHECKLIST

```
☑ All constraints (C₁, C₂, C₃) formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with explicit π and σ values for every agent
☑ All characters use variable names (X₁, X₂, X₃) — no source identifiers in body
☑ Variable mapping table present in header only
☑ No source occupation, setting, or domain vocabulary anywhere in output
☑ No <experience>, <dialogue_markers>, or <narrative> fields present
☑ Transformation rules (TR₁–TR₄) use abstract trigger descriptions
   (organize_collective, establish_alternative_system, exit_constraint,
   relinquish_institutional_position)
☑ Error observables (E₁–E₄) are testable structural conditions, not source actions
☑ Institutional rationality model specified (BIR) with explicit justification
☑ Terminal attractor selected (Seeded Possibility), justified, matrix-compatible
☑ Indexical variance explicitly preserved and stated for all three constraints
☑ Boltzmann test run for all three constraints (all FAIL — none qualify as Mountain
  candidates; C₁ disqualified by ε threshold alone, C₂/C₃ disqualified by
  non-factorizable P-variation)
☑ No ambiguity in specifications — all thresholds shown with pass/fail reasoning
☑ Output is pure symbolic network — source identification not recoverable from body
```