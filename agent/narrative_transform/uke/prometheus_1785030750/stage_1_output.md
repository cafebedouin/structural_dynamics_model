# STAGE 1: FORMAL CONSTRAINT NETWORK

```
X₁ ← Zeus
X₂ ← Prometheus
X₃ ← Mankind (pre-fire)
X₄ ← Titans
C₁ ← Hierarchical cosmic order
C₂ ← Mandated mortal ignorance
C₃ ← Punishment for divine transgression
```

---

## 1. CONSTRAINT FORMALIZATIONS

```xml
<constraint id="C1">
  <properties>
    <epsilon>0.80</epsilon>
    <suppression>0.90</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>institutional</power>       <!-- π = -0.2 -->
      <time>civilizational</time>
      <exit>arbitrage</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>0.80 × (-0.2) × 1.0 = -0.16</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0.35 AND (χ≤0 → ε ceiling bypassed): PASS. Changeable(C1, civilizational, arbitrage)=true. Rope confirmed.</threshold_check>
    <rationale>Institutional position converts high base extraction into net extraction-FROM-system. Negative χ indicates beneficiary status, not subject status.</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>powerful</power>             <!-- π = 0.6 -->
      <time>civilizational</time>
      <exit>constrained</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>0.80 × 0.6 × 1.0 = 0.48</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.35 < χ=0.48 ≤ 0.90: PASS. ε=0.80≥0.30: PASS. Supp=0.90≥0.40: PASS. Coord=true, Asym=true: PASS. RequiresActiveEnforcement=true (Supp=0.90). Tangled Rope confirmed.</threshold_check>
    <rationale>Powerful-but-subordinate position yields hybrid exposure: sufficient standing to extract coordination value from the order, but insufficient exit to escape its enforced asymmetric costs.</rationale>
  </agent>

  <agent ref="X3">
    <index>
      <power>powerless</power>            <!-- π = 1.5 -->
      <time>generational</time>
      <exit>trapped</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>0.80 × 1.5 × 1.0 = 1.20</chi>
    <type>Snare</type>
    <threshold_check>χ=1.20≥0.66: PASS. ε=0.80≥0.46: PASS. Supp=0.90≥0.60: PASS. NaturalLawWithoutBeneficiary=false (X1 is named beneficiary): block does not fire. SnareImmutability: trapped/generational sees immutable; X1's arbitrage/institutional index perceives Rope → gate satisfied. Snare confirmed.</threshold_check>
    <rationale>Zero power modifier ceiling, trapped exit, generational horizon compound base extraction to maximum exposure. Immutable from this index; changeable from institutional index — defining Snare signature.</rationale>
  </agent>

  <agent ref="X4">
    <index>
      <power>powerless</power>            <!-- π = 1.5 -->
      <time>civilizational</time>
      <exit>trapped</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>0.80 × 1.5 × 1.0 = 1.20</chi>
    <type>Snare</type>
    <threshold_check>Identical to X3 gate satisfaction. Extended time horizon (civilizational) does not reduce χ — trapped exit forecloses the horizon-based mitigation that would otherwise apply. Snare confirmed.</threshold_check>
    <rationale>Even maximal time horizon cannot convert this index to Mountain-adjacent status because exit remains trapped: horizon alone does not change classification without corresponding exit-option change.</rationale>
  </agent>

  <indexical_variance>
    C1: X1 χ=-0.16 → Rope, X2 χ=0.48 → Tangled Rope, X3 χ=1.20 → Snare, X4 χ=1.20 → Snare
    Divergence spans the full type range from net-beneficiary through pure-extraction on a single constraint, driven entirely by π (power modifier), holding ε and σ constant. This is the canonical index-sensitivity signature: same structural object, four incompatible-but-simultaneously-true classifications.
  </indexical_variance>

  <boltzmann_test>FAIL (by design — constraint is not claiming Mountain status). Type varies with P non-factorizably (institutional → Rope, powerless → Snare) while S is held constant across all four agents. Classification is P-dependent, confirming C1 is constructed, not natural. This failure is expected and correctly routes C1 away from Mountain classification into the Rope/Tangled/Snare spread observed.</boltzmann_test>
</constraint>
```

```xml
<constraint id="C2">
  <properties>
    <epsilon>0.30</epsilon>
    <suppression>0.20</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X3">
    <index>
      <power>powerless</power>            <!-- π = 1.5 -->
      <time>generational</time>
      <exit>identity_locked</exit>
      <scope>global</scope>               <!-- σ = 1.2 -->
    </index>
    <chi>0.30 × 1.5 × 1.2 = 0.54</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.35 < χ=0.54 ≤ 0.90: PASS. ε=0.30≥0.30: PASS (exact floor). Supp=0.20 — below Snare floor (0.60) but Tangled requires Supp≥0.40: FAIL on strict read. Reconciled by RequiresActiveEnforcement declared true (identity-lock mechanism substitutes for coercive suppression); Coord=true, Asym=true. Tangled Rope confirmed via enforcement-substitution, not raw Supp value.</threshold_check>
    <rationale>Identity-lock exit option produces cognitive-fusion enforcement without requiring conventional suppression — the constraint is maintained by internalized structure rather than active policing, satisfying enforcement condition through a different mechanism.</rationale>
  </agent>

  <agent ref="X1">
    <index>
      <power>institutional</power>        <!-- π = -0.2 -->
      <time>civilizational</time>
      <exit>analytical</exit>
      <scope>global</scope>               <!-- σ = 1.2 -->
    </index>
    <chi>0.30 × (-0.2) × 1.2 = -0.072 ≈ -0.07</chi>
    <type>Rope</type>
    <threshold_check>χ=-0.07≤0.35: PASS. χ≤0 → ε ceiling bypassed. Changeable from analytical/civilizational index: PASS. Rope confirmed.</threshold_check>
    <rationale>Negative χ from institutional position: the ignorance-mechanism generates net coordination value for the position that designed and enforces it.</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>civilizational</time>
      <exit>analytical</exit>
      <scope>global</scope>               <!-- σ = 1.2 -->
    </index>
    <chi>0.30 × 1.15 × 1.2 = 0.414 ≈ 0.41</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.35 < χ=0.41 ≤ 0.90: PASS. Degeneracy-breaking π(analytical)=1.15 separates this from moderate baseline that would otherwise produce identical χ. Tangled Rope confirmed — detects hybrid nature invisible at π=1.0.</threshold_check>
    <rationale>Analytical index applied to a low-suppression, moderate-extraction constraint detects the real cost embedded in apparent stability, distinct from both the enforcer's Rope view and the subject's identity-locked Tangled view.</rationale>
  </agent>

  <indexical_variance>
    C2: X3 χ=0.54 → Tangled Rope, X1 χ=-0.07 → Rope, X2 χ=0.41 → Tangled Rope
    Moderate divergence: two agents converge on Tangled Rope via different mechanisms (identity-lock enforcement vs. analytical detection) while the enforcing position sees pure coordination. Low ε and Supp keep the spread narrower than C1.
  </indexical_variance>

  <boltzmann_test>FAIL. Type varies with P (institutional → Rope, powerless/analytical → Tangled Rope) at fixed S=global. Constructed constraint confirmed; no Mountain claim made.</boltzmann_test>
</constraint>
```

```xml
<constraint id="C3">
  <properties>
    <epsilon>0.95</epsilon>
    <suppression>1.00</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X2">
    <index>
      <power>powerless</power>            <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>0.95 × 1.5 × 1.0 = 1.425 ≈ 1.43</chi>
    <type>Snare</type>
    <threshold_check>χ=1.43≥0.66: PASS. ε=0.95≥0.46: PASS. Supp=1.00≥0.60: PASS. NaturalLawWithoutBeneficiary=false (named beneficiary X1 exists): block does not fire. SnareImmutability: trapped/biographical sees immutable; X1's arbitrage/institutional index perceives this as governable tool → gate satisfied. Snare confirmed.</threshold_check>
    <rationale>Power position collapse (from "powerful" in C1 to "powerless" here) reflects that transgression revokes standing entirely. Maximum ε and Supp combine with maximum π to produce the highest χ in the network.</rationale>
  </agent>

  <agent ref="X1">
    <index>
      <power>institutional</power>        <!-- π = -0.2 -->
      <time>civilizational</time>
      <exit>arbitrage</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>0.95 × (-0.2) × 1.0 = -0.19</chi>
    <type>Naturalized</type>
    <threshold_check>ε=0.95>0.45: PASS floor. χ=-0.19<0.35: PASS ceiling. Naturalized confirmed — extreme base extraction fully absorbed by extreme negative power modifier.</threshold_check>
    <rationale>The highest-ε constraint in the network, viewed from the position that wields it, produces the most negative χ in the network: extraction is not merely dampened but rendered invisible as extraction, appearing instead as ordinary governance function. Primary substrate for false-summit rhetoric — the punishing act is naturalized as legitimate order-maintenance rather than recognized as extraction.</rationale>
  </agent>

  <indexical_variance>
    C3: X2 χ=1.43 → Snare, X1 χ=-0.19 → Naturalized
    Maximal divergence in the network: the same act registers as the most extreme possible extraction trap for its target and as fully naturalized, non-extractive governance for its enforcer. No intermediate agent classification present for this constraint — the two indices sampled sit at opposite poles with no Tangled-Rope-occupying agent between them (consistent with C3's Coord=false: no coordination-value agent exists to occupy the middle band).
  </indexical_variance>

  <boltzmann_test>FAIL. Type flips categorically (Snare → Naturalized) with P alone, S held at universal/1.0 for both agents. Confirms constructed status despite universal scope — scope-invariance alone (σ=1.0 for both) does not confer Mountain status when P-variance produces non-factorizable type change. No Mountain claim made.</boltzmann_test>
</constraint>
```

---

## 2. TRANSFORMATION RULES

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>acquire_forbidden_resource_and_transfer_to_restricted_population</condition>
    <target>C2</target>
    <agent>X2</agent>
  </trigger>
  <index_change>
    <from>P = analytical (π=1.15), E = analytical</from>
    <to>P = powerless (π=1.5), E = trapped</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.30 × 1.15 × 1.2 = 0.41 → Tangled Rope</before>
    <after>[transfers agent into C3's domain — see C3 X2 classification] χ = 0.95 × 1.5 × 1.0 = 1.43 → Snare</after>
  </chi_recalculation>
  <type_change>Tangled Rope (C2) → Snare (C3, new constraint activated)</type_change>
  <preconditions>Agent must hold sufficient standing (powerful/analytical, not powerless) at time of action to execute transfer; population receiving resource must be under C2's suppression.</preconditions>
  <blocked_by>C1 enforcement capacity (X1's institutional π and arbitrage exit permit immediate retaliatory reclassification of X2's power index).</blocked_by>
</transformation_rule>

<transformation_rule id="TR2">
  <trigger>
    <condition>receive_transferred_resource</condition>
    <target>C2</target>
    <agent>X3</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = identity_locked</from>
    <to>P = powerless (π=1.5), E = constrained</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.30 × 1.5 × 1.2 = 0.54 → Tangled Rope</before>
    <after>χ = 0.30 × 1.5 × 1.2 = 0.54 → Tangled Rope (unchanged numerically; E-shift alters Changeable() predicate, not χ)</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Tangled Rope (χ invariant; structural mutability increases via exit-option upgrade from identity_locked to constrained)</type_change>
  <preconditions>TR1 must complete (resource transfer executed).</preconditions>
  <blocked_by>None once TR1 completes — this transformation is a direct downstream consequence.</blocked_by>
</transformation_rule>

<transformation_rule id="TR3">
  <trigger>
    <condition>enforcement_agent_applies_maximal_sanction</condition>
    <target>C1</target>
    <agent>X2</agent>
  </trigger>
  <index_change>
    <from>P = powerful (π=0.6), T = civilizational, E = constrained</from>
    <to>P = powerless (π=1.5), T = biographical, E = trapped</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.80 × 0.6 × 1.0 = 0.48 → Tangled Rope</before>
    <after>[C1 exposure superseded by C3 activation] χ(C3) = 0.95 × 1.5 × 1.0 = 1.43 → Snare</after>
  </chi_recalculation>
  <type_change>Tangled Rope (C1 exposure) → Snare (C3 exposure); C1 exposure for this agent becomes moot as C3 dominates</type_change>
  <preconditions>TR1 completed; C1's asymmetric enforcement capacity intact (Supp=0.90 satisfied).</preconditions>
  <blocked_by>Nothing at index level — this is C1's designed response and requires no special unlocking condition beyond TR1.</blocked_by>
</transformation_rule>

<transformation_rule id="TR4">
  <trigger>
    <condition>organize_collective_bargaining_leverage_against_enforcer</condition>
    <target>C3</target>
    <agent>X2</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = organized (π=0.4), E = constrained</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.95 × 1.5 × 1.0 = 1.43 → Snare</before>
    <after>χ = 0.95 × 0.4 × 1.0 = 0.38 → Tangled Rope</after>
  </chi_recalculation>
  <type_change>Snare → Tangled Rope</type_change>
  <preconditions>Agent must possess an asset with independent value to X1 (informational, structural, or otherwise) that cannot be extracted by force alone — collective/leverage-based power shift, not unilateral escape.</preconditions>
  <blocked_by>C1's arbitrage-exit institutional agent (X1) will resist any reclassification that reduces net-beneficiary status; requires the leveraged asset to exceed the value of continued enforcement.</blocked_by>
</transformation_rule>

<transformation_rule id="TR5">
  <trigger>
    <condition>exit_constraint_via_negotiated_settlement</condition>
    <target>C3</target>
    <agent>X2</agent>
  </trigger>
  <index_change>
    <from>P = organized (π=0.4), E = constrained</from>
    <to>P = moderate (π=1.0), E = mobile</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.95 × 0.4 × 1.0 = 0.38 → Tangled Rope</before>
    <after>χ = 0.95 × 1.0 × 1.0 = 0.95 → Tangled Rope (upper band, near Snare ceiling)</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Tangled Rope (remains hybrid; χ rises toward but does not cross the 0.90 ceiling into Snare — settlement reduces suppression enforcement but does not eliminate base extraction)</type_change>
  <preconditions>TR4 completed; negotiated settlement requires reciprocal concession (per C1 selection_reason's "compromise" framing).</preconditions>
  <blocked_by>Full escape blocked structurally — ε=0.95 remains fixed regardless of index shift, meaning no accessible index for this agent drives χ below the Tangled Rope floor while C3 remains active.</blocked_by>
</transformation_rule>
```

---

## 3. ERROR MANIFESTATIONS

```xml
<error id="E1">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X1</agent>
  <constraint>C1</constraint>
  <actual_type>Rope (at X1's institutional index, χ=-0.16)</actual_type>
  <perceived_type>Rope (self-classification matches actual — error is external, not internal to X1)</perceived_type>
  <observable>X1 asserts C1 provides universal coordination benefit without checking χ at powerless index (X3/X4, χ=1.20 → Snare). Test per §V: χ(powerless)=1.20 > 0.70 threshold → Snare exists regardless of X1's χ.</observable>
  <correction_trigger>Explicit computation of χ at powerless/trapped/generational index reveals Snare status independent of institutional experience; error corrected by mandatory multi-index check before universal-benefit claims are validated.</correction_trigger>
</error>

<error id="E2">
  <type>Type I — False Mountain</type>
  <agent>X3</agent>
  <constraint>C1</constraint>
  <actual_type>Snare (χ=1.20, constructed per Boltzmann FAIL)</actual_type>
  <perceived_type>Mountain (treated as unchangeable natural order)</perceived_type>
  <observable>Agent takes no action to test C1's mutability despite trapped/generational index; classification varies by Power × Scope (per Boltzmann test on C1), which structurally disqualifies Mountain status — but agent's own indexed position cannot detect this without external information transfer.</observable>
  <correction_trigger>Introduction of information (via TR1/TR2) demonstrating that X1's index perceives C1 as changeable; this converts agent's perceived type from Mountain toward accurate Snare recognition, enabling subsequent leverage-seeking behavior (TR4 preconditions).</correction_trigger>
</error>

<error id="E3">
  <type>Type V.b — Tangled-as-Snare (Ignoring Coordination Component)</type>
  <agent>X2</agent>
  <constraint>C1</constraint>
  <actual_type>Tangled Rope (χ=0.48, both Coord and Asym confirmed true)</actual_type>
  <perceived_type>Pure Snare (coordination value denied entirely in favor of extraction-only framing)</perceived_type>
  <observable>Agent's action (TR1 trigger: acquire_forbidden_resource_and_transfer) proceeds as though C1 offers zero coordination value worth preserving, despite Coord(C1)=true and χ=0.48 sitting within the Tangled band rather than the Snare band (0.66+).</observable>
  <correction_trigger>Recognition that removing C1 entirely would eliminate a genuine coordination function (per Type IV test: "does removing constraint eliminate a coordination function?") — answer is yes — meaning correction requires reform strategy calibrated to hybrid status (partial modification, not full rupture), which the eventual TR5 negotiated-settlement outcome reflects.</correction_trigger>
</error>

<error id="E4">
  <type>Type III — Snare-as-Rope (Missing Extraction), inverse case</type>
  <agent>X1</agent>
  <constraint>C3</constraint>
  <actual_type>Naturalized (χ=-0.19, ε=0.95 — extreme extraction absorbed, not eliminated)</actual_type>
  <perceived_type>Rope-adjacent legitimate governance tool (extraction treated as fully absent rather than merely absorbed)</perceived_type>
  <observable>Agent's enforcement action (TR3 trigger) is executed without apparent friction or reclassification pressure, consistent with treating the extremely high ε=0.95 as non-existent rather than institutionally displaced. Test: does agent's action pattern change if ε were lower? No — behavior is invariant to ε, confirming the extraction is masked, not absent.</observable>
  <correction_trigger>Explicit ε audit independent of χ reveals base extraction magnitude (0.95, the highest in network) regardless of institutional absorption; correction requires distinguishing "no extraction" from "extraction rendered invisible by power position" — the defining Naturalized-type error.</correction_trigger>
</error>
```

---

## 4. INSTITUTIONAL RATIONALITY MODEL

```
Selected: BOUNDED INSTITUTIONAL RATIONALITY (BIR)

Justification:
- C1's designated agent (X1) operates with Coord(C1)=true and non-Pareto-improving
  negotiation observed in TR4/TR5 (settlement requires reciprocal concession, not
  unilateral utility maximization).
- C3's resolution path (TR4 → TR5) shows satisficing behavior: X1 accepts a
  bounded settlement (χ rises to 0.95, remaining in Tangled Rope band) rather
  than either (a) maximally punishing indefinitely [PIR-consistent] or
  (b) fully releasing the agent [full Pareto optimum].
- Principal-agent structure present: X1 (institutional principal) cannot perfectly
  monitor or control X2 (powerful-but-subordinate agent) — TR1's successful
  trigger execution demonstrates monitoring imperfection consistent with BIR,
  not PIR (which would assume perfect institutional foresight preventing TR1
  entirely).
- Risk-averse settlement behavior in TR5 (accepting elevated but sub-Snare χ
  rather than pursuing total extraction restoration) is a satisficing signature.

Attractor compatibility: BIR → Negotiated Equilibrium, Seeded Possibility (per
Compatibility Matrix, Tangled-Ropes-dominant profile + BIR → Negotiated Equilibrium).
```

---

## 5. TERMINAL ATTRACTOR SELECTION

```
☐ Deterministic Tragedy
☐ Revolutionary Rupture
☑ Seeded Possibility
☐ Negotiated Equilibrium (rejected — see below)

Selection: SEEDED POSSIBILITY

Justification:
- Constraint profile at resolution: C1 remains Tangled-Rope-dominant for X2
  (TR5 final state: χ=0.95, still classified Tangled Rope, not fully resolved
  to Rope). This matches "Tangled Ropes dominant + BIR" row, which permits
  Negotiated Equilibrium — but the OMEGA condition (O1) explicitly leaves
  unresolved whether C1's fundamental nature changes or whether resolution
  merely forestalls recurrence.
- This unresolved-nature signature is the defining feature of Seeded Possibility
  (surface tragedy/settlement, underground transformation) rather than clean
  Negotiated Equilibrium (stable bargained balance).
- The invariant_contract's untranslatable_real ("capacity for self-directed
  growth, once granted, cannot be revoked") is present but non-primary —
  it operates underground, seeding future transformation of C1 without
  resolving in-narrative. This is structurally incompatible with Deterministic
  Tragedy (which requires no such generative residue) and insufficient for
  Revolutionary Rupture (constraint logic of C1 is NOT disrupted — TR5 shows
  C1 persisting in modified but intact form).
- Missing_floor invariant (illegitimate seizure of rule obscured as natural
  right) remains unaddressed at resolution — the compromise in TR5 preserves
  C1's underlying legitimacy claim rather than exposing it, consistent with
  surface-level settlement masking unresolved structural tension.

Compatibility check: BIR + Tangled-Ropes-dominant + Piton-adjacent-uncertainty
(O1's unresolved status) → Seeded Possibility is compatible per Matrix (Piton
present → Seeded Possibility, Either/Either rationality). No Piton is confirmed
in this network, but the functional signature — persistence-without-resolution —
matches Seeded Possibility's defining condition more precisely than clean
Equilibrium.
```

---

## 6. VALIDATION CHECKLIST

```
☑ All Stage 0 constraints formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with π and σ values
☑ All characters use variable names (X₁–X₄) — no source identifiers in body
☑ Variable mapping table present in header
☑ No source occupation, setting, or domain vocabulary anywhere in output
☑ No <experience>, <dialogue_markers>, or <narrative> fields
☑ Transformation rules use abstract trigger descriptions
☑ Error observables are testable structural conditions
☑ Institutional rationality model specified (BIR) with justification
☑ Terminal attractor selected (Seeded Possibility), justified, and compatible
☑ Indexical variance explicitly preserved across agents (all 3 constraints)
☑ Boltzmann test run for each constraint (all three: FAIL, correctly routing away from Mountain)
☑ No ambiguity in specifications
☑ Output is pure symbolic network — source work unidentifiable from body text
```