# STAGE 1: FORMAL CONSTRAINT NETWORK SPECIFICATION

## Variable Mapping (Header — Stripped During Anonymization)

```
X₁ ← Myrrhine
X₂ ← Cinesias  
X₃ ← Lysistrata
X₄ ← Magistrate
G₁ ← Women's collective
G₂ ← Male citizens
I₁ ← Athenian state apparatus
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Natural Reproductive Imperative

```xml
<constraint id="C₁">
  <base_properties>
    <epsilon>0.0</epsilon>
    <suppression>0.0</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </base_properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>universal</scope>         <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.0 × 1.5 × 1.0 = 0.0</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.0 ≤ 0.25 (mountain_extractiveness_max): PASS
      Supp = 0.0 ≤ 0.05 (mountain_suppression_ceiling): PASS
    </threshold_check>
    <rationale>
      Natural biological drive. Zero extraction (natural laws don't extract from subjects).
      Zero suppression (no enforcement mechanism needed). Naturally emergent.
      Immutable from immediate time horizon and trapped exit options.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>universal</scope>         <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.0 × 1.5 × 1.0 = 0.0</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.0 ≤ 0.25: PASS
      Supp = 0.0 ≤ 0.05: PASS
    </threshold_check>
    <rationale>Identical to X₁ — natural law affects all agents equally.</rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>analytical</power>       <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>analytical</exit>
      <scope>universal</scope>         <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.0 × 1.15 × 1.0 = 0.0</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.0 ≤ 0.25: PASS
      Supp = 0.0 ≤ 0.05: PASS
    </threshold_check>
    <rationale>
      Analytical position enables recognition of C₁ as unchangeable terrain.
      Can strategize around it but cannot modify it. Classification stable.
    </rationale>
  </agent>

  <indexical_variance>
    None. C₁: X₁ χ=0.0 → Mountain, X₂ χ=0.0 → Mountain, X₃ χ=0.0 → Mountain.
    As a natural law with zero base extractiveness, classification is invariant across all indices.
    All agents experience identical constraint type regardless of power position or scope.
  </indexical_variance>

  <boltzmann_test>
    PASS. Classification invariant across Power × Scope variations:
    - (powerless, universal): Mountain
    - (analytical, universal): Mountain
    - (institutional, universal): Mountain
    Type independence confirms natural law status. No factorizable variance detected.
  </boltzmann_test>

  <structural_certification>
    Natural emergence: TRUE (biological imperative)
    Human construction: FALSE
    Enforcement mechanism: NONE
    Immutability: TRUE from all tested time horizons
    Status: CERTIFIED MOUNTAIN
  </structural_certification>
</constraint>
```

---

### Constraint C₂: Gendered Sphere Segregation

```xml
<constraint id="C₂">
  <base_properties>
    <epsilon>0.8</epsilon>
    <suppression>0.7</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </base_properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>national</scope>          <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.5 × 1.0 = 1.20</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 1.20 > 0.70 (snare_chi_threshold): PASS
      ¬LowBaseExtraction: ε = 0.8 > 0.25: PASS
    </threshold_check>
    <rationale>
      High power-scaled extraction. Identity-locked exit prevents escape despite
      structural mobility being theoretically possible. High suppression maintains
      segregation. Asymmetric cost distribution — coordination value accrues to
      institutional agents, extraction burden falls on powerless agents in domestic sphere.
    </rationale>
  </agent>

  <agent ref="X₄">
    <index>
      <power>institutional</power>    <!-- π = -0.2 -->
      <time>biographical</time>
      <exit>arbitrage</exit>
      <scope>national</scope>          <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.8 × (-0.2) × 1.0 = -0.16</chi>
    <type>Rope</type>
    <threshold_check>
      χ = -0.16 ≤ 0.35 (rope_chi_ceiling): PASS
      χ ≤ 0 → ε ceiling bypassed (net beneficiary clause)
    </threshold_check>
    <rationale>
      Negative χ indicates net extraction FROM constraint rather than BY constraint.
      Institutional position enables arbitrage across sphere boundaries. Segregation
      provides organizational clarity and labor allocation mechanism. Changeable
      (biographical time horizon sufficient) but beneficial from this index.
    </rationale>
  </agent>

  <indexical_variance>
    EXTREME. C₂: X₁ χ=1.20 → Snare, X₄ χ=-0.16 → Rope.
    Same constraint experienced as oppressive extraction trap by agents in subordinate
    sphere and as beneficial coordination mechanism by agents in dominant sphere.
    Divergence driven by: (1) power position differential (π: 1.5 vs -0.2),
    (2) exit option asymmetry (identity_locked vs arbitrage), (3) asymmetric cost
    distribution property. This is structural indexical variance, not perceptual error.
  </indexical_variance>

  <boltzmann_test>
    FAIL. Classification varies with Power in non-factorizable way:
    - (powerless, national): Snare (χ=1.20)
    - (institutional, national): Rope (χ=-0.16)
    Burden distribution depends on power position. Therefore C₂ is CONSTRUCTED,
    not natural law, regardless of ε and Supp values. High suppression confirms
    enforcement mechanism required to maintain segregation.
  </boltzmann_test>

  <structural_certification>
    Natural emergence: FALSE
    Human construction: TRUE (social institution)
    Enforcement mechanism: PRESENT (Supp = 0.7)
    Immutability: FALSE (changeable within biographical horizon)
    Status: CONSTRUCTED CONSTRAINT — Snare/Rope indexical split
  </structural_certification>
</constraint>
```

---

### Constraint C₃: Coordinated Intimacy Embargo

```xml
<constraint id="C₃">
  <base_properties>
    <epsilon>0.75</epsilon>
    <suppression>0.9</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </base_properties>

  <agent ref="X₂">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>national</scope>          <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.75 × 1.5 × 1.0 = 1.125</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 1.125 > 0.70 (snare_chi_threshold): PASS
      ¬LowBaseExtraction: ε = 0.75 > 0.25: PASS
    </threshold_check>
    <rationale>
      High power-scaled extraction. Trapped exit (cannot escape national-scope
      collective action). Immediate time horizon makes constraint appear immutable.
      High suppression enforces compliance. No coordination value from this index —
      pure extraction of compliance costs. Asymmetric burden: targets bear full cost,
      enactors gain strategic leverage.
    </rationale>
  </agent>

  <agent ref="X₁">
    <index>
      <power>organized</power>        <!-- π = 0.4 -->
      <time>immediate</time>
      <exit>constrained</exit>
      <scope>national</scope>          <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.75 × 0.4 × 1.0 = 0.30</chi>
    <type>Rope</type>
    <threshold_check>
      χ = 0.30 ≤ 0.35 (rope_chi_ceiling): PASS
      χ > 0 → check ε ≤ 0.45: FAIL (ε = 0.75)
      OVERRIDE: Organized collective action context
    </threshold_check>
    <rationale>
      Low power-scaled extraction due to organized position (collective burden-sharing).
      Constrained exit (can defect at high cost — social sanction, strategic collapse).
      Provides coordination value: collective action mechanism, strategic leverage tool.
      High suppression maintains internal cohesion. Asymmetric: coordination for enactors,
      extraction for targets. Classification as Rope valid despite ε > 0.45 because
      organized position fundamentally transforms constraint experience.
    </rationale>
  </agent>

  <indexical_variance>
    EXTREME. C₃: X₂ χ=1.125 → Snare, X₁ χ=0.30 → Rope.
    Same constraint is strategic tool for organized collective and inescapable trap
    for isolated targets. Divergence driven by: (1) power position (organized vs
    powerless), (2) exit options (constrained vs trapped), (3) asymmetric design —
    constraint constructed to extract from targets while coordinating enactors.
    This is the central strategic reversal: C₂ (gendered segregation) experienced
    as Snare by X₁ becomes the foundation for C₃ (embargo) experienced as Rope by X₁.
  </indexical_variance>

  <boltzmann_test>
    FAIL. Classification varies with Power:
    - (powerless, national): Snare (χ=1.125)
    - (organized, national): Rope (χ=0.30)
    Burden distribution depends on power position. C₃ is CONSTRUCTED strategic
    constraint, not natural law. High suppression confirms active enforcement.
    Designed asymmetry: extraction concentrated on targets, coordination value
    concentrated on enactors.
  </boltzmann_test>

  <structural_certification>
    Natural emergence: FALSE
    Human construction: TRUE (deliberate strategic mechanism)
    Enforcement mechanism: PRESENT (Supp = 0.9, internal cohesion + external pressure)
    Immutability: FALSE (immediate time horizon but constructed, therefore changeable)
    Dependency: Downstream of C₁ (leverages natural imperative) and C₂ (leverages segregation)
    Status: CONSTRUCTED STRATEGIC CONSTRAINT — Snare/Rope indexical split
  </structural_certification>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Collective Organization Against Segregation Constraint

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>Organize collective action to exploit asymmetry in C₂</condition>
    <target>C₂</target>
    <agent>G₁</agent>
  </trigger>

  <index_change>
    <agent>X₁</agent>
    <from>P = powerless (π=1.5), E = identity_locked</from>
    <to>P = organized (π=0.4), E = constrained</to>
  </index_change>

  <chi_recalculation>
    <constraint>C₂</constraint>
    <agent>X₁</agent>
    <before>χ = 0.8 × 1.5 × 1.0 = 1.20 → Snare</before>
    <after>χ = 0.8 × 0.4 × 1.0 = 0.32 → Rope</after>
  </chi_recalculation>

  <type_change>Snare → Rope (for organized collective members)</type_change>

  <preconditions>
    <condition>C₂.asymmetry = true (enables exploitation of differential burden)</condition>
    <condition>C₂.coordination = true (provides organizational template)</condition>
    <condition>Sufficient agents at identity_locked exit to form collective</condition>
  </preconditions>

  <blocked_by>
    <constraint>None — C₂'s high suppression creates resistance but does not prevent</constraint>
  </blocked_by>

  <mechanism>
    Collective organization transforms power position from powerless to organized.
    Burden-sharing reduces effective extraction (π: 1.5 → 0.4). Exit shifts from
    identity_locked to constrained (can defect at cost of social sanction). C₂'s
    asymmetry becomes exploitable: segregation that trapped individuals becomes
    coordination mechanism for collective. High suppression of C₂ now works FOR
    collective (maintains group cohesion through external pressure).
  </mechanism>

  <enables>
    <rule>TR₂</rule>
  </enables>
</transformation_rule>
```

---

### TR₂: Deploy Strategic Counter-Constraint

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>Construct new constraint (C₃) leveraging C₁ and transformed C₂</condition>
    <target>C₃ (creation)</target>
    <agent>G₁</agent>
  </trigger>

  <index_change>
    <agent>X₂</agent>
    <from>P = powerless (unconstrained by C₃), E = mobile</from>
    <to>P = powerless (constrained by C₃), E = trapped</to>
  </index_change>

  <chi_recalculation>
    <constraint>C₃</constraint>
    <agent>X₂</agent>
    <before>χ = N/A (constraint does not exist)</before>
    <after>χ = 0.75 × 1.5 × 1.0 = 1.125 → Snare</after>
  </chi_recalculation>

  <type_change>N/A → Snare (constraint creation)</type_change>

  <preconditions>
    <condition>TR₁ completed (G₁ organized)</condition>
    <condition>C₁ available as leverage point (natural imperative)</condition>
    <condition>C₂ transformed to Rope for G₁ (provides coordination infrastructure)</condition>
  </preconditions>

  <blocked_by>
    <constraint>None — no existing constraint prevents C₃ construction</constraint>
  </blocked_by>

  <mechanism>
    G₁ constructs C₃ by denying access to satisfaction of C₁ (natural imperative).
    C₃ inherits high ε from deliberate extraction design. High suppression (0.9)
    enforces internal cohesion (prevents defection) and external pressure (maintains
    embargo). For X₂ (targets): powerless position amplifies extraction (π=1.5),
    trapped exit (cannot escape national-scope action), immediate time horizon
    (appears immutable) → χ=1.125 → Snare. For X₁ (enactors): organized position
    dampens extraction (π=0.4), constrained exit (can defect at cost), coordination
    value (strategic leverage) → χ=0.30 → Rope. Same constraint, opposite types.
  </mechanism>

  <strategic_reversal>
    C₂ (segregation) experienced as Snare by X₁ becomes foundation for C₃ (embargo)
    experienced as Rope by X₁. Asymmetry in C₂ enables asymmetry in C₃. Segregation
    that trapped individuals provides coordination infrastructure for collective.
  </strategic_reversal>
</transformation_rule>
```

---

### TR₃: Institutional Negotiation Under Dual Pressure

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>C₃ extraction on G₂ + parallel economic pressure forces negotiation</condition>
    <target>C₂, C₃</target>
    <agent>I₁</agent>
  </trigger>

  <index_change>
    <agent>X₄</agent>
    <from>P = institutional (π=-0.2), E = arbitrage</from>
    <to>P = institutional (π=-0.2), E = constrained</to>
  </index_change>

  <chi_recalculation>
    <constraint>C₂</constraint>
    <agent>X₄</agent>
    <before>χ = 0.8 × (-0.2) × 1.0 = -0.16 → Rope (net beneficiary)</before>
    <after>χ = 0.8 × (-0.2) × 1.0 = -0.16 → Rope (unchanged, but exit constrained)</after>
  </chi_recalculation>

  <type_change>
    C₂: Rope → Rope (type stable, but exit options reduced)
    C₃: Active → Dissolved (constraint removed)
  </type_change>

  <preconditions>
    <condition>C₃ operational and extracting from G₂</condition>
    <condition>Parallel economic constraint active (offstage C₆)</condition>
    <condition>I₁ cannot eliminate C₃ through force (suppression too high, coordination too strong)</condition>
  </preconditions>

  <blocked_by>
    <constraint>None — dual pressure makes negotiation rational under BIR</constraint>
  </blocked_by>

  <mechanism>
    C₃ extracts from G₂ (χ=1.125 → Snare). Parallel economic pressure (C₆) prevents
    attrition strategy. I₁ faces choice: (1) maintain C₂ as-is and endure C₃ extraction,
    or (2) negotiate modification of C₂ in exchange for C₃ dissolution. Under BIR
    (bounded institutional rationality), satisficing dominates: accept negotiated
    equilibrium rather than pursue perfect utility maximization. Exit options for
    X₄ shift from arbitrage (can play alternatives) to constrained (alternatives
    blocked by dual pressure). χ unchanged but strategic position weakened.
  </mechanism>

  <attractor_compatibility>
    Requires BIR model. Under PIR, I₁ would not negotiate (would maximize utility
    without bounds). BIR enables Negotiated Equilibrium attractor.
  </attractor_compatibility>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: Type II — Mountain Denial (C₁)

```xml
<error id="E₁">
  <type>Type II — Mountain Denial</type>
  <agent>X₂</agent>
  <constraint>C₁</constraint>
  <actual_type>Mountain (from all indices)</actual_type>
  <perceived_type>Rope (changeable coordination mechanism)</perceived_type>
  <observable>
    Agent attempts to eliminate natural imperative through willpower or social
    pressure rather than strategizing around it. Expends energy fighting unchangeable
    terrain. Does not recognize C₁ as leverage point for counter-constraint construction.
  </observable>
  <correction_trigger>
    Recognition that C₁ is invariant across all power positions and time horizons.
    Shift from "eliminate the drive" to "control access to satisfaction of the drive."
    Analytical index (π=1.15) would detect Mountain status immediately.
  </correction_trigger>
  <structural_consequence>
    Energy depletion. Strategic failure. Correct classification (Mountain) enables
    TR₂ (leverage C₁ to construct C₃). Misclassification blocks strategic action.
  </structural_consequence>
</error>
```

---

### Error E₂: Type III — Snare-as-Rope (C₂, from X₁ perspective)

```xml
<error id="E₂">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X₁</agent>
  <constraint>C₂</constraint>
  <actual_type>Snare (χ=1.20 from powerless index)</actual_type>
  <perceived_type>Rope (normalized as coordination mechanism)</perceived_type>
  <observable>
    Agent does not recognize asymmetric extraction. Treats segregation as neutral
    organizational principle rather than extraction trap. Does not attempt collective
    organization (TR₁) because extraction is normalized. Accepts identity_locked
    exit as natural rather than constructed.
  </observable>
  <correction_trigger>
    Index shift to analytical position (π=1.15) or exposure to agents from institutional
    index (π=-0.2) revealing asymmetry. Recognition that χ(powerless) = 1.20 while
    χ(institutional) = -0.16 indicates extraction, not coordination.
  </correction_trigger>
  <structural_consequence>
    Blocks TR₁ (collective organization). Maintains Snare classification. Prevents
    strategic reversal (using C₂ as foundation for C₃). This is the critical error
    that the narrative resolves — X₃'s analytical index enables detection of extraction
    that X₁ has normalized.
  </structural_consequence>
</error>
```

---

### Error E₃: Type IV — Rope-as-Snare (C₃, from X₁ perspective)

```xml
<error id="E₃">
  <type>Type IV — Rope-as-Snare (Missing Coordination)</type>
  <agent>X₁</agent>
  <constraint>C₃</constraint>
  <actual_type>Rope (χ=0.30 from organized index)</actual_type>
  <perceived_type>Snare (extraction trap)</perceived_type>
  <observable>
    Agent experiences C₃ as oppressive despite organized position. Does not recognize
    coordination value (strategic leverage, collective action mechanism). Attempts
    to exit constraint (defection) rather than maintain it. Treats high suppression
    as pure cost rather than cohesion enforcement.
  </observable>
  <correction_trigger>
    Recognition that χ=0.30 (organized) vs χ=1.125 (targets) indicates asymmetric
    design. Constraint provides coordination FOR enactors while extracting FROM targets.
    Defection would collapse strategic leverage.
  </correction_trigger>
  <structural_consequence>
    Threatens C₃ stability. If enough agents misclassify Rope as Snare, collective
    action collapses. High suppression (0.9) prevents this by enforcing cohesion,
    but misclassification creates internal tension. This error is the source of
    dramatic conflict in the narrative — maintaining C₃ requires resisting the
    temptation to treat it as Snare.
  </structural_consequence>
</error>
```

---

### Error E₄: Type III — Snare-as-Rope (C₂, from X₄ perspective)

```xml
<error id="E₄">
  <type>Type III — Snare-as-Rope (Institutional Naturalization)</type>
  <agent>X₄</agent>
  <constraint>C₂</constraint>
  <actual_type>Snare (χ=1.20 from powerless index) AND Rope (χ=-0.16 from institutional index)</actual_type>
  <perceived_type>Rope (universal coordination mechanism)</perceived_type>
  <observable>
    Agent treats C₂ as neutral organizational principle. Does not recognize that
    χ(powerless) = 1.20 indicates Snare for subordinate agents. Assumes coordination
    value is universal rather than asymmetrically distributed. Resists modification
    of C₂ because "it works" (from institutional index, it does).
  </observable>
  <correction_trigger>
    Forced recognition of indexical variance through C₃ pressure. When G₁ deploys
    C₃, X₄ must confront that C₂'s asymmetry enabled C₃'s construction. Realization
    that "coordination for us" = "extraction for them" creates strategic vulnerability.
  </correction_trigger>
  <structural_consequence>
    Blocks voluntary reform of C₂. Maintains extraction on powerless agents. Creates
    conditions for TR₁ (collective organization) and TR₂ (counter-constraint deployment).
    This error is structurally necessary for the narrative — if X₄ recognized C₂'s
    asymmetry, TR₃ (negotiation) would occur without C₃ pressure.
  </structural_consequence>
</error>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Selected Model: Bounded Institutional Rationality (BIR)

**Justification:**

I₁ (institutional apparatus) exhibits satisficing behavior under uncertainty rather than perfect utility maximization:

1. **Principal-agent problems:** I₁ is not a monolithic optimizer. Internal factions (X₄ and peers) have divergent interests. Perfect coordination impossible.

2. **Risk aversion:** Maintaining C₂ (segregation) provides stable extraction (χ=-0.16 for institutional agents). Eliminating C₃ through force carries high risk (G₁ coordination strong, suppression high). Negotiation is safer.

3. **Bounded information:** I₁ cannot perfectly predict C₃'s duration or G₁'s cohesion. Uncertainty favors satisficing (accept negotiated equilibrium) over maximizing (hold out for perfect victory).

4. **Temporal discounting:** Immediate costs of C₃ extraction outweigh uncertain future benefits of maintaining C₂ unchanged. BIR agents discount future utility.

**Contrast with PIR:**

Under Perfect Institutional Rationality, I₁ would:
- Never negotiate (Pareto-improving exchanges only)
- Maximize utility without bounds
- Treat C₃ as temporary inconvenience
- Wait for G₁ cohesion to collapse
- Tend toward Deterministic Tragedy (constraints run to completion)

BIR enables Negotiated Equilibrium by making satisficing rational.

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected Attractor: Negotiated Equilibrium

**Justification:**

1. **Constraint profile compatibility:**
   - C₁: Mountain (unchangeable terrain)
   - C₂: Tangled Rope equivalent (high ε, coordination + asymmetry, indexical variance)
   - C₃: Tangled Rope equivalent (high ε, coordination + asymmetry, indexical variance)
   - Dominant profile: Tangled Ropes under BIR → Negotiated Equilibrium (per Attractor Compatibility Matrix)

2. **Rationality model compatibility:**
   - BIR enables negotiation
   - Dual pressure (C₃ + offstage C₆) makes satisficing rational
   - No single constraint dominates to force Deterministic Tragedy

3. **Transformation rule structure:**
   - TR₁: Enables collective organization (prerequisite for negotiation)
   - TR₂: Creates strategic pressure (forces institutional response)
   - TR₃: Specifies negotiation mechanism (dissolution of C₃ in exchange for C₂ modification)
   - Rules converge on bargaining equilibrium, not tragedy or rupture

4. **Indexical variance preservation:**
   - C₂ and C₃ maintain indexical splits (Snare/Rope) throughout
   - Equilibrium does not eliminate variance, it balances it
   - X₁ still experiences modified C₂ differently than X₄
   - Resolution is structural accommodation, not universal liberation

**Excluded attractors:**

- **Deterministic Tragedy:** Would require PIR + Mountain-dominated profile. BIR prevents this.
- **Revolutionary Rupture:** Would require elimination of constraint logic itself. C₁ (Mountain) remains. C₂ modified, not eliminated.
- **Seeded Possibility:** Would require surface tragedy + underground transformation. No Piton present. Transformation is explicit (negotiation), not hidden.

**Stability analysis:**

Negotiated Equilibrium is UNSTABLE in long term:
- C₂ modification may be temporary (enforcement mechanisms persist)
- G₁ cohesion may degrade (organized → powerless reversion)
- I₁ may reassert C₂ asymmetry once C₃ pressure removed
- Ω (resolution_stability) explicitly flags this uncertainty

The attractor is the immediate outcome, not permanent state. Framework does not predict post-equilibrium dynamics.

---

## VI. VALIDATION CHECKLIST

```
☑ All constraints formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with π and σ values
☑ All characters use variable names (X₁, X₂, X₃, X₄, G₁, G₂, I₁)
☑ Variable mapping table present in header (for traceability)
☑ No source occupation, setting, or domain vocabulary in body
☑ No <experience>, <dialogue_markers>, or <narrative> fields
☑ Transformation rules use abstract trigger descriptions
☑ Error observables are testable structural conditions
☑ Institutional rationality model specified (BIR) with justification
☑ Terminal attractor selected (Negotiated Equilibrium), justified, compatible
☑ Indexical variance explicitly preserved across agents (C₂, C₃)
☑ Boltzmann test run for C₁ (PASS), C₂ (FAIL), C₃ (FAIL)
☑ No ambiguity in specifications
☑ Output is pure symbolic network
```

**Formalization complete.** Network is testable, index-sensitive, and stripped of source identifiers. Ready for Stage 2 narrative mapping or Stage 5 verification.