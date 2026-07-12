# STAGE 1: FORMAL CONSTRAINT SPECIFICATION

## Variable Mapping (Header — Traceability Only)

```
X₁ ← Narrator (apprentice)
X₂ ← Verrel
X₃ ← Crown
X₄ ← Merchant
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Metaphysical Invariant

```xml
<constraint id="C₁">
  <base_properties>
    <epsilon>0.05</epsilon>
    <suppression>0.0</suppression>
    <coordination>false</coordination>
    <asymmetric>false</asymmetric>
  </base_properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.05 × 1.5 × 0.8 = 0.06</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.05 ≤ 0.25 (mountain_extractiveness_max) ✓
      Supp = 0.0 ≤ 0.05 (mountain_suppression_ceiling) ✓
    </threshold_check>
    <rationale>
      Zero suppression indicates natural emergence. Minimal extraction 
      invariant across power positions. Immutable from biographical horizon.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>analytical</power>        <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.05 × 1.15 × 0.8 = 0.046</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.05 ≤ 0.25 ✓
      Supp = 0.0 ≤ 0.05 ✓
    </threshold_check>
    <rationale>
      Analytical position detects constraint but cannot alter it. 
      Classification stable despite degeneracy-breaking modifier.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>institutional</power>     <!-- π = -0.2 -->
      <time>generational</time>
      <exit>arbitrage</exit>
      <scope>regional</scope>          <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.05 × (-0.2) × 0.9 = -0.009</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.05 ≤ 0.25 ✓
      Supp = 0.0 ≤ 0.05 ✓
    </threshold_check>
    <rationale>
      Negative χ indicates net beneficiary position, but classification 
      remains Mountain due to natural emergence and zero suppression. 
      Institutional power cannot modify fundamental constraint.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=0.06 → Mountain, X₂ χ=0.046 → Mountain, X₃ χ=-0.009 → Mountain
    Zero variance. Classification stable across all tested indices despite 
    χ variation. Passes Boltzmann Independence Test.
  </indexical_variance>

  <boltzmann_test>
    PASS
    Reasoning: Classification invariant across Power dimension (powerless, 
    analytical, institutional) and Scope dimension (local, regional). 
    χ varies by power position but type does not. Satisfies independence 
    criterion for natural law.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₂: Epistemological Foundation

```xml
<constraint id="C₂">
  <base_properties>
    <epsilon>0.1</epsilon>
    <suppression>0.8</suppression>
    <coordination>true</coordination>
    <asymmetric>true</asymmetric>
  </base_properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.1 × 1.5 × 0.8 = 0.12</chi>
    <type>Rope</type>
    <threshold_check>
      χ = 0.12 ≤ 0.35 (rope_chi_ceiling) ✓
      ε = 0.1 ≤ 0.45 (rope_epsilon_ceiling) ✓
    </threshold_check>
    <rationale>
      Low extraction, genuine coordination value. High suppression reflects 
      enforcement cost of maintaining chosen baseline against alternatives. 
      Changeable (distinguishes from Mountain).
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>analytical</power>        <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.1 × 1.15 × 0.8 = 0.092</chi>
    <type>Rope</type>
    <threshold_check>
      χ = 0.092 ≤ 0.35 ✓
      ε = 0.1 ≤ 0.45 ✓
    </threshold_check>
    <rationale>
      Analytical position recognizes arbitrariness of baseline choice but 
      acknowledges coordination necessity. Classification stable.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>institutional</power>     <!-- π = -0.2 -->
      <time>generational</time>
      <exit>arbitrage</exit>
      <scope>regional</scope>          <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.1 × (-0.2) × 0.9 = -0.018</chi>
    <type>Rope</type>
    <threshold_check>
      χ = -0.018 ≤ 0 (net beneficiary) → ε ceiling bypassed ✓
    </threshold_check>
    <rationale>
      Institutional position extracts value from baseline choice. Negative χ 
      indicates net benefit. Dual threshold: when χ ≤ 0, ε ceiling does not 
      apply. Classification remains Rope due to coordination function.
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₁ χ=0.12 → Rope, X₂ χ=0.092 → Rope, X₃ χ=-0.018 → Rope
    Zero variance. Principle that baseline must be chosen is coordination 
    necessity (Rope) for all agents who understand it, regardless of whether 
    they bear cost or extract benefit.
  </indexical_variance>

  <boltzmann_test>
    FAIL
    Reasoning: Classification stable (all Rope), but χ varies by power 
    position in non-factorizable way. X₃ is net beneficiary (χ < 0) while 
    X₁, X₂ bear cost (χ > 0). This asymmetry indicates constructed system, 
    not natural law. High suppression (0.8) confirms enforcement requirement. 
    Constraint is well-designed coordination mechanism, not metaphysical 
    invariant.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₃: Institutional Standard

```xml
<constraint id="C₃">
  <base_properties>
    <epsilon>0.8</epsilon>
    <suppression>0.9</suppression>
    <coordination>true</coordination>
    <asymmetric>true</asymmetric>
  </base_properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 0.96 > 0.70 (snare_chi_threshold) ✓
      ε = 0.8 (not low base extraction) ✓
    </threshold_check>
    <rationale>
      High extraction amplified by powerless position. Trapped exit option 
      prevents escape. Coordination value exists but dominated by extraction 
      from this index.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>analytical</power>        <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.15 × 0.8 = 0.736</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 0.736 > 0.70 ✓
      ε = 0.8 ✓
    </threshold_check>
    <rationale>
      Analytical position detects extraction mechanism but identity-locked 
      exit prevents escape. Degeneracy-breaking modifier (π=1.15) pushes χ 
      above Snare threshold where moderate position would normalize as 
      Tangled Rope.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>institutional</power>     <!-- π = -0.2 -->
      <time>generational</time>
      <exit>arbitrage</exit>
      <scope>regional</scope>          <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.8 × (-0.2) × 0.9 = -0.144</chi>
    <type>Rope</type>
    <threshold_check>
      χ = -0.144 ≤ 0 (net beneficiary) → ε ceiling bypassed ✓
    </threshold_check>
    <rationale>
      Institutional position extracts FROM constraint rather than being 
      extracted from. Negative χ indicates net benefit. Coordination function 
      genuine from this index. Dual threshold: ε ceiling does not apply when 
      χ ≤ 0.
    </rationale>
  </agent>

  <agent ref="X₄">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>immediate</time>
      <exit>constrained</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.0 × 0.8 = 0.64</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ = 0.64 ≤ 0.70 (tangled range) ✓
      Coord = true ✓
      Asym = true ✓
    </threshold_check>
    <rationale>
      Moderate extraction in mid-range. Coordination value present but 
      asymmetric cost distribution. Typical user experiences hybrid: 
      constraint provides function but extracts significantly. Irreducible 
      Tangled Rope, not confused classification.
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₁ χ=0.96 → Snare, X₂ χ=0.736 → Snare, X₃ χ=-0.144 → Rope, X₄ χ=0.64 → Tangled Rope
    HIGH VARIANCE. Same institutional rule classified as Snare (powerless, 
    analytical), Tangled Rope (moderate), and Rope (institutional). This is 
    the central demonstration of indexed relativity: each classification is 
    objectively true from its structural position.
  </indexical_variance>

  <boltzmann_test>
    FAIL
    Reasoning: Classification varies dramatically by Power dimension. 
    Powerless agents experience Snare, institutional agents experience Rope. 
    This non-factorizable variance indicates constructed extraction system, 
    not natural law. High suppression (0.9) confirms enforcement requirement. 
    Asymmetric property confirms intentional design.
  </boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Collective Organization Attempt

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>Agents at powerless index attempt collective organization to redistribute extraction burden</condition>
    <target>C₃</target>
    <agents>X₁, X₂</agents>
  </trigger>

  <index_change>
    <agent ref="X₁">
      <from>P = powerless (π=1.5)</from>
      <to>P = organized (π=0.4)</to>
    </agent>
    <agent ref="X₂">
      <from>P = analytical (π=1.15)</from>
      <to>P = organized (π=0.4)</to>
    </agent>
  </index_change>

  <chi_recalculation>
    <agent ref="X₁">
      <before>χ = 0.8 × 1.5 × 0.8 = 0.96 → Snare</before>
      <after>χ = 0.8 × 0.4 × 0.8 = 0.256 → Rope</after>
    </agent>
    <agent ref="X₂">
      <before>χ = 0.8 × 1.15 × 0.8 = 0.736 → Snare</before>
      <after>χ = 0.8 × 0.4 × 0.8 = 0.256 → Rope</after>
    </agent>
  </chi_recalculation>

  <type_change>
    X₁: Snare → Rope
    X₂: Snare → Rope
  </type_change>

  <preconditions>
    <condition>Exit option must improve from trapped/identity_locked to at least constrained</condition>
    <condition>Coordination infrastructure must exist (communication, trust)</condition>
    <condition>Collective action threshold must be overcome</condition>
  </preconditions>

  <blocked_by>
    <constraint ref="C₆_deferred">
      Guild culture atomization prevents coordination infrastructure
    </constraint>
    <institutional_response>
      X₃ (institutional position) has incentive to suppress collective 
      organization as it threatens negative χ (net benefit) position
    </institutional_response>
  </blocked_by>
</transformation_rule>
```

---

### TR₂: Alternative Standard Establishment

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>Agent establishes alternative measurement system outside institutional control</condition>
    <target>C₃</target>
    <agent>X₂</agent>
  </trigger>

  <index_change>
    <agent ref="X₂">
      <from>E = identity_locked</from>
      <to>E = analytical (exit to meta-position)</to>
    </agent>
  </index_change>

  <chi_recalculation>
    <agent ref="X₂">
      <before>χ = 0.8 × 1.15 × 0.8 = 0.736 → Snare</before>
      <after>χ = 0.8 × 1.15 × 0.8 = 0.736 (unchanged) → Snare</after>
      <note>
        Exit option change to analytical does not modify π or σ. Agent can 
        observe constraint from outside but remains subject to it when 
        participating in system. Alternative standard creates documentation 
        but does not transform constraint classification.
      </note>
    </agent>
  </chi_recalculation>

  <type_change>
    None (classification stable)
  </type_change>

  <preconditions>
    <condition>Analytical capacity to detect bias in institutional standard</condition>
    <condition>Resources to maintain independent measurement</condition>
    <condition>Willingness to accept identity-locked position (cannot exit system entirely)</condition>
  </preconditions>

  <blocked_by>
    <constraint ref="C₁">
      Metaphysical invariant: truth remains unownable regardless of 
      measurement system. Alternative standard documents bias but cannot 
      capture universal coherence.
    </constraint>
  </blocked_by>

  <omega_marker>
    This transformation addresses Ω(accumulation_effect): Does private 
    documentation accumulate into systemic change? Rule specifies: NO 
    immediate χ transformation, but creates potential for future TR₁ 
    (collective organization) by providing evidence base.
  </omega_marker>
</transformation_rule>
```

---

### TR₃: Institutional Baseline Shift

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>Institutional agent modifies baseline standard in response to documented bias</condition>
    <target>C₃</target>
    <agent>X₃</agent>
  </trigger>

  <index_change>
    <note>No index change — institutional position stable</note>
  </index_change>

  <chi_recalculation>
    <agent ref="X₁">
      <before>χ = 0.8 × 1.5 × 0.8 = 0.96 → Snare</before>
      <after>χ = [0.8 - Δε] × 1.5 × 0.8 = [reduced] → [type depends on Δε magnitude]</after>
      <note>
        Baseline shift reduces ε by redistributing extraction. If Δε ≥ 0.33, 
        χ drops below 0.70 (Snare threshold) → Tangled Rope. If Δε ≥ 0.53, 
        χ drops below 0.35 (Rope ceiling) → Rope.
      </note>
    </agent>
    <agent ref="X₃">
      <before>χ = 0.8 × (-0.2) × 0.9 = -0.144 → Rope</before>
      <after>χ = [0.8 - Δε] × (-0.2) × 0.9 = [less negative] → Rope (stable)</after>
      <note>
        Institutional position remains net beneficiary but extracts less. 
        Classification stable as Rope unless Δε so large that χ becomes 
        positive (institutional position bears cost).
      </note>
    </agent>
  </chi_recalculation>

  <type_change>
    Depends on Δε magnitude:
    - Small reform (Δε < 0.33): Snare → Snare (insufficient)
    - Moderate reform (0.33 ≤ Δε < 0.53): Snare → Tangled Rope
    - Major reform (Δε ≥ 0.53): Snare → Rope
  </type_change>

  <preconditions>
    <condition>Evidence of bias sufficient to overcome institutional inertia</condition>
    <condition>Principal-agent alignment (institutional actors willing to reduce own extraction)</condition>
    <condition>No external pressure maintaining high ε (market competition, regulatory capture)</condition>
  </preconditions>

  <blocked_by>
    <rationality_model>
      Under PIR: Institutional agent maximizes utility → will not voluntarily 
      reduce ε unless Pareto-improving (impossible here — zero-sum redistribution).
      Under BIR: Institutional agent may satisfice under uncertainty, accept 
      moderate reform to avoid Revolutionary Rupture risk.
    </rationality_model>
  </blocked_by>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: Type III (Snare-as-Rope)

```xml
<error id="E₁">
  <type>Type III: Snare-as-Rope (Missing Extraction)</type>
  <agent>X₄</agent>
  <constraint>C₃</constraint>
  <actual_type>Tangled Rope (χ=0.64 from moderate index)</actual_type>
  <perceived_type>Rope (extraction normalized as coordination cost)</perceived_type>
  <observable>
    Agent accepts asymmetric cost distribution as necessary coordination 
    overhead. Does not investigate whether extraction could be reduced while 
    preserving coordination function. Treats ε=0.8 as immutable when it is 
    constructed and modifiable.
  </observable>
  <correction_trigger>
    Exposure to alternative systems with lower ε but equivalent coordination. 
    OR: Index shift to analytical position (π=1.15) revealing extraction 
    through degeneracy-breaking.
  </correction_trigger>
  <structural_consequence>
    Normalization prevents reform pressure. Institutional position (X₃) 
    benefits from this error as it stabilizes high-ε equilibrium.
  </structural_consequence>
</error>
```

---

### Error E₂: Type I (False Mountain)

```xml
<error id="E₂">
  <type>Type I: False Mountain</type>
  <agent>X₁</agent>
  <constraint>C₃</constraint>
  <actual_type>Snare (χ=0.96 from powerless index)</actual_type>
  <perceived_type>Mountain (treats institutional standard as natural law)</perceived_type>
  <observable>
    Agent does not attempt collective organization (TR₁) despite χ calculation 
    indicating transformation feasibility. Treats high extraction as 
    unchangeable background condition rather than constructed system.
  </observable>
  <correction_trigger>
    Boltzmann test demonstration: Show that classification varies by Power 
    dimension (X₃ experiences Rope, X₁ experiences Snare) → proves constraint 
    is constructed, not natural.
  </correction_trigger>
  <structural_consequence>
    Energy depletion from accepting unchangeable what could be changed. 
    Prevents TR₁ (collective organization) from triggering.
  </structural_consequence>
</error>
```

---

### Error E₃: Type II (Mountain Denial)

```xml
<error id="E₃">
  <type>Type II: Mountain Denial</type>
  <agent>X₂</agent>
  <constraint>C₁</constraint>
  <actual_type>Mountain (χ=0.046, passes Boltzmann test)</actual_type>
  <perceived_type>Rope (treats metaphysical invariant as changeable coordination mechanism)</perceived_type>
  <observable>
    Agent attempts to "capture" or "instrumentalize" fundamental constraint 
    through measurement system design. Expends energy trying to modify what 
    cannot be modified from any index.
  </observable>
  <correction_trigger>
    Repeated failure of instrumentalization attempts. Recognition that 
    constraint classification is stable across all tested indices (Boltzmann 
    independence).
  </correction_trigger>
  <structural_consequence>
    Energy depletion fighting unchangeable terrain. May lead to correct 
    classification of C₃ (constructed) but wastes resources on C₁ (natural).
  </structural_consequence>
</error>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Selected Model: Bounded Institutional Rationality (BIR)

**Justification:**

The constraint network involves human institutions (measurement standards, certification systems, guild structures) rather than algorithmic or natural law governance. Institutional agents face:

1. **Principal-agent problems:** X₃ (institutional position) may not perfectly align with system designers' intent
2. **Uncertainty:** Incomplete information about long-term consequences of maintaining high ε
3. **Risk aversion:** Potential for Revolutionary Rupture if extraction becomes unsustainable
4. **Satisficing behavior:** Institutional actors accept "good enough" equilibria rather than optimizing

**Implications:**

- TR₃ (baseline shift) becomes possible under BIR where it would be blocked under PIR
- Negotiated Equilibrium and Seeded Possibility attractors are reachable
- Institutional response to collective organization (TR₁) may involve compromise rather than pure suppression

**Contrast with PIR:**

Under Perfect Institutional Rationality, X₃ would maximize extraction without bounds, negotiate only Pareto-improving changes (impossible in zero-sum redistribution), and tend toward Deterministic Tragedy. The presence of high suppression (Supp=0.9 for C₃) indicates enforcement costs that suggest bounded rather than perfect rationality.

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected Attractor: Seeded Possibility

**Justification:**

The constraint network exhibits:

1. **Mountain-dominated foundation:** C₁ provides unchangeable background (metaphysical invariant)
2. **High-extraction Snare:** C₃ from powerless/analytical indices
3. **Indexical variance:** Same constraint classified differently across positions
4. **Deferred Piton presence:** C₄ (declared_hand) functions as expired Scaffold → Piton, indicating degraded coordination mechanism that persists

**Attractor Compatibility Matrix check:**

```
Profile: Mountain + Snares + Piton present
Rationality: BIR
Compatible attractors: Seeded Possibility ✓
```

**Structural dynamics:**

- **Surface trajectory:** C₃ runs to completion for X₁, X₂ (Snare from their indices) → appears as Deterministic Tragedy
- **Underground transformation:** TR₂ (alternative standard establishment) creates documentation infrastructure that does not immediately transform χ but seeds potential for future collective organization
- **Piton marker:** C₄ (declared_hand) represents vestigial ethical framework — function dried up but structure persists, providing symbolic resource for integrity maintenance

**Omega resolution:**

Ω(accumulation_effect) resolves as: Private documentation does NOT accumulate into immediate systemic change (TR₂ does not transform χ), BUT creates necessary precondition for TR₁ (collective organization) which WOULD transform χ if triggered. The attractor is "seeded possibility" rather than "realized transformation."

**Narrative implication (Stage 2 only):**

The story ends with surface tragedy (X₁, X₂ remain in Snare) but underground infrastructure (alternative measurement system, documented bias) that could enable future transformation. This is compatible with BIR (institutional actors may eventually respond to accumulated evidence) but not guaranteed.

---

## VI. VALIDATION CHECKLIST

```
☑ All constraints formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with π and σ values
☑ All characters use variable names (X₁, X₂, X₃, X₄) — no source identifiers in body
☑ Variable mapping table present in header (for traceability before anonymization)
☑ No source occupation, setting, or domain vocabulary anywhere in output
☑ No <experience>, <dialogue_markers>, or <narrative> fields
☑ Transformation rules use abstract trigger descriptions
☑ Error observables are testable structural conditions
☑ Institutional rationality model specified (BIR) with justification
☑ Terminal attractor selected (Seeded Possibility), justified, and compatible
☑ Indexical variance explicitly preserved across agents (C₃ high variance documented)
☑ Boltzmann test run for each constraint claiming Mountain status (C₁ PASS, C₂ FAIL, C₃ FAIL)
☑ No ambiguity in specifications
☑ Output is pure symbolic network — reader cannot identify source work
```

---

**END OF STAGE 1 FORMAL SPECIFICATION**