# STAGE 1: FORMAL CONSTRAINT SPECIFICATION

## Variable Mapping (Header — Traceability Only)

```
G₁ ← Guild system
I₁ ← Royal certification authority
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Fundamental Measurement Impossibility

```xml
<constraint id="C₁">
  <base_properties>
    <epsilon>0.05</epsilon>
    <suppression>0.0</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </base_properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.05 × 1.5 × 0.8 = 0.06</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.05 ≤ 0.25 (mountain_extractiveness_max) → PASS
      Supp = 0.0 ≤ 0.05 (mountain_suppression_ceiling) → PASS
    </threshold_check>
    <rationale>
      Natural epistemological limit. The act of measurement/possession 
      fundamentally alters the state being measured. Zero suppression 
      (no enforcement needed). Minimal extraction (affects all equally). 
      Immutable from biographical horizon.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.05 × 1.15 × 0.8 = 0.046</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.05 ≤ 0.25 → PASS
      Supp = 0.0 ≤ 0.05 → PASS
    </threshold_check>
    <rationale>
      Analytical position provides clarity on the impossibility but does 
      not reduce it. Natural law independent of observer power position.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=0.06 → Mountain, X₂ χ=0.046 → Mountain
    Zero variance. Natural law classification invariant across indices.
  </indexical_variance>

  <boltzmann_test>
    PASS. Classification invariant across Power (powerless vs analytical) 
    and Scope dimensions. χ varies slightly due to π modifier, but type 
    remains Mountain for all tested indices. Burden does not systematically 
    vary by structural position → natural emergence confirmed.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₂: Arbitrary Baseline Construction

```xml
<constraint id="C₂">
  <base_properties>
    <epsilon>0.5</epsilon>
    <suppression>0.5</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </base_properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.5 × 1.5 × 0.9 = 0.675</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ = 0.675 ≤ 0.70 (tangled_chi_range) → PASS
      Coord = true ∧ Asym = true → PASS
    </threshold_check>
    <rationale>
      System provides genuine coordination (common reference enables trade) 
      but extraction is moderate and asymmetric. Powerless position amplifies 
      cost of arbitrary baseline. Cannot exit (trapped in regional economy). 
      Coordination value prevents Snare classification despite χ approaching 
      threshold.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.5 × 1.15 × 0.9 = 0.518</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ = 0.518 ≤ 0.70 → PASS
      Coord = true ∧ Asym = true → PASS
    </threshold_check>
    <rationale>
      Analytical position reduces experienced extraction (π = 1.15 vs 1.5) 
      but does not eliminate it. Recognizes arbitrariness of baseline but 
      cannot escape system (identity_locked to professional role). Mid-range 
      χ with coordination → Tangled Rope.
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₁ χ=0.675 → Tangled Rope, X₂ χ=0.518 → Tangled Rope
    Low variance. Both classify as Tangled Rope despite χ difference. 
    Power position modulates extraction intensity but not type. Asymmetric 
    coordination-extraction hybrid stable across powerless/analytical divide.
  </indexical_variance>

  <boltzmann_test>
    FAIL. Classification would vary if tested at institutional power position 
    (π = -0.2 would yield χ = -0.09 → Rope). Burden systematically varies by 
    power → constructed system, not natural law. Suppression = 0.5 confirms 
    enforcement requirement.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₃: Sanctioned Measurement Inaccuracy

```xml
<constraint id="C₃">
  <base_properties>
    <epsilon>0.8</epsilon>
    <suppression>0.9</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </base_properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.5 × 0.9 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 1.08 > 0.70 (snare_chi_threshold) → PASS
      ε = 0.8 > 0.25 (not low base extraction) → PASS
    </threshold_check>
    <rationale>
      High extraction (ε = 0.8) amplified by powerless position. Official 
      certification creates asymmetric cost: powerless agents bear full 
      burden of systematic inaccuracy while institutional agents extract 
      value. High suppression (0.9) indicates coercive enforcement. 
      Coordination claim (common standard) dominated by extraction reality.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.15 × 0.9 = 0.828</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 0.828 > 0.70 → PASS
      ε = 0.8 > 0.25 → PASS
    </threshold_check>
    <rationale>
      Analytical position reduces χ from 1.08 to 0.828 but remains above 
      Snare threshold. Recognition of systematic inaccuracy does not provide 
      escape (identity_locked). High suppression prevents deviation from 
      official standard. Extraction dominates coordination value.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>institutional</power>        <!-- π = -0.2 -->
      <time>generational</time>
      <exit>arbitrage</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.8 × (-0.2) × 0.9 = -0.144</chi>
    <type>Rope</type>
    <threshold_check>
      χ = -0.144 ≤ 0.35 (rope_chi_ceiling) → PASS
      χ ≤ 0 → ε ceiling bypassed (net beneficiary)
    </threshold_check>
    <rationale>
      Institutional position inverts extraction (π = -0.2 → negative χ). 
      Net beneficiary: extracts value FROM systematic inaccuracy. Coordination 
      function genuine from this index (standardization enables control). 
      Arbitrage exit option: can play alternative systems against each other.
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₁ χ=1.08 → Snare, X₂ χ=0.828 → Snare, X₃ χ=-0.144 → Rope
    HIGH VARIANCE. Same constraint exhibits maximum type divergence:
    - Powerless/analytical: Snare (high extraction trap)
    - Institutional: Rope (coordination mechanism with net benefit)
    
    Demonstrates core indexed relativity principle: structural position 
    determines which properties of C₃ are visible/dominant. All three 
    classifications are objectively true from their respective indices.
  </indexical_variance>

  <boltzmann_test>
    FAIL. Classification varies systematically by Power dimension:
    - π = 1.5 (powerless) → Snare
    - π = 1.15 (analytical) → Snare  
    - π = -0.2 (institutional) → Rope
    
    Burden distribution non-factorizable → constructed system. High 
    suppression (0.9) confirms enforcement apparatus. This is a designed 
    extraction mechanism, not natural law.
  </boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Collective Organization Attempt

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>organize_collective_measurement_system</condition>
    <target>C₂, C₃</target>
    <agent>X₁ or X₂</agent>
  </trigger>

  <index_change>
    <from>P = powerless (π=1.5) or analytical (π=1.15)</from>
    <to>P = organized (π=0.4)</to>
  </index_change>

  <chi_recalculation>
    <constraint>C₂</constraint>
    <before>
      X₁: χ = 0.5 × 1.5 × 0.9 = 0.675 → Tangled Rope
      X₂: χ = 0.5 × 1.15 × 0.9 = 0.518 → Tangled Rope
    </before>
    <after>
      χ = 0.5 × 0.4 × 0.9 = 0.18 → Rope
    </after>
  </chi_recalculation>

  <chi_recalculation>
    <constraint>C₃</constraint>
    <before>
      X₁: χ = 0.8 × 1.5 × 0.9 = 1.08 → Snare
      X₂: χ = 0.8 × 1.15 × 0.9 = 0.828 → Snare
    </before>
    <after>
      χ = 0.8 × 0.4 × 0.9 = 0.288 → Rope
    </after>
  </chi_recalculation>

  <type_change>
    C₂: Tangled Rope → Rope
    C₃: Snare → Rope
  </type_change>

  <preconditions>
    - Multiple agents at powerless/analytical positions
    - Communication channels available
    - Collective action not suppressed by C₃ enforcement
  </preconditions>

  <blocked_by>
    C₃ suppression (0.9) creates high barrier to coordination.
    Institutional agents (X₃) have incentive to prevent (would lose 
    extraction benefit). Requires overcoming coordination problem under 
    active suppression.
  </blocked_by>
</transformation_rule>
```

---

### TR₂: Analytical Detachment

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>exit_to_pure_analytical_position</condition>
    <target>C₂, C₃</target>
    <agent>X₂</agent>
  </trigger>

  <index_change>
    <from>E = identity_locked, P = analytical (π=1.15)</from>
    <to>E = analytical, P = analytical (π=1.15)</to>
  </index_change>

  <chi_recalculation>
    <constraint>C₂</constraint>
    <before>χ = 0.5 × 1.15 × 0.9 = 0.518 → Tangled Rope</before>
    <after>χ = 0 (not subject to constraint) → N/A</after>
  </chi_recalculation>

  <chi_recalculation>
    <constraint>C₃</constraint>
    <before>χ = 0.8 × 1.15 × 0.9 = 0.828 → Snare</before>
    <after>χ = 0 (not subject to constraint) → N/A</after>
  </chi_recalculation>

  <type_change>
    Agent exits constraint field entirely. Can analyze without being subject.
  </type_change>

  <preconditions>
    - Ability to exit professional role (overcome identity_locked)
    - Alternative livelihood available
    - Willingness to abandon embedded position
  </preconditions>

  <blocked_by>
    Identity fusion to professional role. Economic dependence on system. 
    Social cost of exit. C₃ may impose penalties on exit (suppression=0.9).
  </blocked_by>
</transformation_rule>
```

---

### TR₃: Institutional Capture

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>achieve_institutional_position</condition>
    <target>C₂, C₃</target>
    <agent>X₁ or X₂</agent>
  </trigger>

  <index_change>
    <from>P = powerless (π=1.5) or analytical (π=1.15)</from>
    <to>P = institutional (π=-0.2)</to>
  </index_change>

  <chi_recalculation>
    <constraint>C₂</constraint>
    <before>
      X₁: χ = 0.5 × 1.5 × 0.9 = 0.675 → Tangled Rope
      X₂: χ = 0.5 × 1.15 × 0.9 = 0.518 → Tangled Rope
    </before>
    <after>
      χ = 0.5 × (-0.2) × 0.9 = -0.09 → Rope
    </after>
  </chi_recalculation>

  <chi_recalculation>
    <constraint>C₃</constraint>
    <before>
      X₁: χ = 0.8 × 1.5 × 0.9 = 1.08 → Snare
      X₂: χ = 0.8 × 1.15 × 0.9 = 0.828 → Snare
    </before>
    <after>
      χ = 0.8 × (-0.2) × 0.9 = -0.144 → Rope
    </after>
  </chi_recalculation>

  <type_change>
    C₂: Tangled Rope → Rope (net beneficiary)
    C₃: Snare → Rope (extraction inverted)
  </type_change>

  <preconditions>
    - Access to institutional power structure
    - Willingness to become extraction beneficiary
    - Acceptance by existing institutional agents
  </preconditions>

  <blocked_by>
    Institutional positions limited (zero-sum). Existing institutional 
    agents defend positions. May require abandoning solidarity with 
    powerless agents. High entry barriers.
  </blocked_by>
</transformation_rule>
```

---

### TR₄: Baseline Drift (C₂ Degradation)

```xml
<transformation_rule id="TR₄">
  <trigger>
    <condition>accumulate_extraction_without_coordination_loss</condition>
    <target>C₂</target>
    <agent>System-level (no specific agent)</agent>
  </trigger>

  <index_change>
    <from>N/A (constraint property change, not index change)</from>
    <to>N/A</to>
  </index_change>

  <chi_recalculation>
    <constraint>C₂</constraint>
    <before>
      ε = 0.5, Coord = true, Asym = true
      X₁: χ = 0.675 → Tangled Rope
    </before>
    <after>
      ε = 0.65, Coord = true, Asym = true
      X₁: χ = 0.65 × 1.5 × 0.9 = 0.878 → Snare
    </after>
  </chi_recalculation>

  <type_change>
    C₂: Tangled Rope → Snare (Transition T2: coordination loss dominance)
  </type_change>

  <preconditions>
    - Institutional agents control baseline definition
    - Gradual extraction increase below detection threshold
    - Coordination function persists (prevents immediate collapse)
  </preconditions>

  <blocked_by>
    Collective monitoring of baseline. Alternative measurement systems. 
    Institutional competition. Requires sustained institutional control 
    without external accountability.
  </blocked_by>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: Type I — False Mountain (C₂)

```xml
<error id="E₁">
  <type>Type I: False Mountain</type>
  <agent>X₁</agent>
  <constraint>C₂</constraint>
  <actual_type>Tangled Rope (χ=0.675, changeable)</actual_type>
  <perceived_type>Mountain (unchangeable natural law)</perceived_type>
  <observable>
    Agent does not attempt collective organization despite:
    - Multiple agents at similar power positions
    - Communication channels available
    - χ calculation indicating organized position (π=0.4) would yield 
      χ=0.18 → Rope transformation
    
    Testable: Does agent treat baseline as negotiable vs. fixed?
  </observable>
  <correction_trigger>
    Exposure to alternative baseline systems. Historical evidence of 
    baseline changes. Successful collective organization in adjacent domain. 
    Index shift to analytical position (π=1.15) may provide clarity but 
    not necessarily action.
  </correction_trigger>
</error>
```

---

### Error E₂: Type III — Snare-as-Rope (C₃, Institutional View)

```xml
<error id="E₂">
  <type>Type III: Snare-as-Rope (Missing Extraction)</type>
  <agent>X₃</agent>
  <constraint>C₃</constraint>
  <actual_type>
    Snare from powerless index (X₁: χ=1.08)
    Rope from institutional index (X₃: χ=-0.144)
  </actual_type>
  <perceived_type>
    Pure Rope (coordination mechanism with no extraction component)
  </perceived_type>
  <observable>
    Agent defends system as purely coordinative, dismissing extraction 
    claims as:
    - Misunderstanding of necessary standardization
    - Failure to appreciate coordination value
    - Resistance to legitimate authority
    
    Testable: Does agent acknowledge asymmetric burden distribution? 
    Does agent recognize χ(powerless) > 0.70 while χ(institutional) < 0?
  </observable>
  <correction_trigger>
    Forced index shift to powerless position (loss of institutional power). 
    External audit revealing extraction asymmetry. Collective action by 
    powerless agents making extraction costs visible. Unlikely without 
    structural position change — institutional position shields from 
    extraction experience.
  </correction_trigger>
</error>
```

---

### Error E₃: Type V.a — Tangled-as-Rope (C₂)

```xml
<error id="E₃">
  <type>Type V.a: Tangled-as-Rope (Ignoring Extraction Component)</type>
  <agent>X₂</agent>
  <constraint>C₂</constraint>
  <actual_type>Tangled Rope (χ=0.518, Coord=true, Asym=true)</actual_type>
  <perceived_type>Rope (pure coordination, extraction minimized)</perceived_type>
  <observable>
    Agent emphasizes coordination value while normalizing extraction:
    - "System works well enough"
    - "Baseline is arbitrary but necessary"
    - "Costs are distributed fairly" (ignoring asymmetry)
    
    Testable: Does agent recognize χ(powerless)=0.675 vs χ(analytical)=0.518? 
    Does agent acknowledge Asym=true?
  </observable>
  <correction_trigger>
    Comparison with lower-extraction alternative. Explicit calculation of 
    χ across power positions. Exposure to powerless agent experiences. 
    Analytical position (π=1.15) should enable recognition but may be 
    blocked by identity_locked exit status (professional investment in 
    system).
  </correction_trigger>
</error>
```

---

### Error E₄: Analytical Powerlessness Paradox

```xml
<error id="E₄">
  <type>Omega-class: Analytical Powerlessness Paradox</type>
  <agent>X₂</agent>
  <constraint>C₃</constraint>
  <actual_type>Snare (χ=0.828)</actual_type>
  <perceived_type>
    Correctly classified as Snare, but misunderstands π modifier effect
  </perceived_type>
  <observable>
    Agent experiences:
    - Correct classification (Snare recognition)
    - Reduced χ compared to powerless (0.828 vs 1.08)
    - BUT: No reduction in experienced burden
    
    Paradox: Formula predicts π=1.15 reduces extraction, but narrative 
    suggests analytical clarity provides only recognition, not relief.
    
    Testable: Does analytical position reduce actual extraction or only 
    increase awareness of extraction? Does χ(analytical) < χ(powerless) 
    reflect genuine burden reduction or measurement artifact?
  </observable>
  <correction_trigger>
    UNRESOLVED. Requires empirical validation of π(analytical)=1.15 
    calibration. Possible resolutions:
    
    1. π(analytical) should equal π(powerless)=1.5 when exit=identity_locked
       (analysis without agency provides no relief)
    
    2. χ formula measures structural extraction, not experienced burden
       (analytical position reduces objective extraction but not subjective 
       experience due to heightened awareness)
    
    3. π modifier is correct but narrative emphasizes psychological cost
       of clarity, which is orthogonal to χ
    
    Requires cross-narrative validation to resolve.
  </correction_trigger>
</error>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Selected Model: Bounded Institutional Rationality (BIR)

**Justification:**

```
Institutional agents (X₃, I₁) exhibit:
- Satisficing behavior (maintain extraction at sustainable level, not maximize)
- Principal-agent problems (certification authority vs. enforcement apparatus)
- Risk aversion (high suppression=0.9 suggests fear of system collapse)
- Negotiation capacity (C₂ baseline is adjustable, not fixed)

Evidence against PIR:
- C₃ extraction (ε=0.8) is high but not maximal
- Suppression (0.9) is high but not perfect (allows some deviation)
- System tolerates analytical positions (X₂) that recognize extraction
- No evidence of algorithmic/implacable enforcement

BIR implications:
- Institutional agents will negotiate if threatened
- System can reach equilibrium through bargaining
- Revolutionary rupture unlikely (institutions adapt)
- Seeded Possibility attractor accessible (underground alternatives tolerated)
```

**Attractor Compatibility:**

```
Constraint Profile:
- 1 Mountain (C₁)
- 1 Tangled Rope (C₂) 
- 1 Snare (C₃) with high indexical variance

Under BIR:
✓ Negotiated Equilibrium (Tangled Rope dominant, institutions negotiate)
✓ Seeded Possibility (Mountain + Snare + BIR → surface compliance, underground transformation)
✗ Deterministic Tragedy (requires PIR)
✗ Revolutionary Rupture (requires pure Snares without Mountains)
```

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected: Seeded Possibility

**Structural Justification:**

```
1. Mountain Foundation (C₁):
   Fundamental measurement impossibility is unchangeable. Any resolution 
   must work within this constraint, not against it.

2. High-Variance Snare (C₃):
   Extreme indexical variance (Snare for X₁/X₂, Rope for X₃) creates 
   underground/overground split. Powerless agents experience trap while 
   institutional agents experience coordination.

3. BIR Institutional Model:
   Institutions satisfice, not maximize. Will tolerate underground 
   alternatives if surface compliance maintained. High suppression (0.9) 
   indicates enforcement capacity but not totalitarian control.

4. Transformation Rule Accessibility:
   TR₁ (collective organization) is blocked by C₃ suppression but not 
   impossible. TR₂ (analytical detachment) is accessible. TR₃ (institutional 
   capture) is zero-sum but possible. Underground transformation can occur 
   while surface system persists.
```

**Attractor Mechanics:**

```
Surface (Visible):
- C₃ maintains high extraction from powerless agents
- Institutional agents continue to benefit
- Official system appears stable
- Suppression prevents overt collective organization

Underground (Invisible):
- Analytical agents (X₂) develop alternative measurement practices
- Informal networks share knowledge of baseline arbitrariness
- Powerless agents (X₁) engage in quiet non-compliance
- Alternative systems seed within official framework

Terminal State:
- Official system persists (institutions satisfied)
- Extraction continues but at reduced effectiveness
- Underground alternatives grow without triggering suppression
- Eventual transformation through accumulated underground practice, 
  not revolutionary rupture
```

**Compatibility Check:**

```
✓ Mountain-dominated: C₁ provides unchangeable foundation
✓ BIR model: Institutions tolerate underground as long as surface stable
✓ High-variance Snare: Creates underground/overground split
✓ Transformation rules: TR₂ accessible, TR₁ possible if suppression weakens
✓ Error manifestations: E₄ (analytical paradox) drives underground development
```

---

## VI. VALIDATION CHECKLIST

```
☑ All constraints formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with π and σ values
☑ All characters use variable names (X₁, X₂, X₃) — no source identifiers in body
☑ Variable mapping table present in header (for traceability)
☑ No source occupation, setting, or domain vocabulary in output
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

<!-- ANONYMIZATION: 3 character names replaced with structural labels: Agent_A, Agent_B, Agent_C -->
