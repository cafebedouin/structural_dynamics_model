# STAGE 1: FORMAL CONSTRAINT NETWORK SPECIFICATION

## Variable Mapping Table

```
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Objective Value Unpossessable

```xml
<constraint id="C₁">
  <properties>
    <epsilon>0.00</epsilon>
    <suppression>0.00</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.00 × 1.5 × 0.8 = 0.00</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.00 ≤ 0.25 (mountain_extractiveness_max): PASS
      Supp = 0.00 ≤ 0.05 (mountain_suppression_ceiling): PASS
    </threshold_check>
    <rationale>
      Zero extraction, zero suppression. Natural emergence from epistemological 
      limits. Immutable from biographical horizon with trapped exit options.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>analytical</power>        <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.00 × 1.15 × 0.8 = 0.00</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.00 ≤ 0.25: PASS
      Supp = 0.00 ≤ 0.05: PASS
    </threshold_check>
    <rationale>
      Analytical position detects structure but cannot alter epistemological 
      foundation. Natural law status unchanged by detection capability.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.00 × 1.0 × 0.8 = 0.00</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.00 ≤ 0.25: PASS
      Supp = 0.00 ≤ 0.05: PASS
    </threshold_check>
    <rationale>
      Moderate power provides no leverage against epistemological limits. 
      Experience confirms rather than challenges immutability.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=0.00 → Mountain, X₂ χ=0.00 → Mountain, X₃ χ=0.00 → Mountain
    Zero variance. Classification invariant across all tested indices.
  </indexical_variance>

  <boltzmann_test>
    PASS
    
    Test matrix:
    - (powerless, local): χ = 0.00 → Mountain
    - (analytical, local): χ = 0.00 → Mountain  
    - (moderate, local): χ = 0.00 → Mountain
    - (institutional, national): χ = 0.00 → Mountain [hypothetical]
    
    Classification independent of P and S dimensions. Burden distribution 
    factorizable (zero burden × any modifier = zero burden). Natural law 
    candidate confirmed.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₂: Compromised Standard

```xml
<constraint id="C₂">
  <properties>
    <epsilon>0.80</epsilon>
    <suppression>0.90</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 0.96 > 0.70 (snare_chi_threshold): PASS
      ¬LowBaseExtraction: ε = 0.80 >> 0.25: PASS
    </threshold_check>
    <rationale>
      High base extraction amplified by powerless position. Trapped exit 
      options prevent escape. Coordination value invisible from this index—
      extraction dominates experience.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.0 × 0.8 = 0.64</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ = 0.64 ≤ 0.70: PASS (tangled_chi_floor to snare_chi_threshold)
      Coord = true: PASS
      Asym = true: PASS
    </threshold_check>
    <rationale>
      Moderate power dampens extraction to mid-range. Coordination value 
      becomes visible—system provides genuine function alongside extraction. 
      Identity-locked exit prevents escape but biographical horizon allows 
      navigation strategies.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>analytical</power>        <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.15 × 0.8 = 0.736</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 0.736 > 0.70: PASS
      ¬LowBaseExtraction: ε = 0.80 >> 0.25: PASS
    </threshold_check>
    <rationale>
      Analytical position breaks normalization—detects extraction that moderate 
      position absorbs as "how things work." π = 1.15 calibrated to surface 
      this degeneracy-breaking. Identity-locked prevents exit despite detection.
    </rationale>
  </agent>

  <agent ref="X₄">
    <index>
      <power>institutional</power>     <!-- π = -0.2 -->
      <time>generational</time>
      <exit>arbitrage</exit>
      <scope>national</scope>          <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.80 × (-0.2) × 1.0 = -0.16</chi>
    <type>Rope</type>
    <threshold_check>
      χ = -0.16 ≤ 0.35 (rope_chi_ceiling): PASS
      χ ≤ 0 → ε ceiling bypassed (net beneficiary clause): PASS
    </threshold_check>
    <rationale>
      Institutional position extracts FROM system rather than being extracted 
      from. Negative χ indicates net benefit. Arbitrage exit options allow 
      playing alternatives. Generational horizon makes system appear modifiable.
    </rationale>
  </agent>

  <agent ref="X₅">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 0.96 > 0.70: PASS
      ¬LowBaseExtraction: ε = 0.80 >> 0.25: PASS
    </threshold_check>
    <rationale>
      Identical structural position to X₁. Same amplification mechanism. 
      Represents broader class of agents bearing extraction burden.
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₁ χ=0.96 → Snare, X₃ χ=0.64 → Tangled Rope, X₂ χ=0.736 → Snare, 
        X₄ χ=-0.16 → Rope, X₅ χ=0.96 → Snare
    
    High variance. Same constraint classified as Rope (beneficiary), Tangled 
    Rope (navigator), and Snare (victim) depending on structural position. 
    
    Critical divergence: X₃ (moderate, π=1.0) normalizes as Tangled Rope while 
    X₂ (analytical, π=1.15) detects as Snare. This is the degeneracy-breaking 
    function of analytical position—surfaces extraction that moderate position 
    absorbs.
    
    Asymmetry property confirmed: χ varies by power position in non-factorizable 
    way. Extraction burden concentrated on powerless agents while institutional 
    agents extract net benefit.
  </indexical_variance>

  <boltzmann_test>
    FAIL
    
    Test matrix:
    - (powerless, local): χ = 0.96 → Snare
    - (moderate, local): χ = 0.64 → Tangled Rope
    - (institutional, national): χ = -0.16 → Rope
    
    Classification varies with P (power position). Burden distribution 
    non-factorizable—institutional agents experience net benefit while 
    powerless agents experience high extraction from identical constraint.
    
    Conclusion: C₂ is CONSTRUCTED, not natural. High suppression (0.90) 
    confirms enforcement requirement. Asymmetry property indicates designed 
    extraction gradient.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₃: Marginal Dissent

```xml
<constraint id="C₃">
  <properties>
    <epsilon>0.00</epsilon>
    <suppression>0.00</suppression>
    <coordination>true</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₃">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.00 × 1.0 × 0.8 = 0.00</chi>
    <type>Rope</type>
    <threshold_check>
      χ = 0.00 ≤ 0.35 (rope_chi_ceiling): PASS
      ε = 0.00 ≤ 0.45 (rope_epsilon_ceiling): PASS
      Coord = true: PASS
    </threshold_check>
    <rationale>
      Zero extraction, zero suppression. Voluntary coordination mechanism for 
      maintaining integrity within compromised system. Identity-locked indicates 
      chosen practice rather than imposed constraint.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>analytical</power>        <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.00 × 1.15 × 0.8 = 0.00</chi>
    <type>Rope</type>
    <threshold_check>
      χ = 0.00 ≤ 0.35: PASS
      ε = 0.00 ≤ 0.45: PASS
      Coord = true: PASS
    </threshold_check>
    <rationale>
      Analytical position detects structure but classification unchanged—zero 
      extraction remains zero regardless of detection capability. Coordination 
      function serves integrity maintenance.
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₃ χ=0.00 → Rope, X₂ χ=0.00 → Rope
    
    Zero variance among adopters. Classification invariant. This is a chosen 
    ethical practice—agents who adopt it experience identical coordination 
    function. Non-adopters not subject to constraint (not included in 
    formalization).
  </indexical_variance>

  <boltzmann_test>
    PASS
    
    Test matrix:
    - (moderate, local): χ = 0.00 → Rope
    - (analytical, local): χ = 0.00 → Rope
    
    Classification independent of P dimension among tested indices. However, 
    this is NOT a natural law—it's a constructed practice. Boltzmann pass 
    indicates symmetric burden distribution, not natural emergence.
    
    Distinction from C₁: C₁ is epistemological necessity (natural law). C₃ is 
    voluntary ethical practice (constructed coordination). Both pass Boltzmann 
    but for different reasons.
  </boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Collective Organization Attempt

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>organize_collective_action</condition>
    <target>C₂</target>
    <agent>X₁, X₅</agent>
  </trigger>
  
  <index_change>
    <from>P = powerless (π = 1.5)</from>
    <to>P = organized (π = 0.4)</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.80 × 1.5 × 0.8 = 0.96 → Snare</before>
    <after>χ = 0.80 × 0.4 × 0.8 = 0.256 → Rope</after>
  </chi_recalculation>
  
  <type_change>Snare → Rope</type_change>
  
  <preconditions>
    - Multiple agents at (powerless, trapped) index
    - Communication channels available
    - Coordination cost < expected benefit reduction
  </preconditions>
  
  <blocked_by>
    - C₂.suppression = 0.90 (high enforcement cost)
    - X₄ institutional position enables preemptive disruption
    - Asymmetric information (X₁, X₅ lack visibility into X₄ incentives)
  </blocked_by>
  
  <rationale>
    Collective organization shifts power position from powerless (π=1.5) to 
    organized (π=0.4). χ recalculation shows extraction drops below rope_chi_ceiling. 
    However, high suppression indicates X₄ has strong incentive to prevent this 
    transformation. Asymmetry property means X₄ experiences C₂ as Rope (χ=-0.16) 
    and will resist transformation that eliminates extraction gradient.
  </rationale>
</transformation_rule>
```

---

### TR₂: Analytical Detection Without Exit

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>detect_extraction_structure</condition>
    <target>C₂</target>
    <agent>X₂</agent>
  </trigger>
  
  <index_change>
    <from>P = moderate (π = 1.0)</from>
    <to>P = analytical (π = 1.15)</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.80 × 1.0 × 0.8 = 0.64 → Tangled Rope</before>
    <after>χ = 0.80 × 1.15 × 0.8 = 0.736 → Snare</after>
  </chi_recalculation>
  
  <type_change>Tangled Rope → Snare</type_change>
  
  <preconditions>
    - Access to comparative information
    - Cognitive capacity for structural analysis
    - Time horizon sufficient for pattern detection
  </preconditions>
  
  <blocked_by>
    - E = identity_locked (exit option unchanged)
    - Biographical time horizon (system appears immutable)
    - No alternative coordination mechanism visible
  </blocked_by>
  
  <rationale>
    Detection shifts classification but not material position. π increases from 
    1.0 to 1.15 (degeneracy-breaking calibration), pushing χ above snare_chi_threshold. 
    Agent now perceives extraction that was previously normalized as coordination.
    
    Critical: Exit options remain identity_locked. Detection without exit creates 
    Type III error risk—agent sees Snare but cannot escape, leading to either 
    resignation or adoption of C₃ (marginal dissent) as integrity-maintenance 
    strategy.
  </rationale>
</transformation_rule>
```

---

### TR₃: Marginal Dissent Adoption

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>adopt_integrity_practice</condition>
    <target>C₃</target>
    <agent>X₂, X₃</agent>
  </trigger>
  
  <index_change>
    <from>E = identity_locked (within C₂)</from>
    <to>E = identity_locked (within C₂ + C₃)</to>
  </index_change>
  
  <chi_recalculation>
    <before>C₂: χ = 0.736 (X₂) or 0.64 (X₃)</before>
    <after>C₂: χ unchanged; C₃: χ = 0.00 (new constraint adopted)</after>
  </chi_recalculation>
  
  <type_change>
    No type change for C₂. C₃ added as Rope (coordination mechanism).
  </type_change>
  
  <preconditions>
    - Detection of C₂ extraction structure (analytical position)
    - Recognition of exit impossibility (identity_locked)
    - Availability of C₃ as known practice
  </preconditions>
  
  <blocked_by>
    - None (voluntary adoption)
  </blocked_by>
  
  <rationale>
    C₃ adoption does not transform C₂—extraction structure persists. Instead, 
    agent adds voluntary coordination constraint (C₃) that provides integrity 
    maintenance within compromised system.
    
    This is NOT reform of C₂. It's acceptance of C₂ as Mountain (from biographical 
    horizon with identity_locked exit) plus construction of ethical practice for 
    navigation. Represents terminal state for agents who detect extraction but 
    cannot escape.
  </rationale>
</transformation_rule>
```

---

### TR₄: Institutional Extraction Intensification

```xml
<transformation_rule id="TR₄">
  <trigger>
    <condition>increase_extraction_efficiency</condition>
    <target>C₂</target>
    <agent>X₄</agent>
  </trigger>
  
  <index_change>
    <from>ε = 0.80</from>
    <to>ε = 0.90</to>
  </index_change>
  
  <chi_recalculation>
    <X₁>
      <before>χ = 0.80 × 1.5 × 0.8 = 0.96 → Snare</before>
      <after>χ = 0.90 × 1.5 × 0.8 = 1.08 → Snare</after>
    </X₁>
    <X₄>
      <before>χ = 0.80 × (-0.2) × 1.0 = -0.16 → Rope</before>
      <after>χ = 0.90 × (-0.2) × 1.0 = -0.18 → Rope</after>
    </X₄>
  </chi_recalculation>
  
  <type_change>
    X₁: Snare → Snare (intensified)
    X₄: Rope → Rope (increased benefit)
  </type_change>
  
  <preconditions>
    - X₄ at institutional position (π = -0.2)
    - Suppression capacity available (Supp = 0.90)
    - X₁, X₅ lack collective organization (powerless position)
  </preconditions>
  
  <blocked_by>
    - TR₁ (collective organization would shift power balance)
    - External constraint on extraction rate
  </blocked_by>
  
  <rationale>
    Institutional position enables unilateral ε increase. Asymmetry property 
    means extraction intensification increases both X₄ benefit (more negative χ) 
    and X₁ burden (more positive χ). High suppression (0.90) indicates enforcement 
    capacity to maintain increased extraction.
    
    This is the natural drift direction for asymmetric constraints under PIR 
    (Perfect Institutional Rationality). X₄ maximizes utility without bound; 
    only countervailing power (TR₁) or external limit prevents ε → 1.0.
  </rationale>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: Type III (Snare-as-Rope) — Missing Extraction

```xml
<error id="E₁">
  <type>III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X₃</agent>
  <constraint>C₂</constraint>
  <actual_type>Tangled Rope (from X₃ index: χ = 0.64)</actual_type>
  <perceived_type>Rope (coordination without extraction)</perceived_type>
  <observable>
    Does not investigate asymmetric burden distribution despite coordination 
    function being visible. Treats system as symmetric when χ(X₁) = 0.96 
    (Snare) while χ(X₃) = 0.64 (Tangled Rope).
  </observable>
  <correction_trigger>
    Shift to analytical position (π: 1.0 → 1.15) would reveal χ = 0.736 (Snare). 
    Alternatively, exposure to X₁ perspective showing χ = 0.96 would surface 
    extraction gradient.
  </correction_trigger>
  <rationale>
    Moderate power position (π = 1.0) dampens extraction to mid-range where 
    coordination value becomes visible. Agent correctly detects coordination 
    but fails to detect that this coordination is purchased through extraction 
    from powerless agents. Asymmetry property invisible from this index.
    
    This is NOT a cognitive failure—it's an index-dependent structural blind spot. 
    X₃'s classification (Tangled Rope) is objectively correct from their position. 
    Error is treating local classification as universal truth.
  </rationale>
</error>
```

---

### Error E₂: Type II (Mountain Denial) — Treating Unchangeable as Changeable

```xml
<error id="E₂">
  <type>II — Mountain Denial</type>
  <agent>X₁</agent>
  <constraint>C₁</constraint>
  <actual_type>Mountain (from all indices: χ = 0.00, immutable)</actual_type>
  <perceived_type>Rope (changeable through effort)</perceived_type>
  <observable>
    Attempts to establish objective value standard through individual action. 
    Expends energy seeking perfect measurement when epistemological limits 
    make this impossible.
  </observable>
  <correction_trigger>
    Recognition of C₁ as natural law (Boltzmann test pass, zero extraction, 
    zero suppression). Shift from immediate to biographical time horizon would 
    reveal immutability.
  </correction_trigger>
  <rationale>
    Immediate time horizon (T) makes many constraints appear changeable. 
    Trapped exit options (E) prevent testing immutability through escape. 
    Agent treats epistemological limit as social construction.
    
    Consequence: Energy depletion fighting unchangeable terrain. This error 
    feeds into C₂ by creating demand for compromised standard—if perfect 
    standard is impossible (C₁) but agent denies this (E₂), they become 
    vulnerable to accepting flawed substitute (C₂).
  </rationale>
</error>
```

---

### Error E₃: Type I (False Mountain) — Treating Changeable as Unchangeable

```xml
<error id="E₃">
  <type>I — False Mountain</type>
  <agent>X₁, X₅</agent>
  <constraint>C₂</constraint>
  <actual_type>Snare (from X₁ index: χ = 0.96, changeable via TR₁)</actual_type>
  <perceived_type>Mountain (unchangeable)</perceived_type>
  <observable>
    Does not attempt collective organization (TR₁) despite χ calculation 
    showing organized position (π = 0.4) would reduce χ to 0.256 (Rope). 
    Treats asymmetric extraction as natural law.
  </observable>
  <correction_trigger>
    Boltzmann test failure for C₂ demonstrates constructed nature. Exposure 
    to X₄ perspective (χ = -0.16, Rope) would reveal asymmetry. Recognition 
    that classification varies by power position indicates changeability.
  </correction_trigger>
  <rationale>
    Biographical time horizon + trapped exit options make C₂ appear immutable 
    from X₁ position. High suppression (0.90) reinforces perception of 
    unchangeability through enforcement visibility.
    
    Critical distinction from C₁: C₁ passes Boltzmann (natural law). C₂ fails 
    Boltzmann (constructed, asymmetric). But from (powerless, biographical, 
    trapped, local) index, both appear as Mountains. This is the structural 
    function of suppression—making constructed constraints appear natural.
    
    Error enables C₂ persistence by preventing TR₁ (collective organization). 
    If X₁ and X₅ recognized changeability, power shift would transform Snare → Rope.
  </rationale>
</error>
```

---

### Error E₄: Type V.a (Tangled-as-Rope) — Ignoring Extraction Component

```xml
<error id="E₄">
  <type>V.a — Tangled-as-Rope (Ignoring Extraction)</type>
  <agent>X₃</agent>
  <constraint>C₂</constraint>
  <actual_type>Tangled Rope (from X₃ index: χ = 0.64, Coord=true, Asym=true)</actual_type>
  <perceived_type>Rope (pure coordination)</perceived_type>
  <observable>
    Focuses exclusively on coordination function. Does not investigate why 
    χ(X₁) = 0.96 while χ(X₃) = 0.64 for same constraint. Treats asymmetry 
    as incidental rather than structural.
  </observable>
  <correction_trigger>
    Analytical position shift (TR₂) would reveal χ = 0.736 (Snare), forcing 
    recognition of extraction. Alternatively, direct exposure to X₁ burden 
    distribution would surface asymmetry.
  </correction_trigger>
  <rationale>
    Tangled Rope is irreducible hybrid—genuine coordination AND asymmetric 
    extraction. X₃ correctly detects coordination (Coord = true) but fails 
    to detect extraction gradient (Asym = true).
    
    This error is distinct from E₁ (Snare-as-Rope). E₁ misses extraction 
    entirely. E₄ detects coordination but treats extraction as acceptable 
    cost rather than structural asymmetry requiring reform.
    
    Consequence: Blocks reform efforts. If extraction is "just how things work" 
    rather than asymmetric burden, no impetus to pursue TR₁ (collective 
    organization) or other transformation.
  </rationale>
</error>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Selected Model: Perfect Institutional Rationality (PIR)

```xml
<rationality_model>
  <type>PIR</type>
  
  <justification>
    C₂ exhibits characteristics of implacable extraction system:
    
    1. High suppression (0.90) indicates enforcement without negotiation
    2. Asymmetry property shows designed extraction gradient
    3. X₄ institutional position (π = -0.2) enables unilateral action
    4. No evidence of principal-agent problems or satisficing behavior
    5. TR₄ (extraction intensification) shows utility maximization without bound
    
    X₄ behavior consistent with PIR assumptions:
    - Maximize extraction (ε) subject only to suppression capacity
    - No negotiation with X₁, X₅ except Pareto-improving (none available)
    - Resist TR₁ (collective organization) as it reduces extraction
    
    C₁ (epistemological limit) reinforces PIR by eliminating appeal to 
    objective standard. With perfect standard impossible, compromised 
    standard becomes inevitable.
  </justification>
  
  <implications>
    Under PIR, C₂ tends toward:
    - ε → 1.0 (maximum extraction)
    - Supp → 1.0 (maximum enforcement)
    - Asymmetry intensification (χ(X₁) increases, χ(X₄) decreases)
    
    Countervailing forces:
    - TR₁ blocked by high suppression and asymmetric information
    - TR₂ (analytical detection) creates awareness but not power
    - TR₃ (marginal dissent) accepts system, provides only integrity maintenance
    
    Terminal state prediction: Deterministic Tragedy or Seeded Possibility
    (see Attractor Selection below)
  </implications>
  
  <alternative_rejected>
    <type>BIR</type>
    <reason>
      No evidence of:
      - Satisficing behavior (X₄ maximizes, doesn't satisfice)
      - Principal-agent problems (X₄ acts as unified institutional agent)
      - Risk aversion (high suppression indicates willingness to enforce)
      - Negotiated equilibrium seeking (asymmetry intensifies, doesn't stabilize)
      
      If BIR were operative,

<!-- ANONYMIZATION: 5 character names replaced: Apprentice -> Agent_A, Mentor -> Agent_B, Master -> Agent_C, Authority -> Agent_D, Subject -> Agent_E -->
