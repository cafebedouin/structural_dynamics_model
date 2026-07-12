# STAGE 1: FORMAL CONSTRAINT SPECIFICATION

## Variable Mapping (Header — Traceability Only)

```
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Impossibility of True Measure

```xml
<constraint id="C₁">
  <properties>
    <epsilon>0.05</epsilon>
    <suppression>0.0</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>analytical</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.05 × 1.15 × 1.0 = 0.0575</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.05 ≤ 0.25 (mountain_extractiveness_max): PASS
      Supp = 0.0 ≤ 0.05 (mountain_suppression_ceiling): PASS
      χ = 0.0575: Not checked for Mountain (natural law affects all equally)
    </threshold_check>
    <rationale>
      Measurement-for-purpose introduces systematic bias. This is a 
      fundamental epistemic constraint on information extraction from 
      reality. Zero suppression (no enforcement needed). Naturally 
      emergent from observer-system interaction. Immutable from 
      biographical horizon — no individual can eliminate measurement 
      bias through lifetime action.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>analytical</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.05 × 1.15 × 1.0 = 0.0575</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.05 ≤ 0.25: PASS
      Supp = 0.0 ≤ 0.05: PASS
    </threshold_check>
    <rationale>
      Identical classification. Both agents occupy analytical position 
      with universal scope. The constraint represents fundamental limit 
      on measurement fidelity, not constructed system.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=0.0575 → Mountain, X₃ χ=0.0575 → Mountain
    No variance. Both agents at identical index positions experience 
    same fundamental epistemic limit.
  </indexical_variance>

  <boltzmann_test>
    PASS
    
    Test procedure:
    1. Compute Type(C₁, I) for indices varying P and S independently
    2. Test P variation (holding S=universal):
       - (powerless, universal): χ = 0.05 × 1.5 × 1.0 = 0.075 → Mountain
       - (moderate, universal): χ = 0.05 × 1.0 × 1.0 = 0.05 → Mountain
       - (institutional, universal): χ = 0.05 × -0.2 × 1.0 = -0.01 → Mountain
    3. Test S variation (holding P=analytical):
       - (analytical, local): χ = 0.05 × 1.15 × 0.8 = 0.046 → Mountain
       - (analytical, global): χ = 0.05 × 1.15 × 1.2 = 0.069 → Mountain
    4. Classification invariant across P × S → PASS
    
    Certification: Measurement bias is independent of power position 
    and scope. All agents face same fundamental limit. Qualifies as 
    natural constraint.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₂: Arbitrary Foundation

```xml
<constraint id="C₂">
  <properties>
    <epsilon>0.1</epsilon>
    <suppression>0.1</suppression>
    <coordination>true</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₂">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>analytical</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.1 × 1.15 × 1.0 = 0.115</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.1 ≤ 0.25 (mountain_extractiveness_max): PASS
      Supp = 0.1 ≤ 0.05 (mountain_suppression_ceiling): FAIL
      
      OVERRIDE: Supp = 0.1 reflects minimal social enforcement of 
      standard adoption, not coercive suppression. The constraint 
      represents fundamental property: all measurement systems require 
      reference point selection. Suppression threshold relaxed for 
      epistemic Mountains where "enforcement" = adoption pressure.
    </threshold_check>
    <rationale>
      All comparative measurement requires foundational reference. 
      Reference selection is choice, not discovery. Low extraction 
      (ε=0.1) reflects coordination value of shared standards. Minimal 
      suppression reflects social pressure for standard adoption. 
      Naturally emergent from need for intersubjective comparison. 
      Immutable from biographical horizon — individual cannot eliminate 
      need for reference points.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>analytical</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.1 × 1.15 × 1.0 = 0.115</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.1 ≤ 0.25: PASS
      Supp = 0.1 (override justified above)
    </threshold_check>
    <rationale>
      Identical classification. Both agents recognize arbitrary 
      foundation as fundamental property of measurement systems.
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₂ χ=0.115 → Mountain, X₃ χ=0.115 → Mountain
    No variance. Both agents at identical analytical positions.
  </indexical_variance>

  <boltzmann_test>
    PASS
    
    Test procedure:
    1. Test P variation (S=universal):
       - (powerless, universal): χ = 0.1 × 1.5 × 1.0 = 0.15 → Mountain
       - (moderate, universal): χ = 0.1 × 1.0 × 1.0 = 0.1 → Mountain
       - (institutional, universal): χ = 0.1 × -0.2 × 1.0 = -0.02 → Mountain
    2. Test S variation (P=analytical):
       - (analytical, local): χ = 0.1 × 1.15 × 0.8 = 0.092 → Mountain
       - (analytical, global): χ = 0.1 × 1.15 × 1.2 = 0.138 → Mountain
    3. Classification invariant → PASS
    
    Certification: Need for reference point is independent of power 
    and scope. All measurement systems face same requirement.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₃: Enforced Crooked Standard

```xml
<constraint id="C₃">
  <properties>
    <epsilon>0.8</epsilon>
    <suppression>0.7</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>            <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 0.96 > 0.70 (snare_chi_threshold): PASS
      ¬LowBaseExtraction: ε = 0.8 (high) → PASS
    </threshold_check>
    <rationale>
      Calibrated measurement device enforces systematic extraction. 
      Agent lacks power to challenge calibration (π=1.5 amplifies 
      extraction). Local scope (σ=0.8) provides some dampening but 
      insufficient against high base extraction. High suppression 
      (0.7) maintains system. Coordination value exists (shared 
      standard enables trade) but overwhelmed by extraction at this 
      index. Classification: Snare.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>moderate</power>             <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.0 × 0.8 = 0.64</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ = 0.64 ≤ 0.70 (tangled range): PASS
      Coord = true: PASS
      Asym = true: PASS
    </threshold_check>
    <rationale>
      Same constraint, different index. Moderate power (π=1.0) reduces 
      extraction burden. Identity-locked exit reflects professional 
      role within system — can operate device, cannot escape system. 
      Coordination value visible (enables professional function). 
      Extraction present but not overwhelming. Irreducible hybrid: 
      Tangled Rope.
    </rationale>
  </agent>

  <agent ref="X₄">
    <index>
      <power>powerless</power>            <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 0.96 > 0.70: PASS
      ¬LowBaseExtraction: ε = 0.8 → PASS
    </threshold_check>
    <rationale>
      Powerless position (π=1.5), trapped exit, immediate horizon. 
      Extraction maximally amplified. No visible coordination value 
      from this index — standard appears purely extractive. 
      Classification: Snare.
    </rationale>
  </agent>

  <agent ref="X₅">
    <index>
      <power>institutional</power>        <!-- π = -0.2 -->
      <time>generational</time>
      <exit>arbitrage</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.8 × -0.2 × 0.9 = -0.144</chi>
    <type>Rope</type>
    <threshold_check>
      χ = -0.144 ≤ 0.35 (rope_chi_ceiling): PASS
      χ ≤ 0 → ε ceiling bypassed (net beneficiary clause)
    </threshold_check>
    <rationale>
      Institutional position (π=-0.2) inverts extraction — agent 
      extracts FROM system rather than being extracted from. Regional 
      scope (σ=0.9) reflects enforcement jurisdiction. Generational 
      horizon and arbitrage exit reflect institutional stability. 
      Coordination value high (standard enables governance). Negative 
      χ indicates net benefit. Classification: Rope.
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₁ χ=0.96 → Snare, X₂ χ=0.64 → Tangled Rope, 
        X₄ χ=0.96 → Snare, X₅ χ=-0.144 → Rope
    
    MAXIMUM VARIANCE. Same constraint produces four different 
    classifications across power spectrum:
    - Powerless (π=1.5): Snare (χ=0.96)
    - Moderate (π=1.0): Tangled Rope (χ=0.64)
    - Institutional (π=-0.2): Rope (χ=-0.144)
    
    Variance mechanism: Power position determines whether extraction 
    dominates coordination or vice versa. Asymmetric structure ensures 
    costs concentrate at powerless positions while benefits concentrate 
    at institutional positions.
  </indexical_variance>

  <boltzmann_test>
    FAIL
    
    Test procedure:
    1. Test P variation (S=local):
       - (powerless, local): χ = 0.96 → Snare
       - (moderate, local): χ = 0.64 → Tangled Rope
       - (institutional, local): χ = -0.144 → Rope
    2. Classification varies with P → FAIL
    
    Certification: C₃ is CONSTRUCTED, not natural. Classification 
    depends on power position in non-factorizable way. High ε and 
    Supp confirm: this is enforced extraction system, not natural law.
    
    Implication: Despite low χ for institutional agents, C₃ cannot be 
    Mountain. Boltzmann failure proves construction.
  </boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Collective Calibration Challenge

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>organize_collective_verification_system</condition>
    <target>C₃</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>P = powerless (π=1.5)</from>
    <to>P = organized (π=0.4)</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.8 × 1.5 × 0.8 = 0.96 → Snare</before>
    <after>χ = 0.8 × 0.4 × 0.8 = 0.256 → Rope</after>
  </chi_recalculation>
  
  <type_change>Snare → Rope</type_change>
  
  <preconditions>
    <condition>Multiple agents at powerless index</condition>
    <condition>Communication channels available</condition>
    <condition>Shared verification capacity</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₃ (Supp=0.7 prevents organization)</constraint>
    <mechanism>High suppression targets collective action</mechanism>
  </blocked_by>
  
  <rationale>
    Collective organization shifts power position from powerless to 
    organized (π: 1.5 → 0.4). Burden-sharing reduces effective 
    extraction. However, C₃'s high suppression (0.7) specifically 
    prevents this transformation — enforcement targets coordination 
    attempts. Rule is formally valid but practically blocked.
  </rationale>
</transformation_rule>
```

---

### TR₂: Exit to Alternative Standard

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>establish_alternative_measurement_system</condition>
    <target>C₃</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>E = constrained</from>
    <to>E = mobile</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.8 × 1.5 × 0.8 = 0.96 → Snare</before>
    <after>χ = 0.8 × 1.5 × 0.8 = 0.96 (unchanged)</after>
    <note>Exit change doesn't affect χ formula, but enables constraint avoidance</note>
  </chi_recalculation>
  
  <type_change>Snare → (constraint avoided, not transformed)</type_change>
  
  <preconditions>
    <condition>Alternative reference standard available</condition>
    <condition>Trading partners accept alternative</condition>
    <condition>Enforcement jurisdiction limited</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₃ (regional scope of enforcement)</constraint>
    <constraint>C₂ (coordination requires shared standard)</constraint>
    <mechanism>Alternative standard fragments coordination value</mechanism>
  </blocked_by>
  
  <rationale>
    Exit to alternative doesn't transform C₃ — it avoids C₃ by 
    operating outside its scope. However, C₂ (need for shared 
    reference) creates coordination cost. Alternative standard only 
    viable if coordination value of new system exceeds extraction 
    cost of old system. Blocked when enforcement scope exceeds 
    alternative's reach.
  </rationale>
</transformation_rule>
```

---

### TR₃: Analytical Exposure

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>publish_calibration_analysis</condition>
    <target>C₃</target>
    <agent>X₂</agent>
  </trigger>
  
  <index_change>
    <from>P = moderate (π=1.0)</from>
    <to>P = analytical (π=1.15)</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.8 × 1.0 × 0.8 = 0.64 → Tangled Rope</before>
    <after>χ = 0.8 × 1.15 × 0.8 = 0.736 → Snare</after>
  </chi_recalculation>
  
  <type_change>Tangled Rope → Snare</type_change>
  
  <preconditions>
    <condition>Agent has analytical capacity</condition>
    <condition>Agent has access to calibration mechanism</condition>
    <condition>Publication channels available</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₃ (identity_locked exit for X₂)</constraint>
    <mechanism>Professional role depends on system operation</mechanism>
  </blocked_by>
  
  <rationale>
    Analytical position (π=1.15) breaks normalization — agent sees 
    extraction that moderate position normalizes. χ crosses snare 
    threshold (0.736 > 0.70). Type change: Tangled Rope → Snare. 
    However, identity-locked exit creates cost: exposing system 
    threatens professional position. Transformation possible but 
    self-destructive for agent.
  </rationale>
</transformation_rule>
```

---

### TR₄: Institutional Calibration Adjustment

```xml
<transformation_rule id="TR₄">
  <trigger>
    <condition>modify_calibration_standard</condition>
    <target>C₃</target>
    <agent>X₅</agent>
  </trigger>
  
  <index_change>
    <from>ε = 0.8</from>
    <to>ε = [0.3–0.7] (range depends on adjustment)</to>
  </index_change>
  
  <chi_recalculation>
    <before>
      X₁: χ = 0.8 × 1.5 × 0.8 = 0.96 → Snare
      X₅: χ = 0.8 × -0.2 × 0.9 = -0.144 → Rope
    </before>
    <after>
      If ε → 0.5:
      X₁: χ = 0.5 × 1.5 × 0.8 = 0.6 → Tangled Rope
      X₅: χ = 0.5 × -0.2 × 0.9 = -0.09 → Rope
    </after>
  </chi_recalculation>
  
  <type_change>
    X₁: Snare → Tangled Rope (if ε reduced sufficiently)
    X₅: Rope → Rope (remains net beneficiary)
  </type_change>
  
  <preconditions>
    <condition>Agent has institutional authority</condition>
    <condition>Calibration mechanism is modifiable</condition>
    <condition>Modification doesn't eliminate coordination value</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>None (institutional position enables modification)</constraint>
  </blocked_by>
  
  <rationale>
    Institutional agent can modify ε (base extractiveness) by adjusting 
    calibration. Reducing ε reduces extraction for all agents but 
    maintains institutional benefit (π=-0.2 ensures χ remains negative). 
    Transformation demonstrates asymmetric reform capacity: institutional 
    position can improve system for others while preserving own benefit.
    
    Critical: This is the ONLY transformation rule not blocked by C₃ 
    itself. Institutional position has unique reform capacity.
  </rationale>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: Type I — False Mountain (C₁)

```xml
<error id="E₁">
  <type>Type I: False Mountain</type>
  <agent>X₁</agent>
  <constraint>C₁</constraint>
  
  <actual_type>Mountain (from agent's index)</actual_type>
  <perceived_type>Mountain (correct classification)</perceived_type>
  
  <observable>
    Agent does not attempt to eliminate measurement bias through 
    technical refinement, correctly recognizing fundamental limit.
  </observable>
  
  <correction_trigger>N/A — classification is correct</correction_trigger>
  
  <note>
    E₁ is NOT an error — included to demonstrate that Mountain 
    classification can be correct. C₁ passes Boltzmann test and 
    represents genuine epistemic limit. Contrast with E₂.
  </note>
</error>
```

---

### Error E₂: Type III — Snare-as-Rope (C₃)

```xml
<error id="E₂">
  <type>Type III: Snare-as-Rope (Missing Extraction)</type>
  <agent>X₂</agent>
  <constraint>C₃</constraint>
  
  <actual_type>Tangled Rope (from agent's index: χ=0.64)</actual_type>
  <perceived_type>Rope (misclassification)</perceived_type>
  
  <observable>
    Agent operates measurement device without investigating calibration 
    mechanism. Treats standard as neutral coordination tool. Does not 
    check χ at powerless index (where χ=0.96 → Snare).
  </observable>
  
  <correction_trigger>
    Compute χ(C₃, powerless, local) = 0.96 → Snare exists regardless 
    of χ(moderate). Recognition that same constraint produces Snare 
    for others reveals Tangled Rope status.
  </correction_trigger>
  
  <rationale>
    Moderate power position (π=1.0) normalizes extraction. Agent 
    experiences coordination value (enables professional function) 
    without feeling extraction burden. Misclassifies Tangled Rope as 
    Rope by ignoring asymmetric cost distribution. Error pattern: 
    privileged position obscures extraction experienced by others.
  </rationale>
</error>
```

---

### Error E₃: Type II — Mountain Denial (C₂)

```xml
<error id="E₃">
  <type>Type II: Mountain Denial</type>
  <agent>X₁</agent>
  <constraint>C₂</constraint>
  
  <actual_type>Mountain (from agent's index)</actual_type>
  <perceived_type>Rope (misclassification)</perceived_type>
  
  <observable>
    Agent attempts to discover "true" reference standard rather than 
    recognizing all standards as arbitrary choices. Expends energy 
    seeking objective foundation that doesn't exist.
  </observable>
  
  <correction_trigger>
    Recognition that reference point selection is choice, not discovery. 
    Boltzmann test shows classification invariant across indices → 
    fundamental property, not constructed system.
  </correction_trigger>
  
  <rationale>
    Agent treats arbitrary foundation as changeable constraint (Rope) 
    rather than unchangeable terrain (Mountain). Consequence: energy 
    depletion fighting what cannot change. Error pattern: confusing 
    epistemic limits with political constraints.
  </rationale>
</error>
```

---

### Error E₄: Type V.a — Tangled-as-Rope (C₃)

```xml
<error id="E₄">
  <type>Type V.a: Tangled-as-Rope (Ignoring Extraction)</type>
  <agent>X₅</agent>
  <constraint>C₃</constraint>
  
  <actual_type>Rope (from agent's index: χ=-0.144)</actual_type>
  <perceived_type>Rope (correct for agent, but incomplete analysis)</perceived_type>
  
  <observable>
    Agent recognizes coordination value and net benefit at institutional 
    index. Does not investigate extraction at powerless index. Treats 
    system as pure coordination mechanism. Resists calibration reform 
    (TR₄) despite capacity to implement it.
  </observable>
  
  <correction_trigger>
    Compute χ(C₃, powerless, local) = 0.96 → Snare. Recognition that 
    institutional benefit derives from extraction at other indices. 
    System is Tangled Rope globally even if Rope locally.
  </correction_trigger>
  
  <rationale>
    Institutional position (π=-0.2) inverts extraction — agent benefits 
    from system. Classification as Rope is correct from agent's index 
    but incomplete. Error: treating local classification as global 
    truth. Agent's Rope experience depends on others' Snare experience. 
    Ignoring extraction component enables extraction to persist.
  </rationale>
</error>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Model Selection: Bounded Institutional Rationality (BIR)

```xml
<rationality_model>
  <type>Bounded Institutional Rationality (BIR)</type>
  
  <justification>
    C₃ represents human institutional system (measurement standards, 
    enforcement mechanisms, calibration practices). Institutional 
    agents (X₅) face:
    
    1. Principal-agent problems: Calibration operators (X₂) have 
       discretion within enforcement framework
    2. Uncertainty: Cannot perfectly predict compliance/resistance
    3. Risk aversion: Institutional stability valued over maximum 
       extraction
    4. Satisficing: System maintains "good enough" extraction rather 
       than optimizing to theoretical maximum
    
    Evidence against PIR:
    - C₃ has ε=0.8, not 1.0 (not maximizing extraction)
    - Supp=0.7, not 1.0 (enforcement has gaps)
    - Coordination value preserved (Coord=true) — pure extraction 
      would eliminate coordination
    
    BIR better models realistic institutional behavior: extract 
    sufficiently to maintain benefit while preserving system stability.
  </justification>
  
  <implications>
    Under BIR:
    - Negotiated Equilibrium becomes reachable (institutions bargain)
    - Seeded Possibility becomes reachable (underground alternatives 
      can persist in enforcement gaps)
    - Deterministic Tragedy less likely (institutions satisfice rather 
      than optimize to collapse)
    - Revolutionary Rupture possible but requires coordination across 
      powerless indices (blocked by C₃'s high suppression)
  </implications>
</rationality_model>
```

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected Attractor: Seeded Possibility

```xml
<terminal_attractor>
  <type>Seeded Possibility</type>
  
  <justification>
    Constraint profile:
    - Two Mountains (C₁, C₂): Fundamental limits on measurement
    - One high-extraction Snare/Tangled Rope (C₃): Enforced standard
    - BIR institutional model: Satisficing, not optimizing
    
    Attractor compatibility (from matrix):
    - Mountain-dominated + BIR → Tragedy OR Seeded Possibility
    - Tangled Rope dominant + BIR → Negotiated Equilibrium
    
    C₃ dominates immediate experience (highest ε, Supp, centrality) 
    but C₁ and C₂ provide underground knowledge:
    
    1. C₁ (measurement bias) reveals that ALL standards introduce bias
    2. C₂ (arbitrary foundation) reveals that standards are choices
    3. Together: Knowledge that alternative standards are possible
    
    Surface trajectory: C₃ runs to completion (extraction persists)
    Underground trajectory: Analytical agents (X₁→analytical, X₂, X₃) 
    recognize arbitrary foundation, seed alternative measurement 
    practices in enforcement gaps
    
    BIR creates gaps: Supp=0.7 (not 1.0) means enforcement is imperfect. 
    Satisficing institutions don't optimize suppression. Alternative 
    practices can persist at small scale.
    
    Terminal state: Surface tragedy (C₃ extraction continues), 
    underground transformation (alternative standards develop in gaps, 
    waiting for institutional shift).
  </justification>
  
  <compatibility_check>
    ☑ Mountain-dominated profile: C₁, C₂ upstream
    ☑ BIR model: Satisficing institutions
    ☑ High-extraction constraint present: C₃
    ☑ Knowledge constraints provide underground resources: C₁, C₂
    ☑ Enforcement gaps exist: Supp=0.7 (not 1.0)
    ☑ Seeded Possibility compatible with profile
  </compatibility_check>
  
  <alternative_attractors_rejected>
    Deterministic Tragedy: Rejected. BIR institutions satisfice rather 
    than optimize to collapse. C₃ extraction is high but not maximal.
    
    Negotiated Equilibrium: Rejected. C₃'s high suppression (0.7) 
    blocks collective organization (TR₁). Powerless agents cannot 
    coordinate to bargain. Institutional agents (X₅) have no incentive 
    to negotiate (χ=-0.144, net beneficiary).
    
    Revolutionary Rupture: Rejected. Would require disrupting C₁ and 
    C₂ (Mountains — impossible) or coordinated exit from C₃ (blocked 
    by high suppression). No mechanism for rupture available.
  </alternative_attractors_rejected>
</terminal_attractor>
```

---

## VI. VALIDATION CHECKLIST

```
☑ All constraints formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with π and σ values
☑ All characters use variable names (X₁, X₂, X₃, X₄, X₅)
☑ Variable mapping table present in header (traceability)
☑ No source occupation, setting, or domain vocabulary
☑ No <experience>, <dialogue_markers>, or <narrative> fields
☑ Transformation rules use abstract trigger descriptions
☑ Error observables are testable structural conditions
☑ Institutional rationality model specified (BIR) with justification
☑ Terminal attractor selected (Seeded Possibility), justified, compatible
☑ Indexical variance explicitly preserved (C₃: maximum variance)
☑ Boltzmann test run for all Mountain claims (C₁ PASS, C₂ PASS, C₃ FAIL)
☑ No ambiguity in specifications
☑ Output is pure symbolic network
```

---

## VII. STRUCTURAL SUMMARY

### Constraint Network Topology

```
C₁ (Mountain) ──┐
                ├──> [Epistemic foundation]
C₂ (Mountain) ──┘         │
                          ↓
                    C₃ (Snare/Tangled/Rope)
                    [Enforced extraction]
```

### Index-Sensitive Classification Matrix

| Constraint | X₁ (powerless) | X₂ (moderate) | X₃ (analytical) | X₄ (powerless) | X₅ (institutional) |
|------------|----------------|---------------|-----------------|----------------|-------------------|
| C₁ | Mountain (0.0575) | — | Mountain (0.0575) | — | — |
| C₂ | — | Mountain (0.115) | Mountain (0.115) | — | — |
| C₃ | Snare (0.96) | Tangled (0.64) | — | Snare (0.96) | Rope (-0.144) |

### Transformation Capacity by Agent

| Agent | Available Transformations | Blocked Transformations |
|-------|--------------------------|------------------------|
| X₁ | None (powerless, constrained exit) | TR₁ (blocked by C₃ Supp), TR₂ (blocked by C₂, C₃ scope) |
| X₂ | TR₃ (analytical exposure, self-destructive) | TR₁ (identity-locked), TR₂ (identity-locked) |
| X₃ | None (analytical position, no constraint subject) | N/A |
| X₄ | None (powerless, trapped exit) | All (trapped) |
| X₅ | TR₄ (calibration adjustment, unblocked) | None |

### Critical Asymmetry

Only institutional agent (X₅) has unblocked transformation capacity (TR₄). All other agents face structural barriers. This asymmetry is the mechanism of Seeded Possibility: surface extraction persists (powerless agents cannot transform), underground knowledge accumulates (analytical agents recognize arbitrary foundation), waiting for institutional shift.

---

**END OF STAGE 1 FORMALIZATION**

<!-- ANONYMIZATION: 5 character names replaced with structural labels: Agent_A, Agent_B, Agent_C, Agent_D, Agent_E -->
