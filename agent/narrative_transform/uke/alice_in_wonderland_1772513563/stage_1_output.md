# STAGE 1: FORMAL CONSTRAINT SPECIFICATION

## Variable Mapping (Header — Traceability Only)

```
X₁ ← Alice
X₂ ← Cheshire Cat
X₃ ← The Dodo
X₄ ← The Hatter
X₅ ← The Queen of Hearts
X₆ ← The Gardeners

C₁ ← Fluid Reality
C₂ ← Insular Nonsense
C₃ ← Capricious Condemnation
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Environmental Instability

```xml
<constraint id="C₁">
  <properties>
    <epsilon>0.20</epsilon>
    <suppression>0.00</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.20 × 1.5 × 0.8 = 0.24</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.20 ≤ 0.25 (mountain_extractiveness_max): PASS
      Supp = 0.00 ≤ 0.05 (mountain_suppression_ceiling): PASS
      NaturalEmergence: PASS (environmental property)
      Immutable(immediate, trapped): PASS
    </threshold_check>
    <rationale>
      Zero suppression indicates no enforcement mechanism. Low base extraction 
      with natural emergence pattern. From immediate time horizon with trapped 
      exit, appears as unchangeable environmental property. Classification does 
      not depend on χ for Mountains — natural laws affect all positions equally.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>arbitrage</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.20 × 1.15 × 0.8 = 0.184</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.20 ≤ 0.25: PASS
      Supp = 0.00 ≤ 0.05: PASS
      NaturalEmergence: PASS
      Immutable(biographical, arbitrage): PASS (can exit system but cannot change property)
    </threshold_check>
    <rationale>
      Analytical position with arbitrage exit can observe from outside but cannot 
      modify the environmental property. Biographical horizon still insufficient 
      to change fundamental terrain. Classification identical to X₁ despite different 
      power position — confirms natural law pattern.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=0.24 → Mountain, X₂ χ=0.184 → Mountain
    Zero variance. Both agents classify identically despite different power positions 
    and exit options. This is the defining signature of a natural constraint — 
    classification is independent of structural position.
  </indexical_variance>

  <boltzmann_test>
    PASS
    
    Test matrix:
    I₁ = (powerless, immediate, trapped, local): Type = Mountain
    I₂ = (analytical, biographical, arbitrage, local): Type = Mountain
    I₃ = (moderate, immediate, constrained, local): Type = Mountain
    I₄ = (powerful, biographical, mobile, regional): Type = Mountain
    
    Classification invariant across Power dimension: ✓
    Classification invariant across Scope dimension: ✓
    
    Conclusion: Constraint exhibits Boltzmann independence. Classification does 
    not vary with power position or scope. Certified as natural environmental 
    property, not constructed system.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₂: Exclusionary Coordination System

```xml
<constraint id="C₂">
  <properties>
    <epsilon>0.50</epsilon>
    <suppression>0.40</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>constrained</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.50 × 1.5 × 0.8 = 0.60</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ = 0.60 ≤ 0.70 (tangled_chi_range): PASS
      Coord = true: PASS
      Asym = true: PASS
    </threshold_check>
    <rationale>
      Moderate power-scaled extraction in hybrid range. System provides genuine 
      coordination value for insiders while extracting from outsiders. Asymmetric 
      cost distribution confirmed by Asym flag. From powerless position with 
      constrained exit, experiences both coordination function (can observe insiders 
      using it successfully) and extraction burden (cannot participate effectively).
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>powerful</power>             <!-- π = 0.6 -->
      <time>immediate</time>
      <exit>mobile</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.50 × 0.6 × 0.8 = 0.24</chi>
    <type>Rope</type>
    <threshold_check>
      χ = 0.24 ≤ 0.35 (rope_chi_ceiling): PASS
      ε = 0.50 > 0.45 (rope_epsilon_ceiling): FAIL
      
      Override: χ ≤ 0 → bypass ε ceiling: NO (χ = 0.24 > 0)
      
      CLASSIFICATION CONFLICT DETECTED
      Action: Check base extraction at powerless index
      χ(powerless) = 0.60 → Tangled Rope
      
      Resolution: Constraint has moderate base extraction. Powerful position 
      dampens to Rope-level χ but does not eliminate extraction for others. 
      Classify as Rope from this index with naturalization flag.
    </threshold_check>
    <rationale>
      Power position dampens extraction below Rope ceiling. However, ε = 0.50 
      exceeds rope_epsilon_ceiling, indicating extraction exists at other indices. 
      From powerful insider position, system functions as coordination mechanism. 
      Mobile exit options and insider status eliminate extraction burden. 
      Classification as Rope is valid from this index but does not represent 
      constraint's full structure.
    </rationale>
    <naturalization_flag>
      ε = 0.50 > 0.45 but χ = 0.24 < 0.40
      Power position is absorbing/hiding extraction that exists at other indices.
      Investigate: Does agent recognize extraction burden on X₁?
    </naturalization_flag>
  </agent>

  <agent ref="X₄">
    <index>
      <power>moderate</power>             <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.50 × 1.0 × 0.8 = 0.40</chi>
    <type>Rope</type>
    <threshold_check>
      χ = 0.40 > 0.35 (rope_chi_ceiling): FAIL by 0.05
      
      Boundary case analysis:
      - Coord = true (provides coordination value)
      - identity_locked exit (cannot leave but not trapped)
      - biographical horizon (can imagine change but not within immediate frame)
      - χ = 0.40 just above Rope ceiling
      
      Classification: Rope (boundary case)
      Rationale: Minimal extraction above threshold, strong coordination value, 
      identity fusion normalizes cost. From this index, system functions primarily 
      as coordination mechanism with tolerable extraction.
    </threshold_check>
    <rationale>
      Moderate power with identity_locked exit creates normalization effect. 
      Agent cannot exit (identity fusion) but also does not experience powerless 
      amplification. Biographical horizon allows recognition that system could 
      change but not within actionable timeframe. χ = 0.40 is boundary case — 
      just above Rope ceiling but below Tangled floor. Coordination value and 
      identity fusion tip classification to Rope from this index.
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₁ χ=0.60 → Tangled Rope, X₃ χ=0.24 → Rope, X₄ χ=0.40 → Rope
    
    Significant variance. Same constraint presents as:
    - Tangled Rope (hybrid extraction-coordination) from powerless outsider position
    - Rope (pure coordination) from powerful insider position
    - Rope (normalized coordination) from moderate identity-locked position
    
    Structural pattern: Exclusionary coordination system. Insiders experience 
    coordination value with dampened extraction. Outsiders experience extraction 
    burden while observing coordination function they cannot access. This is 
    irreducible indexical variance — all three classifications are objectively 
    true from their respective positions.
  </indexical_variance>

  <boltzmann_test>
    FAIL
    
    Test matrix:
    I₁ = (powerless, immediate, constrained, local): Type = Tangled Rope
    I₂ = (powerful, immediate, mobile, local): Type = Rope
    I₃ = (moderate, biographical, identity_locked, local): Type = Rope
    
    Classification varies with Power dimension: ✗
    
    Conclusion: Constraint is constructed, not natural. Classification depends 
    on power position — insiders vs. outsiders experience different constraint 
    types. This is a social coordination system with asymmetric access, not an 
    environmental property.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₃: Arbitrary Enforcement System

```xml
<constraint id="C₃">
  <properties>
    <epsilon>0.90</epsilon>
    <suppression>0.90</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.5 × 0.8 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 1.08 > 0.70 (snare_chi_threshold): PASS
      ¬LowBaseExtraction: PASS (ε = 0.90)
    </threshold_check>
    <rationale>
      High base extraction amplified by powerless position. Trapped exit with 
      immediate horizon creates maximum vulnerability. High suppression (0.90) 
      indicates active enforcement required to maintain system. From this index, 
      constraint is pure extraction trap — any action can trigger enforcement, 
      no reliable strategy for compliance exists.
    </rationale>
  </agent>

  <agent ref="X₅">
    <index>
      <power>institutional</power>        <!-- π = -0.2 -->
      <time>immediate</time>
      <exit>arbitrage</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × (-0.2) × 0.8 = -0.144</chi>
    <type>Rope</type>
    <threshold_check>
      χ = -0.144 ≤ 0 (net beneficiary): PASS
      Bypass ε ceiling (χ ≤ 0 clause): PASS
      Coord = true: PASS (coordinates subject behavior through threat)
    </threshold_check>
    <rationale>
      Institutional position inverts extraction — agent is net beneficiary of 
      the constraint. Negative χ indicates extraction flows TO this position 
      FROM subjects. Arbitrage exit allows selective enforcement (can choose 
      when to apply rules). From this index, system is pure coordination 
      mechanism — subjects' fear coordinates their behavior predictably.
    </rationale>
  </agent>

  <agent ref="X₆">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.5 × 0.8 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 1.08 > 0.70: PASS
      ¬LowBaseExtraction: PASS
    </threshold_check>
    <rationale>
      Identical index to X₁, identical classification. Multiple agents at same 
      structural position experience same constraint type. High extraction with 
      trapped exit creates lethal vulnerability — enforcement is arbitrary but 
      consequences are severe.
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₁ χ=1.08 → Snare, X₅ χ=-0.144 → Rope, X₆ χ=1.08 → Snare
    
    Extreme variance. Same constraint presents as:
    - Snare (lethal extraction trap) from powerless subject positions
    - Rope (coordination mechanism) from institutional enforcer position
    
    Structural pattern: Pure asymmetric enforcement system. Enforcer experiences 
    negative χ (net beneficiary) while subjects experience χ > 1.0 (extreme 
    extraction). This is maximum indexical divergence — constraint type inverts 
    completely between enforcer and subject positions.
    
    Critical observation: X₁ and X₆ at identical indices produce identical 
    classifications, confirming that indexical variance is structural, not 
    agent-specific.
  </indexical_variance>

  <boltzmann_test>
    FAIL
    
    Test matrix:
    I₁ = (powerless, immediate, trapped, local): Type = Snare
    I₂ = (institutional, immediate, arbitrage, local): Type = Rope
    I₃ = (powerless, immediate, trapped, local): Type = Snare
    
    Classification varies with Power dimension: ✗
    Classification inverts between enforcer and subject: ✗✗
    
    Conclusion: Constraint is constructed enforcement system with extreme 
    asymmetry. Classification depends entirely on power position — enforcer 
    vs. subject determines whether constraint is Rope or Snare. This is a 
    pure extraction mechanism disguised as coordination for the enforcer.
  </boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Collective Organization Attempt

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>organize_collective_resistance</condition>
    <target>C₂</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>P = powerless (π = 1.5)</from>
    <to>P = organized (π = 0.4)</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.50 × 1.5 × 0.8 = 0.60 → Tangled Rope</before>
    <after>χ = 0.50 × 0.4 × 0.8 = 0.16 → Rope</after>
  </chi_recalculation>
  
  <type_change>Tangled Rope → Rope</type_change>
  
  <preconditions>
    <condition>Multiple agents at powerless index must coordinate</condition>
    <condition>Communication channels must exist</condition>
    <condition>Shared recognition of extraction burden</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₁ (environmental instability prevents reliable coordination)</constraint>
    <constraint>C₂ itself (exclusionary logic makes collective formation impossible)</constraint>
    <constraint>C₃ (arbitrary enforcement creates fear of organization)</constraint>
  </blocked_by>
  
  <structural_note>
    Transformation requires overcoming C₂'s own exclusionary logic. The constraint 
    that would be transformed by collective action is the same constraint that 
    prevents collective formation. This creates a stable trap — transformation 
    is theoretically possible but practically blocked by the constraint's own 
    structure.
  </structural_note>
</transformation_rule>
```

---

### TR₂: Exit Attempt

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>attempt_exit_from_system</condition>
    <target>C₂, C₃</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>E = constrained (C₂), E = trapped (C₃)</from>
    <to>E = mobile (if successful)</to>
  </index_change>
  
  <chi_recalculation>
    <C₂>
      <before>χ = 0.50 × 1.5 × 0.8 = 0.60 → Tangled Rope</before>
      <after>χ = 0.50 × 1.5 × 0.8 = 0.60 → Tangled Rope (no change — exit option doesn't affect π or σ)</after>
    </C₂>
    <C₃>
      <before>χ = 0.90 × 1.5 × 0.8 = 1.08 → Snare</before>
      <after>χ = 0.90 × 1.5 × 0.8 = 1.08 → Snare (no change)</after>
    </C₃>
  </chi_recalculation>
  
  <type_change>None (exit changes E dimension but not χ calculation)</type_change>
  
  <preconditions>
    <condition>Alternative system must exist outside current constraint network</condition>
    <condition>Agent must have resources to reach alternative</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₁ (environmental instability makes "outside" undefined)</constraint>
    <constraint>C₃ (arbitrary enforcement can trigger during exit attempt)</constraint>
  </blocked_by>
  
  <structural_note>
    Exit attempt does not change χ because E dimension affects immutability 
    assessment and error patterns, not power-scaled extraction. However, 
    successful exit removes agent from constraint network entirely. The 
    transformation is binary: either exit succeeds (agent leaves system) or 
    fails (agent remains at same index).
  </structural_note>
</transformation_rule>
```

---

### TR₃: Enforcement Escalation

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>increase_enforcement_intensity</condition>
    <target>C₃</target>
    <agent>X₅</agent>
  </trigger>
  
  <index_change>
    <from>None (institutional position unchanged)</from>
    <to>None</to>
  </index_change>
  
  <chi_recalculation>
    <X₅>
      <before>χ = 0.90 × (-0.2) × 0.8 = -0.144 → Rope</before>
      <after>χ = 0.95 × (-0.2) × 0.8 = -0.152 → Rope (increased benefit)</after>
    </X₅>
    <X₁>
      <before>χ = 0.90 × 1.5 × 0.8 = 1.08 → Snare</before>
      <after>χ = 0.95 × 1.5 × 0.8 = 1.14 → Snare (increased extraction)</after>
    </X₁>
  </chi_recalculation>
  
  <type_change>None (both remain in same type, extraction intensifies)</type_change>
  
  <preconditions>
    <condition>Institutional position must have enforcement capacity</condition>
    <condition>Subject resistance or non-compliance must exist</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>None (institutional position has unilateral enforcement capacity)</constraint>
  </blocked_by>
  
  <structural_note>
    Escalation increases ε for all agents but affects χ asymmetrically. Enforcer's 
    negative χ becomes more negative (increased benefit). Subjects' positive χ 
    increases (increased extraction). This is a positive feedback loop — resistance 
    triggers escalation, escalation increases extraction, increased extraction 
    may trigger more resistance.
  </structural_note>
</transformation_rule>
```

---

### TR₄: Time Horizon Extension

```xml
<transformation_rule id="TR₄">
  <trigger>
    <condition>extend_planning_horizon</condition>
    <target>C₁</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>T = immediate</from>
    <to>T = biographical</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.20 × 1.5 × 0.8 = 0.24 → Mountain</before>
    <after>χ = 0.20 × 1.5 × 0.8 = 0.24 → Mountain (no change — T doesn't affect χ)</after>
  </chi_recalculation>
  
  <type_change>Mountain → Mountain (but immutability assessment changes)</type_change>
  
  <preconditions>
    <condition>Agent must gain information about constraint's constructed nature</condition>
    <condition>Agent must have cognitive capacity to imagine change</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₁ itself (environmental instability prevents stable long-term planning)</constraint>
  </blocked_by>
  
  <structural_note>
    Time horizon extension does not change χ but changes immutability assessment. 
    From biographical horizon, C₁ might appear changeable (if recognized as 
    constructed). However, C₁'s environmental instability actively prevents 
    biographical planning — the constraint blocks its own reclassification. 
    This creates Error Type I (False Mountain) that is self-reinforcing.
  </structural_note>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: False Mountain (Type I)

```xml
<error id="E₁">
  <type>Type I — False Mountain</type>
  <agent>X₁</agent>
  <constraint>C₁</constraint>
  
  <actual_type>Mountain (from immediate/trapped index)</actual_type>
  <perceived_type>Mountain (correct classification from this index)</perceived_type>
  
  <error_pattern>
    Not a misclassification from agent's index, but a structural trap. C₁
    appears as Mountain from (immediate, trapped) index. However, if C₁ is 
    actually constructed (dream boundary), then classification would change 
    at (biographical, mobile) index. Agent cannot access that index while 
    inside the constraint.
  </error_pattern>
  
  <observable>
    Agent does not attempt to investigate constraint's constructed nature.
    Agent does not seek information about system boundaries.
    Agent treats environmental instability as unchangeable fact.
  </observable>
  
  <correction_trigger>
    Information revealing C₁'s constructed nature (dream boundary).
    Access to exit option that changes E from trapped to mobile.
    Time horizon extension that changes T from immediate to biographical.
  </correction_trigger>
  
  <structural_note>
    This is a Boltzmann-ambiguous case. If C₁ is truly natural (environmental 
    physics), then Mountain classification is correct at all indices. If C₁ 
    is constructed (dream logic), then it only appears as Mountain from 
    (immediate, trapped) index. The error is not in the classification but 
    in the inability to test which case applies.
  </structural_note>
</error>
```

---

### Error E₂: Snare-as-Rope (Type III)

```xml
<error id="E₂">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X₃</agent>
  <constraint>C₂</constraint>
  
  <actual_type>Rope (from powerful/mobile index)</actual_type>
  <perceived_type>Rope (correct from this index, but incomplete)</perceived_type>
  
  <error_pattern>
    Agent correctly classifies C₂ as Rope from (powerful, mobile) index where 
    χ = 0.24. However, agent fails to recognize that same constraint is Tangled 
    Rope from (powerless, constrained) index where χ = 0.60. This is not a 
    misclassification but a failure to recognize indexical variance.
  </error_pattern>
  
  <observable>
    Agent treats coordination system as universally beneficial.
    Agent does not recognize extraction burden on outsiders.
    Agent resists reforms that would reduce asymmetry.
    Agent interprets outsider complaints as incompetence rather than structural extraction.
  </observable>
  
  <correction_trigger>
    Forced perspective-taking from powerless index.
    Measurement of χ at multiple power positions.
    Recognition that ε = 0.50 exceeds rope_epsilon_ceiling.
  </correction_trigger>
  
  <structural_note>
    This is naturalization in action. Agent's classification is correct from 
    their index, but they generalize it to all indices. The naturalization_flag 
    on X₃'s classification indicates this risk. Error is not in the math but 
    in the scope claim — "this is a Rope" (correct) vs. "this is a Rope for 
    everyone" (incorrect).
  </structural_note>
</error>
```

---

### Error E₃: Rope-as-Snare (Type IV)

```xml
<error id="E₃">
  <type>Type IV — Rope-as-Snare (Missing Coordination)</type>
  <agent>X₁</agent>
  <constraint>C₂</constraint>
  
  <actual_type>Tangled Rope (from powerless/constrained index)</actual_type>
  <perceived_type>Snare (misclassification — missing coordination component)</perceived_type>
  
  <error_pattern>
    Agent experiences χ = 0.60 (Tangled Rope range) but perceives constraint 
    as pure extraction (Snare). Fails to recognize that Coord = true — system 
    provides genuine coordination value for insiders. Misclassification leads 
    to destructive reform attempts that would eliminate coordination function.
  </error_pattern>
  
  <observable>
    Agent attempts to destroy C₂ entirely rather than reform asymmetry.
    Agent does not distinguish between coordination and extraction components.
    Agent rejects insider testimony about coordination value.
    Agent proposes reforms that would eliminate functional coordination.
  </observable>
  
  <correction_trigger>
    Recognition that insiders successfully coordinate using C₂.
    Measurement showing Coord = true despite high extraction.
    Understanding that χ = 0.60 is Tangled Rope, not Snare (χ > 0.70).
  </correction_trigger>
  
  <structural_note>
    This error is symmetric to E₂. Where E₂ misses extraction by generalizing 
    from powerful index, E₃ misses coordination by generalizing from powerless 
    index. Both errors stem from treating indexed classification as universal 
    truth. Correct analysis requires recognizing irreducible indexical variance.
  </structural_note>
</error>
```

---

### Error E₄: Tangled Rope Mishandling (Type V.c)

```xml
<error id="E₄">
  <type>Type V.c — Wrong Reform Strategy</type>
  <agent>X₁</agent>
  <constraint>C₂</constraint>
  
  <actual_type>Tangled Rope</actual_type>
  <perceived_type>Tangled Rope (correct classification)</perceived_type>
  
  <error_pattern>
    Agent correctly identifies C₂ as Tangled Rope but attempts surgical reform 
    (separating coordination from extraction) when constraint's purity is too 
    low. Supp = 0.40 indicates moderate enforcement requirement. Attempting to 
    preserve coordination while eliminating extraction may be structurally 
    impossible if the two are mechanically coupled.
  </error_pattern>
  
  <observable>
    Agent attempts to reform C₂ by reducing ε while preserving Coord.
    Agent does not assess whether coordination depends on extraction.
    Agent does not consider full replacement (Scaffold construction).
    Reform attempts fail repeatedly but strategy does not change.
  </observable>
  
  <correction_trigger>
    Purity assessment showing coordination-extraction coupling.
    Recognition that surgical reform is blocked by constraint structure.
    Shift to Scaffold construction strategy (build alternative, sunset original).
  </correction_trigger>
  
  <structural_note>
    This is the most sophisticated error type. Agent has correct classification 
    and recognizes both coordination and extraction components. Error is in 
    reform strategy — attempting to separate coupled components rather than 
    replacing entire constraint. Requires purity analysis to determine if 
    surgical reform is feasible.
  </structural_note>
</error>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Model Selection: Bounded Institutional Rationality (BIR)

```xml
<rationality_model>
  <type>BIR</type>
  
  <justification>
    C₃ (arbitrary enforcement) exhibits satisficing behavior rather than 
    utility maximization. X₅ does not execute all threats — enforcement is 
    selective and performative. This indicates:
    
    1. Principal-agent problem: X₅ may not have perfect information about 
       all violations or perfect enforcement capacity.
    
    2. Risk aversion: Executing all threats would destabilize system. X₅ 
       maintains threat credibility through selective enforcement.
    
    3. Bounded optimization: X₅ satisfices (maintains order through fear) 
       rather than maximizes (eliminates all violations).
    
    C₂ (exclusionary coordination) also shows BIR pattern:
    - Insiders tolerate some outsider presence (not perfect exclusion)
    - System maintains stability through partial coordination
    - No evidence of utility maximization, only satisficing equilibrium
    
    PIR would predict:
    - C₃: All threats executed → system collapse (unsustainable)
    - C₂: Perfect exclusion or perfect inclusion → observed hybrid impossible
    
    BIR predicts: