# STAGE 1: FORMAL CONSTRAINT NETWORK SPECIFICATION

## Variable Mapping (Header — Stripped During Anonymization)

```
X₁ ← Santiago (the old man)
X₂ ← Manolin (the boy)
X₃ ← Other fishermen (collective)
G₁ ← Fishing community
I₁ ← Social enforcement system (reputation, mockery)
C₁ ← Individual subsistence extraction constraint
C₂ ← Social proof/reputation constraint
C₃ ← Mentorship bond constraint
C₄ ← Age-based capability decline (biological)
C₅ ← Resource depletion constraint
C₆ ← Collective action impossibility
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Individual Subsistence Extraction

```xml
<constraint id="C₁">
  <properties>
    <epsilon>0.85</epsilon>
    <suppression>0.70</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>              <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>                  <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.85 × 1.5 × 0.8 = 1.02</chi>
    <type>Snare</type>
    <threshold_check>snare_chi_threshold (0.70): PASS (1.02 > 0.70)</threshold_check>
    <rationale>
      High base extraction (ε=0.85) amplified by powerless position. 
      Individual must extract from resource base to survive, but extraction 
      exceeds sustainable yield. High suppression through economic necessity 
      (cannot stop without starvation). No coordination value — purely 
      extractive relationship. Power amplification pushes χ above Snare threshold.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>moderate</power>               <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>mobile</exit>
      <scope>local</scope>                  <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.85 × 1.0 × 0.8 = 0.68</chi>
    <type>Tangled_Rope</type>
    <threshold_check>tangled_chi_floor (0.46): PASS (0.68 ≥ 0.46); snare_chi_threshold (0.70): FAIL (0.68 ≤ 0.70)</threshold_check>
    <rationale>
      Same base extraction, but moderate power position (alternative employment 
      options, family support) dampens effective extraction. Falls into Tangled 
      Rope range — extraction present but not overwhelming. Exit mobility 
      provides structural buffer absent for X₁.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>moderate</power>               <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>local</scope>                  <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.85 × 1.0 × 0.8 = 0.68</chi>
    <type>Tangled_Rope</type>
    <threshold_check>tangled_chi_floor (0.46): PASS; snare_chi_threshold (0.70): FAIL</threshold_check>
    <rationale>
      Collective experiences same moderate extraction as X₂. Exit constrained 
      (high switching costs) but not trapped. Tangled Rope classification 
      reflects genuine subsistence need mixed with extractive pressure.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=1.02 → Snare, X₂ χ=0.68 → Tangled_Rope, X₃ χ=0.68 → Tangled_Rope
    
    Power position determines whether extraction is overwhelming (Snare) or 
    manageable (Tangled). Same constraint structure produces different 
    experienced types. X₁ trapped in pure extraction; X₂/X₃ experience 
    extraction but retain agency margin.
  </indexical_variance>

  <boltzmann_test>FAIL — Classification varies by power position (powerless vs. moderate). 
  Burden distribution is non-uniform across indices. This is a constructed 
  economic system, not natural law.</boltzmann_test>
</constraint>
```

---

### Constraint C₂: Social Proof/Reputation System

```xml
<constraint id="C₂">
  <properties>
    <epsilon>0.55</epsilon>
    <suppression>0.80</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>              <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                  <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.55 × 1.5 × 0.8 = 0.66</chi>
    <type>Tangled_Rope</type>
    <threshold_check>tangled_chi_floor (0.46): PASS (0.66 ≥ 0.46); snare_chi_threshold (0.70): FAIL (0.66 ≤ 0.70)</threshold_check>
    <rationale>
      Social enforcement mechanism with genuine coordination function (signals 
      competence, enables trust) but asymmetric cost distribution. High 
      suppression through mockery and exclusion. Identity-locked exit (self-concept 
      fused to role) amplifies extraction. Coordination value prevents Snare 
      classification despite high χ.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>moderate</power>               <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>mobile</exit>
      <scope>local</scope>                  <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.55 × 1.0 × 0.8 = 0.44</chi>
    <type>Rope</type>
    <threshold_check>rope_chi_ceiling (0.35): FAIL (0.44 > 0.35); rope_epsilon_ceiling (0.45): PASS (0.55 > 0.45 BUT χ > 0.35 triggers dual threshold); RESULT: Tangled_Rope by narrow margin</threshold_check>
    <rationale>
      Moderate power and exit mobility dampen extraction. Coordination value 
      (reputation signals) remains. χ=0.44 just above Rope ceiling (0.35), 
      and ε=0.55 exceeds rope_epsilon_ceiling (0.45). Dual threshold violation 
      → Tangled_Rope, though barely. X₂ experiences reputation system as 
      mostly functional with minor extraction.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>moderate</power>               <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>local</scope>                  <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.55 × 1.0 × 0.8 = 0.44</chi>
    <type>Tangled_Rope</type>
    <threshold_check>Same as X₂</threshold_check>
    <rationale>
      Collective experiences reputation system as coordination mechanism with 
      extraction component. Constrained exit (switching costs) but not trapped. 
      Tangled Rope reflects hybrid nature.
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₁ χ=0.66 → Tangled_Rope, X₂ χ=0.44 → Tangled_Rope (borderline Rope), X₃ χ=0.44 → Tangled_Rope
    
    All agents classify as Tangled Rope, but X₁ experiences higher extraction 
    (identity-locked amplification). X₂/X₃ closer to Rope boundary. Coordination 
    function preserved across indices, but burden varies.
  </indexical_variance>

  <boltzmann_test>FAIL — Classification stable (all Tangled Rope) but χ varies 
  by power position. Burden distribution non-uniform. Social enforcement system 
  is constructed, not natural.</boltzmann_test>
</constraint>
```

---

### Constraint C₃: Mentorship Bond

```xml
<constraint id="C₃">
  <properties>
    <epsilon>0.25</epsilon>
    <suppression>0.10</suppression>
    <coordination>true</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>moderate</power>               <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                  <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.25 × 1.0 × 0.8 = 0.20</chi>
    <type>Rope</type>
    <threshold_check>rope_chi_ceiling (0.35): PASS (0.20 ≤ 0.35); rope_epsilon_ceiling (0.45): PASS (0.25 ≤ 0.45)</threshold_check>
    <rationale>
      Low extraction, low suppression, genuine coordination (knowledge transfer, 
      emotional support). Symmetric benefit distribution. Identity-locked exit 
      reflects deep attachment, not coercion. Pure Rope — functional relationship 
      with minimal extraction.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>moderate</power>               <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                  <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.25 × 1.0 × 0.8 = 0.20</chi>
    <type>Rope</type>
    <threshold_check>rope_chi_ceiling (0.35): PASS; rope_epsilon_ceiling (0.45): PASS</threshold_check>
    <rationale>
      Symmetric to X₁. Low extraction, genuine coordination. Identity-locked 
      reflects attachment, not trap. Rope classification stable across both agents.
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₁ χ=0.20 → Rope, X₂ χ=0.20 → Rope
    
    No variance — both agents experience identical low extraction and coordination 
    value. Symmetric relationship produces identical indexed classifications.
  </indexical_variance>

  <boltzmann_test>PASS (candidate) — Classification invariant across power positions. 
  However, constraint is constructed (human relationship), not naturally emergent. 
  Low ε and Supp suggest well-designed coordination, not natural law. Boltzmann 
  independence alone insufficient for Mountain status — must also satisfy 
  NaturalEmergence predicate, which C₃ fails.</boltzmann_test>
</constraint>
```

---

### Constraint C₄: Age-Based Capability Decline

```xml
<constraint id="C₄">
  <properties>
    <epsilon>0.15</epsilon>
    <suppression>0.00</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>              <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>universal</scope>              <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.15 × 1.5 × 1.0 = 0.225</chi>
    <type>Mountain</type>
    <threshold_check>mountain_extractiveness_max (0.25): PASS (0.15 ≤ 0.25); mountain_suppression_ceiling (0.05): PASS (0.00 ≤ 0.05)</threshold_check>
    <rationale>
      Biological aging is naturally emergent, immutable from biographical time 
      horizon, affects all agents uniformly. Zero suppression (no enforcement 
      needed). Minimal extraction (natural process, not constructed burden). 
      Universal scope (σ=1.0) — natural laws are scope-invariant.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>moderate</power>               <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>universal</scope>              <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.15 × 1.0 × 1.0 = 0.15</chi>
    <type>Mountain</type>
    <threshold_check>mountain_extractiveness_max (0.25): PASS; mountain_suppression_ceiling (0.05): PASS</threshold_check>
    <rationale>
      Same biological process. Power position irrelevant — aging affects everyone 
      equally. Mountain classification stable.
    </rationale>
  </agent>

  <indexical_variance>
    C₄: X₁ χ=0.225 → Mountain, X₂ χ=0.15 → Mountain
    
    χ varies slightly due to power modifier, but classification stable (both Mountain). 
    This is acceptable variance — natural laws can impose different absolute burdens 
    (gravity affects massive objects more) while remaining natural. Key test: 
    does classification change? No.
  </indexical_variance>

  <boltzmann_test>PASS — Classification invariant across power positions and scope. 
  Naturally emergent (biological process). Immutable from biographical horizon. 
  Zero suppression. Satisfies all Mountain criteria.</boltzmann_test>
</constraint>
```

---

### Constraint C₅: Resource Depletion

```xml
<constraint id="C₅">
  <properties>
    <epsilon>0.20</epsilon>
    <suppression>0.05</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>              <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>regional</scope>               <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.20 × 1.5 × 0.9 = 0.27</chi>
    <type>Mountain</type>
    <threshold_check>mountain_extractiveness_max (0.25): FAIL (0.27 > 0.25); mountain_suppression_ceiling (0.05): PASS</threshold_check>
    <rationale>
      Resource depletion appears as Mountain from powerless index — unchangeable 
      within biographical horizon, naturally emergent from extraction dynamics. 
      However, χ=0.27 exceeds mountain_extractiveness_max (0.25) by narrow margin. 
      Borderline case: is this natural scarcity or constructed tragedy of commons?
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>moderate</power>               <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>mobile</exit>
      <scope>regional</scope>               <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.20 × 1.0 × 0.9 = 0.18</chi>
    <type>Mountain</type>
    <threshold_check>mountain_extractiveness_max (0.25): PASS (0.18 ≤ 0.25); mountain_suppression_ceiling (0.05): PASS</threshold_check>
    <rationale>
      From moderate index, resource depletion appears as Mountain — natural 
      scarcity, unchangeable within biographical horizon. Exit mobility (can 
      switch locations) dampens extraction.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>organized</power>              <!-- π = 0.4 -->
      <time>generational</time>
      <exit>constrained</exit>
      <scope>regional</scope>               <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.20 × 0.4 × 0.9 = 0.072</chi>
    <type>Rope</type>
    <threshold_check>rope_chi_ceiling (0.35): PASS (0.072 ≤ 0.35); rope_epsilon_ceiling (0.45): PASS (0.20 ≤ 0.45)</threshold_check>
    <rationale>
      From organized collective with generational horizon, resource depletion 
      becomes changeable (sustainable management possible). Organized power 
      (π=0.4) dramatically dampens extraction. Transitions from Mountain to 
      Rope — reveals constructed nature of "tragedy."
    </rationale>
  </agent>

  <indexical_variance>
    C₅: X₁ χ=0.27 → Mountain (borderline), X₂ χ=0.18 → Mountain, X₃ χ=0.072 → Rope
    
    CRITICAL VARIANCE: Classification changes from Mountain (individual indices) 
    to Rope (organized collective). This reveals C₅ is NOT a natural law — it's 
    a coordination failure. Resource depletion appears unchangeable to individuals 
    but becomes manageable through collective action. The "tragedy" is constructed, 
    not natural.
  </indexical_variance>

  <boltzmann_test>FAIL — Classification varies by power position (powerless/moderate 
  → Mountain, organized → Rope). Burden distribution non-uniform. This is a 
  coordination problem (tragedy of commons), not natural scarcity. The appearance 
  of Mountain status from individual indices is a Type I error (False Mountain).</boltzmann_test>
</constraint>
```

---

### Constraint C₆: Collective Action Impossibility

```xml
<constraint id="C₆">
  <properties>
    <epsilon>0.90</epsilon>
    <suppression>0.85</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>              <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>                  <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.5 × 0.8 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>snare_chi_threshold (0.70): PASS (1.08 > 0.70)</threshold_check>
    <rationale>
      Structural barrier to collective organization. High extraction (isolation 
      prevents burden-sharing), high suppression (social enforcement against 
      cooperation). Powerless position amplifies — cannot organize alone. 
      Pure Snare — extraction trap with no coordination value.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>moderate</power>               <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>mobile</exit>
      <scope>local</scope>                  <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.0 × 0.8 = 0.72</chi>
    <type>Snare</type>
    <threshold_check>snare_chi_threshold (0.70): PASS (0.72 > 0.70)</threshold_check>
    <rationale>
      Moderate power dampens extraction slightly, but barrier remains. Exit 
      mobility provides escape option (can leave system), but within system, 
      collective action blocked. Snare classification stable.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>organized</power>              <!-- π = 0.4 -->
      <time>generational</time>
      <exit>constrained</exit>
      <scope>regional</scope>               <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.90 × 0.4 × 0.9 = 0.324</chi>
    <type>Rope</type>
    <threshold_check>rope_chi_ceiling (0.35): PASS (0.324 ≤ 0.35); rope_epsilon_ceiling (0.45): FAIL (0.90 > 0.45) BUT χ ≤ 0.35 allows bypass</threshold_check>
    <rationale>
      From organized index, collective action barrier dissolves — organization 
      itself overcomes the constraint. Dramatic extraction reduction (π=0.4). 
      Regional scope slightly amplifies (σ=0.9), but organized power dominates. 
      Transitions to Rope — coordination becomes possible.
    </rationale>
  </agent>

  <indexical_variance>
    C₆: X₁ χ=1.08 → Snare, X₂ χ=0.72 → Snare, X₃ χ=0.324 → Rope
    
    CRITICAL VARIANCE: Collective action impossibility is Snare from individual 
    indices but Rope from organized index. This is the core structural trap — 
    individuals cannot organize (Snare), but organization would eliminate the 
    constraint (Rope). Classic coordination failure with high activation energy.
  </indexical_variance>

  <boltzmann_test>FAIL — Classification varies dramatically by power position 
  (powerless/moderate → Snare, organized → Rope). This is a constructed barrier, 
  not natural law. The impossibility is structural, not physical.</boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Collective Organization Attempt

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>organize_collective</condition>
    <target>C₆</target>
    <agent>X₁, X₂, X₃</agent>
  </trigger>
  
  <index_change>
    <from>P = powerless (π=1.5), P = moderate (π=1.0)</from>
    <to>P = organized (π=0.4)</to>
  </index_change>
  
  <chi_recalculation>
    <constraint>C₁</constraint>
    <before>χ(X₁) = 0.85 × 1.5 × 0.8 = 1.02 → Snare</before>
    <after>χ(X₁) = 0.85 × 0.4 × 0.8 = 0.272 → Rope</after>
  </chi_recalculation>
  
  <chi_recalculation>
    <constraint>C₅</constraint>
    <before>χ(X₁) = 0.20 × 1.5 × 0.9 = 0.27 → Mountain</before>
    <after>χ(X₁) = 0.20 × 0.4 × 0.9 = 0.072 → Rope</after>
  </chi_recalculation>
  
  <chi_recalculation>
    <constraint>C₆</constraint>
    <before>χ(X₁) = 0.90 × 1.5 × 0.8 = 1.08 → Snare</before>
    <after>χ(X₁) = 0.90 × 0.4 × 0.8 = 0.288 → Rope</after>
  </chi_recalculation>
  
  <type_change>
    C₁: Snare → Rope (subsistence extraction becomes manageable)
    C₅: Mountain → Rope (resource depletion becomes addressable)
    C₆: Snare → Rope (collective action barrier dissolves)
  </type_change>
  
  <preconditions>
    - Sufficient agents willing to coordinate
    - Communication channels available
    - Shared recognition of collective benefit
  </preconditions>
  
  <blocked_by>
    C₂ (social enforcement against cooperation)
    C₆ (collective action impossibility — self-blocking)
    High suppression in C₁ (economic necessity prevents coordination time)
  </blocked_by>
  
  <notes>
    This is the high-energy transformation that could prevent tragedy. 
    Requires overcoming C₆ (the barrier to its own solution) and C₂
    (social enforcement). Activation energy extremely high — explains 
    why transformation doesn't occur despite theoretical availability.
  </notes>
</transformation_rule>
```

---

### TR₂: Resource Exhaustion

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>continue_individual_extraction</condition>
    <target>C₁, C₅</target>
    <agent>X₁, X₃</agent>
  </trigger>
  
  <index_change>
    <from>E = trapped (C₁ extraction sustainable at low level)</from>
    <to>E = trapped (C₁ extraction unsustainable, C₅ depletion accelerates)</to>
  </index_change>
  
  <chi_recalculation>
    <constraint>C₅</constraint>
    <before>χ(X₁) = 0.20 × 1.5 × 0.9 = 0.27 → Mountain</before>
    <after>χ(X₁) = 0.35 × 1.5 × 0.9 = 0.4725 → Mountain (ε increases as depletion worsens)</after>
  </chi_recalculation>
  
  <type_change>
    C₅: Mountain → Mountain (classification stable, but ε increases)
    C₁: Snare → Snare (extraction intensifies as resources decline)
  </type_change>
  
  <preconditions>
    - No collective organization (TR₁ not triggered)
    - Continued individual extraction pressure
    - Time passage (biographical scale)
  </preconditions>
  
  <blocked_by>
    None — this is the default trajectory absent intervention
  </blocked_by>
  
  <notes>
    Natural degradation path. Without collective action (TR₁), individual 
    extraction continues, resource depletion accelerates, extraction becomes 
    harder (ε increases in C₅), but agents remain trapped. Deterministic 
    Tragedy attractor.
  </notes>
</transformation_rule>
```

---

### TR₃: Mentorship Dissolution

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>external_pressure_breaks_bond</condition>
    <target>C₃</target>
    <agent>X₁, X₂</agent>
  </trigger>
  
  <index_change>
    <from>E = identity_locked (deep attachment)</from>
    <to>E = constrained (attachment broken by external force)</to>
  </index_change>
  
  <chi_recalculation>
    <constraint>C₃</constraint>
    <before>χ(X₁) = 0.25 × 1.0 × 0.8 = 0.20 → Rope</before>
    <after>χ(X₁) = 0.25 × 1.0 × 0.8 = 0.20 → Rope (χ unchanged, but coordination value lost)</after>
  </chi_recalculation>
  
  <type_change>
    C₃: Rope → Piton (coordination function lost, structure persists as memory)
  </type_change>
  
  <preconditions>
    - External constraint forces separation (C₂ social pressure on X₂)
    - Economic necessity (C₁ extraction demands on X₁)
  </preconditions>
  
  <blocked_by>
    None — external constraints override internal bond
  </blocked_by>
  
  <notes>
    Rope degradation to Piton. Mentorship bond (C₃) provides genuine coordination 
    but cannot withstand pressure from C₁ (subsistence extraction) and C₂ 
    (social enforcement). Function dries up, emotional residue remains. 
    Classic T4 transition (Rope → Piton via external pressure).
  </notes>
</transformation_rule>
```

---

### TR₄: Social Enforcement Intensification

```xml
<transformation_rule id="TR₄">
  <trigger>
    <condition>deviate_from_norm</condition>
    <target>C₂</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>Supp(C₂) = 0.80</from>
    <to>Supp(C₂) = 0.95 (intensified enforcement)</to>
  </index_change>
  
  <chi_recalculation>
    <constraint>C₂</constraint>
    <before>χ(X₁) = 0.55 × 1.5 × 0.8 = 0.66 → Tangled_Rope</before>
    <after>χ(X₁) = 0.65 × 1.5 × 0.8 = 0.78 → Snare (ε increases with suppression)</after>
  </chi_recalculation>
  
  <type_change>
    C₂: Tangled_Rope → Snare (coordination loss, extraction dominates)
  </type_change>
  
  <preconditions>
    - Agent attempts deviation (e.g., extended solo expedition)
    - Community perceives threat to norms
  </preconditions>
  
  <blocked_by>
    None — social enforcement is self-reinforcing
  </blocked_by>
  
  <notes>
    Classic T2 transition (Tangled Rope → Snare). Deviation triggers enforcement 
    intensification. Coordination value (reputation signaling) lost as system 
    shifts to pure punishment. Suppression increase drives ε increase (more 
    energy spent on enforcement = more extraction). X₁ crosses Snare threshold.
  </notes>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: Type I — False Mountain (Resource Deple