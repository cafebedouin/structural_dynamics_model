# STAGE 1: FORMAL CONSTRAINT NETWORK SPECIFICATION

## Variable Mapping (Header — Traceability Only)

```
X₁ ← speaker (analytical observer)
X₂ ← Madame Sosostris (fortune teller)
X₃ ← Stetson (war veteran)
X₄ ← neurotic lady (upper-class woman)
X₅ ← traveler (wanderer through wasteland)
X₆ ← the Unreal City (institutional entity)
X₇ ← typist (working-class woman)
X₈ ← clerk (young man carbuncular)
X₉ ← Tiresias (mythological observer)

C₁ ← Fragmented Cultural Inheritance
C₂ ← Systemic Vitality Drain
C₃ ← Transactional Intimacy
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Fragmented Cultural Inheritance

```xml
<constraint id="C₁">
  <properties>
    <epsilon>0.60</epsilon>
    <suppression>0.00</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>analytical</power>        <!-- π = 1.15 -->
      <time>civilizational</time>
      <exit>analytical</exit>
      <scope>global</scope>             <!-- σ = 1.2 -->
    </index>
    <chi>ε × π × σ = 0.60 × 1.15 × 1.2 = 0.828</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.828 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>
      Analytical position detects extraction that others normalize. The inability to 
      synthesize coherent meaning from fragmented cultural data creates high cognitive 
      burden. Global scope amplifies extraction (σ = 1.2) as verification becomes 
      impossible across civilizational scale. Zero suppression indicates no enforcement 
      needed — fragmentation is self-sustaining. Analytical modifier (π = 1.15) breaks 
      degeneracy with moderate power position, revealing Snare where others see baseline 
      confusion.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>institutional</power>      <!-- π = -0.2 -->
      <time>biographical</time>
      <exit>arbitrage</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.60 × (-0.2) × 0.9 = -0.108</chi>
    <type>Rope</type>
    <threshold_check>χ = -0.108 ≤ 0 → net beneficiary, ε ceiling bypassed → PASS</threshold_check>
    <rationale>
      Institutional position extracts FROM the constraint rather than being extracted by 
      it. Cultural fragmentation creates demand for interpretive services. Arbitrage exit 
      option allows playing multiple confused clients against each other. Negative χ 
      indicates net benefit — the more fragmented the cultural landscape, the more valuable 
      the interpretive monopoly. Regional scope (σ = 0.9) slightly dampens extraction but 
      χ remains negative.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.60 × 1.5 × 0.8 = 0.72</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.72 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>
      Powerless position amplifies extraction (π = 1.5). Trapped exit means no alternative 
      meaning-making systems available. Local scope (σ = 0.8) provides slight dampening 
      through easier verification, but powerless amplification dominates. The constraint 
      extracts cognitive resources attempting to derive coherent worldview from incompatible 
      fragments. Zero suppression — no force needed when alternatives are unavailable.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=0.828 → Snare, X₂ χ=-0.108 → Rope, X₃ χ=0.72 → Snare
    
    High indexical variance. The constraint functions as Snare for those attempting to 
    derive meaning (X₁, X₃) but as profitable Rope for those exploiting the confusion (X₂). 
    The same structural property — cultural fragmentation — extracts from meaning-seekers 
    while benefiting meaning-brokers. This is not perspective relativism but structural 
    position determining which face of the constraint is experienced.
  </indexical_variance>

  <boltzmann_test>
    FAIL — Classification varies by Power position:
    - At institutional power (π = -0.2): Rope (net beneficiary)
    - At powerless (π = 1.5): Snare (high extraction)
    - At analytical (π = 1.15): Snare (degeneracy-breaking detection)
    
    Burden distribution is non-factorizable across Power × Scope dimensions. This indicates 
    constructed constraint, not natural law. The fragmentation is maintained by asymmetric 
    benefit structure — those who profit from confusion have no incentive to resolve it.
    
    Conclusion: C₁ is NOT a Mountain despite zero suppression. It is a constructed system 
    with differential impact by structural position.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₂: Systemic Vitality Drain

```xml
<constraint id="C₂">
  <properties>
    <epsilon>0.80</epsilon>
    <suppression>0.10</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₄">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.0 × 0.8 = 0.64</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ = 0.64 ≤ 0.70 (tangled_chi_floor to snare_chi_threshold) → PASS
      Coord = false BUT asymmetry in experience (identity_locked creates perceived 
      coordination need) → Hybrid classification justified
    </threshold_check>
    <rationale>
      Moderate power provides baseline agency but identity_locked exit prevents escape. 
      The environment extracts vitality while appearing to offer social coordination 
      (hence hybrid classification). Local scope (σ = 0.8) provides verification ease 
      but doesn't overcome base extraction. χ = 0.64 falls in Tangled Rope range — 
      genuine extraction present but not overwhelming, mixed with perceived coordination 
      value that keeps agent engaged.
    </rationale>
  </agent>

  <agent ref="X₅">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.5 × 0.9 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.08 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>
      Powerless position amplifies already-high base extraction (ε = 0.80). Constrained 
      exit means escape possible but costly. Regional scope (σ = 0.9) slightly amplifies 
      through verification difficulty. The barren environment actively extracts resources 
      (physical, emotional, cognitive) with no coordination return. Minimal suppression 
      (0.10) indicates the system is self-perpetuating — victims lack resources to organize 
      resistance.
    </rationale>
  </agent>

  <agent ref="X₆">
    <index>
      <power>institutional</power>      <!-- π = -0.2 -->
      <time>generational</time>
      <exit>analytical</exit>
      <scope>national</scope>           <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.80 × (-0.2) × 1.0 = -0.16</chi>
    <type>Rope</type>
    <threshold_check>χ = -0.16 ≤ 0 → net beneficiary, ε ceiling bypassed → PASS</threshold_check>
    <rationale>
      Institutional position extracts FROM the vitality drain rather than being drained. 
      The barren environment creates dependency on institutional mediation. Generational 
      time horizon allows long-term extraction strategies. Analytical exit means the 
      institution can observe without being subject to the constraint. National scope 
      (σ = 1.0) provides baseline. Negative χ indicates the system is self-perpetuating 
      because those with power to change it benefit from its continuation.
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₄ χ=0.64 → Tangled Rope, X₅ χ=1.08 → Snare, X₆ χ=-0.16 → Rope
    
    High indexical variance. The barren environment is an active Snare for the powerless 
    (X₅), a Tangled Rope for those with moderate power but identity-locked (X₄), and a 
    self-perpetuating system (Rope) from institutional viewpoint (X₆). The same structural 
    condition — systemic inability to generate life, meaning, or connection — has radically 
    different constraint properties depending on structural position.
  </indexical_variance>

  <boltzmann_test>
    FAIL — Classification varies by Power position:
    - At institutional power (π = -0.2): Rope (net beneficiary)
    - At moderate power (π = 1.0): Tangled Rope (mixed extraction/coordination)
    - At powerless (π = 1.5): Snare (high extraction)
    
    Burden distribution is non-factorizable. The vitality drain is maintained by those who 
    benefit from it (institutional position) while extracting from those without power to 
    escape. This is constructed scarcity, not natural scarcity.
    
    Conclusion: C₂ is NOT a Mountain. It is a constructed system with asymmetric benefit 
    structure that appears natural to those within it.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₃: Transactional Intimacy

```xml
<constraint id="C₃">
  <properties>
    <epsilon>0.90</epsilon>
    <suppression>0.30</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₇">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.5 × 0.8 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.08 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>
      Powerless position amplifies extreme base extraction (ε = 0.90). Trapped exit means 
      no alternative relationship structures available. Local scope (σ = 0.8) provides 
      slight dampening but powerless amplification dominates. High suppression (0.30) 
      indicates force required to maintain — economic coercion, social pressure. Despite 
      coordination=true (the transaction does coordinate two parties), asymmetry=true 
      reveals extraction overwhelms any coordination value. χ = 1.08 is well above Snare 
      threshold.
    </rationale>
  </agent>

  <agent ref="X₈">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>mobile</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.0 × 0.8 = 0.72</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.72 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>
      Moderate power provides baseline agency. Mobile exit means alternatives exist but 
      are not being pursued. Local scope (σ = 0.8) dampens slightly. Despite lower χ than 
      X₇, still exceeds Snare threshold. The mechanical, loveless structure extracts 
      emotional/relational resources even from the party with more structural power. 
      Suppression (0.30) indicates social enforcement maintaining the transactional frame.
    </rationale>
  </agent>

  <agent ref="X₉">
    <index>
      <power>analytical</power>         <!-- π = 1.15 -->
      <time>civilizational</time>
      <exit>analytical</exit>
      <scope>universal</scope>          <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.15 × 1.0 = 1.035</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.035 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>
      Analytical position with civilizational time horizon observes the pattern across 
      all instances. Universal scope (σ = 1.0) indicates the structure is invariant across 
      contexts. Analytical modifier (π = 1.15) reveals the extraction even from observer 
      position — the cognitive burden of witnessing the pattern repeatedly. Despite 
      analytical exit (not subject to the constraint directly), χ exceeds Snare threshold 
      due to extreme base extraction (ε = 0.90).
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₇ χ=1.08 → Snare, X₈ χ=0.72 → Snare, X₉ χ=1.035 → Snare
    
    Low indexical variance. From every documented index (victim, perpetrator, observer), 
    the structure of mechanical, loveless encounters is a highly extractive Snare. The 
    coordination=true property (transaction does coordinate two parties) is overwhelmed 
    by asymmetry=true and extreme ε = 0.90. This is the most universally extractive 
    constraint in the network — even the party with more power (X₈) experiences it as 
    Snare.
  </indexical_variance>

  <boltzmann_test>
    FAIL — But for different reason than C₁ and C₂:
    
    Classification does NOT vary by Power position (all three agents classify as Snare), 
    BUT the constraint fails Boltzmann on asymmetry grounds. The burden distribution is 
    factorizable (everyone experiences Snare), but the asymmetry=true property indicates 
    constructed differential impact within the transaction itself.
    
    Additionally, suppression = 0.30 indicates enforcement mechanism — natural laws don't 
    require social pressure to maintain.
    
    Conclusion: C₃ is NOT a Mountain. It is a constructed system with universal extraction 
    but asymmetric distribution within each instance. The fact that it extracts from all 
    positions doesn't make it natural — it makes it a particularly effective Snare.
  </boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Collective Organization Attempt

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>organize_collective_meaning_making</condition>
    <target>C₁</target>
    <agent>X₃</agent>
  </trigger>
  
  <index_change>
    <from>P = powerless (π = 1.5), E = trapped</from>
    <to>P = organized (π = 0.4), E = constrained</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.60 × 1.5 × 0.8 = 0.72 → Snare</before>
    <after>χ = 0.60 × 0.4 × 0.8 = 0.192 → Rope</after>
  </chi_recalculation>
  
  <type_change>Snare → Rope</type_change>
  
  <preconditions>
    <condition>Sufficient agents at powerless index to form collective</condition>
    <condition>Communication channels available</condition>
    <condition>Shared recognition of fragmentation as constructed (not natural)</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₂ (vitality drain prevents organization energy)</constraint>
    <constraint>C₁ itself (fragmentation prevents shared framework for organizing)</constraint>
    <error>Type III (if agents normalize fragmentation as natural confusion)</error>
  </blocked_by>
  
  <rationale>
    Collective organization changes power position from powerless (π = 1.5) to organized 
    (π = 0.4), dramatically reducing χ. Exit changes from trapped to constrained as 
    collective provides alternative meaning-making system. The constraint transforms from 
    Snare to Rope because collective burden-sharing reduces per-agent extraction below 
    rope_chi_ceiling (0.35). However, C₁'s fragmentation property makes this transformation 
    extremely difficult — how do you organize around a shared framework when the constraint 
    IS the absence of shared framework?
  </rationale>
</transformation_rule>
```

---

### TR₂: Institutional Mediation Establishment

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>establish_interpretive_monopoly</condition>
    <target>C₁</target>
    <agent>X₂</agent>
  </trigger>
  
  <index_change>
    <from>P = institutional (π = -0.2), E = arbitrage, S = regional (σ = 0.9)</from>
    <to>P = institutional (π = -0.2), E = arbitrage, S = national (σ = 1.0)</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.60 × (-0.2) × 0.9 = -0.108 → Rope</before>
    <after>χ = 0.60 × (-0.2) × 1.0 = -0.12 → Rope</after>
  </chi_recalculation>
  
  <type_change>Rope → Rope (increased extraction FROM constraint)</type_change>
  
  <preconditions>
    <condition>C₁ fragmentation maintained (source of demand)</condition>
    <condition>Powerless agents remain at trapped exit (captive market)</condition>
  </preconditions>
  
  <blocked_by>
    <transformation>TR₁ (collective organization eliminates demand for mediation)</transformation>
  </blocked_by>
  
  <rationale>
    Expanding scope from regional to national increases σ from 0.9 to 1.0, making χ more 
    negative (increased benefit). The institutional position profits more as fragmentation 
    spreads. This transformation is self-reinforcing — success at national scale increases 
    resources to prevent TR₁ (collective organization). The constraint remains Rope for X₂ 
    but becomes more profitable.
  </rationale>
</transformation_rule>
```

---

### TR₃: Vitality Exhaustion

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>resource_depletion_threshold_reached</condition>
    <target>C₂</target>
    <agent>X₅</agent>
  </trigger>
  
  <index_change>
    <from>P = powerless (π = 1.5), E = constrained</from>
    <to>P = powerless (π = 1.5), E = trapped (degradation)</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.80 × 1.5 × 0.9 = 1.08 → Snare</before>
    <after>χ = 0.80 × 1.5 × 0.9 = 1.08 → Snare (unchanged but agent capacity reduced)</after>
  </chi_recalculation>
  
  <type_change>Snare → Snare (degraded capacity, not type change)</type_change>
  
  <preconditions>
    <condition>Prolonged exposure to C₂ without relief</condition>
    <condition>No access to vitality-restoring resources</condition>
  </preconditions>
  
  <blocked_by>
    <transformation>TR₄ (alternative system establishment)</transformation>
  </blocked_by>
  
  <rationale>
    This is NOT a type transformation but a capacity degradation. Exit changes from 
    constrained to trapped as exhaustion eliminates even costly escape options. χ remains 
    unchanged because the formula measures structural extraction, not agent capacity. 
    However, the agent's ability to respond to the extraction is reduced. This represents 
    Snare running to completion — the constraint extracts until the agent can no longer 
    provide resources.
  </rationale>
</transformation_rule>
```

---

### TR₄: Alternative System Establishment

```xml
<transformation_rule id="TR₄">
  <trigger>
    <condition>establish_alternative_vitality_source</condition>
    <target>C₂</target>
    <agent>X₄</agent>
  </trigger>
  
  <index_change>
    <from>P = moderate (π = 1.0), E = identity_locked</from>
    <to>P = moderate (π = 1.0), E = mobile</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.80 × 1.0 × 0.8 = 0.64 → Tangled Rope</before>
    <after>χ = 0.80 × 1.0 × 0.8 = 0.64 → Tangled Rope (but exit available)</after>
  </chi_recalculation>
  
  <type_change>Tangled Rope → Tangled Rope (exit change, not type change)</type_change>
  
  <preconditions>
    <condition>Sufficient resources to construct alternative</condition>
    <condition>Recognition that current system is extractive (not natural scarcity)</condition>
    <condition>C₂ has not progressed to TR₃ exhaustion state</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₁ (fragmentation prevents coherent alternative vision)</constraint>
    <constraint>C₆ (institutional position blocks alternatives to maintain extraction)</constraint>
    <error>Type I (if agent treats C₂ as Mountain — unchangeable natural scarcity)</error>
  </blocked_by>
  
  <rationale>
    Exit changes from identity_locked to mobile as alternative system provides escape 
    option. χ remains unchanged because the constraint itself hasn't changed — but now 
    the agent can leave. This is the critical transformation that prevents TR₃ exhaustion. 
    However, it requires moderate power (X₅ at powerless cannot execute this transformation) 
    and recognition that the barren environment is constructed, not natural.
  </rationale>
</transformation_rule>
```

---

### TR₅: Transactional Structure Collapse

```xml
<transformation_rule id="TR₅">
  <trigger>
    <condition>refuse_transactional_frame</condition>
    <target>C₃</target>
    <agent>X₇</agent>
  </trigger>
  
  <index_change>
    <from>P = powerless (π = 1.5), E = trapped</from>
    <to>P = powerless (π = 1.5), E = constrained</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.90 × 1.5 × 0.8 = 1.08 → Snare</before>
    <after>χ = 0.90 × 1.5 × 0.8 = 1.08 → Snare (but participation refused)</after>
  </chi_recalculation>
  
  <type_change>Snare → Snare (refusal, not transformation)</type_change>
  
  <preconditions>
    <condition>Recognition of transactional structure as extractive</condition>
    <condition>Willingness to bear cost of refusal</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₂ (vitality drain eliminates resources needed to refuse)</constraint>
    <error>Type III (if agent normalizes transaction as legitimate coordination)</error>
  </blocked_by>
  
  <rationale>
    This is refusal, not transformation. Exit changes from trapped to constrained as 
    refusal becomes possible (at high cost). χ remains unchanged because the constraint 
    structure persists — the agent simply stops participating. If enough agents at 
    powerless index refuse, suppression would need to increase (currently 0.30) or the 
    constraint would collapse. However, C₂ vitality drain typically prevents accumulation 
    of refusal energy.
  </rationale>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: Type I (False Mountain) — C₂ Naturalization

```xml
<error id="E₁">
  <type>Type I: False Mountain</type>
  <agent>X₅</agent>
  <constraint>C₂</constraint>
  <actual_type>Snare (χ = 1.08 from powerless index)</actual_type>
  <perceived_type>Mountain (unchangeable natural scarcity)</perceived_type>
  <observable>
    Does not attempt to establish alternative vitality sources despite moderate-power 
    agents (X₄) demonstrating feasibility. Treats barren environment as natural law 
    rather than constructed system. Expends energy adapting to scarcity rather than 
    challenging its construction.
  </observable>
  <correction_trigger>
    Exposure to successful TR₄ execution by similar-index agent, OR
    Recognition that institutional position (X₆) benefits from maintaining scarcity 
    (χ = -0.16 indicates net extraction FROM the constraint)
  </correction_trigger>
  <structural_consequence>
    Energy depletion fighting what appears unchangeable. Prevents TR₄ (alternative 
    system establishment) and accelerates TR₃ (exhaustion). The error is self-reinforcing — 
    exhaustion further reduces capacity to recognize constructed nature of constraint.
  </structural_consequence>
</error>
```

---

### Error E₂: Type III (Snare-as-Rope) — C₃ Normalization

```xml
<error id="E₂">
  <type>Type III: Snare-as-Rope (Missing Extraction)</type>
  <agent>X₈</agent>
  <constraint>C₃</constraint>
  <actual_type>Snare (χ = 0.72 from moderate-power index)</actual_type>
  <perceived_type>Rope (legitimate coordination mechanism)</perceived_type>
  <observable>
    Treats mechanical, loveless transaction as normal relationship structure. Does not 
    recognize asymmetric extraction (asymmetry = true, ε = 0.90). Mobile exit option 
    (E = mobile) not utilized despite alternatives existing. Normalizes suppression 
    (0.30) as natural social structure rather than enforcement mechanism.
  </observable>
  <correction_trigger>
    Comparison with non-transactional relationship structure, OR
    Recognition that powerless-index agent (X₇) experiences same structure as extreme 
    Snare (χ = 1.08), OR
    Analytical observation (X₉) revealing pattern across civilizational time scale
  </correction_trigger>
  <structural_consequence>
    Perpetuates extractive structure by treating it as coordination. Prevents TR₅
    (refusal) and maintains suppression level. The error is particularly dangerous 
    because moderate power position (π = 1.0) provides just enough agency to exit but 
    normalization prevents recognition of need to exit.
  </structural_consequence>
</error>
```

---

### Error E₃: Type III (Snare-as-Rope) — C₁ Exploitation Blindness

```xml
<error id="E₃">
  <type>Type III: Snare-as-Rope (Missing Extraction)</type>
  <agent>X₃</agent>
  <constraint>C₁</constraint>
  <actual_type>Snare (χ = 0.72 from powerless index)</actual_type>
  <perceived_type>Rope (natural confusion requiring expert guidance)</perceived_type>
  <observable>
    Seeks interpretive services from institutional position (X₂) rather than attempting 
    collective meaning-making (TR₁). Treats cultural fragmentation as natural state 
    requiring mediation rather than constructed system benefiting mediators. Does not 
    recognize that X₂ has negative χ (-0.108) — profits FROM the fragmentation.
  </observable>
  <correction_trigger>
    Recognition that fragmentation is maintained by those who profit from it (X₂ 
    institutional position), OR
    Exposure to collective meaning-making attempt (TR₁) demonstrating feasibility, OR
    Analytical observation (X₁) revealing constructed nature of fragmentation
  </correction_trigger>
  <structural_consequence>
    Perpetuates demand for institutional mediation, blocking TR₁ (collective organization) 
    and enabling TR₂ (mediation expansion). The error converts potential collective action 
    into individual consumption of interpretive services. Self-reinforcing because paying 
    for mediation depletes resources needed for collective organization.
  </structural_consequence>
</error>
```

---

### Error E₄: Type II (Mountain Denial) — C₁ Revolutionary Overreach

```xml
<error id="E₄">
  <type>Type II: Mountain Denial</type>
  <agent>X₁</agent>
  <constraint>C₁</constraint>
  <actual_type>Snare (χ = 0.828 from analytical index) — but constructed, not natural</actual_type>
  <perceived_type>Immediately transformable through individual insight</perceived_type>
  <observable>
    Attempts to resolve civilizational-scale fragmentation through individual analytical 
    work. Treats cultural inheritance as immediately synthesizable despite global scope 
    (σ = 1.2)